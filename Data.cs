using System;
using System.IO;
using System.Data;
using System.Linq;
using System.Text;
using System.Windows;
using System.Collections;
using System.Collections.Generic;
using Microsoft.VisualBasic.FileIO;

namespace 内科
{
	public class Doctor
	{
		public readonly string   Name;			// 表示名
		public readonly string   Department;	// 診療科
		public readonly int      Index;			// 医師インデックス(ID)
		public readonly int      Year;			// 卒業年次
		public readonly int      BaseCount;		// 基本当直回数
		public readonly int      MinCount;		// 最小当直回数
		public readonly int      MaxCount;		// 最大当直回数
		public readonly string[] WorkStyle;		// 勤務形態

		private readonly double[] Multipliers;

		private Doctor(string              name,
			           string              department,
					   int                 index,
					   int                 year,
					　 IEnumerable<int>    counts,
					   IEnumerable<string> workstyle,
					   IEnumerable<double> multipliers)
		{
			Name        = name;
			Department  = department;
			Index       = index;
			Year        = year;
			BaseCount   = counts.ElementAt(0);
			MinCount    = counts.ElementAt(1);
			MaxCount    = counts.ElementAt(2);
			WorkStyle   = workstyle.ToArray();
			Multipliers = multipliers.ToArray();
		}

		/*
		 * 以下を考慮してシフト枠の負荷を算出
		 * 1. 1次 or 2次
		 * 2. 平日 or 休日
		 * 3. 翌日が平日 or 休日 (or 外来)
		 * 4. 日直 or 当直
		*/
		public double Weight(bool is_2nd, bool is_dayoff, bool is_preoff, bool is_night)
		{
			int i1 = Convert.ToInt32(is_2nd);
			int i2 = Convert.ToInt32(is_dayoff);
			int i3 = Convert.ToInt32(is_preoff);
			int i4 = Convert.ToInt32(is_night);
			return Math.Sqrt(Multipliers[i1] * Multipliers[2+i2] * Multipliers[4+i3] * Multipliers[6+i4]);
		}

		public double Experience => (DateTime.Now - new DateTime(Year, 4, 1)).TotalDays / 365;
		
		//--------------------------------------------------------------------------------
		private static List<Doctor> Doctors = new List<Doctor>();		// グローバルな医師リスト

		static Doctor()
		{
			string file = "医師リスト.csv";
			var    enc  = Encoding.GetEncoding("Shift_JIS");

			try {	
				using (var fs = new FileStream(file, FileMode.Open, FileAccess.Read, FileShare.ReadWrite)) 
				using (var sr = new TextFieldParser(fs, enc))
				{
					sr.TextFieldType = FieldType.Delimited;
					sr.SetDelimiters(",");

					sr.ReadLine();	// skip header

					for (int i = 0; !sr.EndOfData; i++)
					{
						string[] values      = sr.ReadFields();
						string   name        = values[2];
						string   department  = values[3];
						int      year        = int.Parse(values[4]);
						var      counts      = values.Skip(5).Take(3).Select(v => int.Parse(v));
						var      workstyle   = values.Skip(8).Take(5);
						var      multipliers = values.Skip(13).Select(v => double.Parse(v));

						if ( Doctors.Any(d => d.Name == name) )
						{
							throw new ArgumentException($"同じ名前の医師({name})が{file}に登録されています。");
						}
						else
						{
							Doctors.Add( new Doctor(name, department, i, year, counts, workstyle, multipliers) );
						}
					}
				}
			}
			catch (IOException e)
			{
				MessageBox.Show($"{file}のファイル読み込みに失敗しました。\n"
							  + $"・実行ファイルと同じフォルダに{file}があることを確認してください。\n"
							  + $"・{file}を開いている場合は閉じてください。\n\n"
							  + $"詳細：{e.Message}", "エラー", MessageBoxButton.OK, MessageBoxImage.Error);
				Environment.Exit(0);
			}
			catch (Exception e)
			{
				MessageBox.Show($"{file}を正しく読み取れませんでした。\n"
							  + $"・{file}を開いて、不適切な値になっている箇所がないか確認してください。\n\n"
							  + $"詳細：{e.Message}", "エラー", MessageBoxButton.OK, MessageBoxImage.Error);
				Environment.Exit(0);
			}
		}

		public static int                 Count             => Doctors.Count;
		public static IEnumerable<Doctor> List              => Doctors;
		public static Doctor              At(int index)     => Doctors[index];
		public static Doctor              Find(string name) => Doctors.Find(d => d.Name == name);
	}
	//------------------------------------------------------------------------------------
	
	public enum ShiftType
	{
		None   = 0b0000,
		Day    = 0b1100,	// 日勤/日直, D
		Night  = 0b0011,	// 夜勤/当直, N
		AM     = 0b1000,	// 午前, A
		PM     = 0b0100,	// 午後, P
		Late   = 0b0110,	// 遅番, L
		Half   = 0b0010,	// 半直
		DHalf  = 0b1110,	// 日勤＋半直
		LNight = 0b0111,	// 遅番＋当直
		Full   = 0b1111,	// 日当直, F
	}

	public static class ShiftTypeExt
	{
		public static bool Or(this ShiftType left, ShiftType right)
		{
			int lt = (int) left, rt = (int) right;
			return (lt | rt) > 0;
		}

		public static bool And(this ShiftType left, ShiftType right)
		{
			int lt = (int) left, rt = (int) right;
			return (lt & rt) > 0;
		}

		public static bool Xor(this ShiftType left, ShiftType right)
		{
			int lt = (int) left, rt = (int) right;
			return (lt ^ rt) > 0;
		}
	}
	//------------------------------------------------------------------------------------

	public abstract class Calendar
	{
		private BitArray ErTypeArray;		// [日数] 救急区分(2次or1次)
		private BitArray HolidayArray;		// [日数] 日曜・祝日

		public readonly int Year;			// 年
		public readonly int Month;			// 月
		public readonly int Days;			// 月の日数

		protected Calendar(int year, int month)
		{
			Year         = year;
			Month        = month;
			Days         = DateTime.DaysInMonth(year, month);
			ErTypeArray  = new BitArray(Days + 1);
			HolidayArray = new BitArray(Days + 1);

			HolidayArray[Days] = DayOfWeek(Days+1) == System.DayOfWeek.Sunday;
		}

		protected void Assign(int day, bool is_2nd, bool is_holiday)
		{
			ErTypeArray[day-1]  = is_2nd;
			HolidayArray[day-1] = is_holiday;
		}

		public DateTime Date(int day) => new DateTime(Year, Month, day);

		public DayOfWeek DayOfWeek(int day)
		{
			int dw = (int) Date(1).DayOfWeek;
			return (DayOfWeek) ((dw + day - 1) % 7);	// 来月1日の曜日も分かる
		}

		public char Youbi(int day)
		{
			char[] days = { '日', '月', '火', '水', '木', '金', '土' };
			return days[(int) DayOfWeek(day)];
		}

		public bool Is2ndER(int day)   => ErTypeArray[day-1];		// 2次救急かどうか
		public bool IsHoliday(int day) => HolidayArray[day-1];		// 日or祝かどうか(土曜は含まない)
	}
	//------------------------------------------------------------------------------------

	public class ShiftCalendar : Calendar
	{
		private IEnumerable<ShiftType>[] Shifts;	// [日数] 日ごとのシフト枠のリスト(それぞれの日に人数分)

		public ShiftCalendar(int year, int month) : base(year, month)
		{
			Shifts = new IEnumerable<ShiftType>[Days];
		}

		public void Assign(int day, IEnumerable<ShiftType> shift, bool is_2nd, bool is_holiday)
		{
			Shifts[day-1] = shift ?? Enumerable.Empty<ShiftType>();
			Assign(day, is_2nd, is_holiday);
		}
		
		public IEnumerable<ShiftType> Shift(int day, bool distinct)
		{
			return distinct ? Shifts[day-1].Distinct() : Shifts[day-1];
		}

		public void Clear()
		{
			Array.Clear(Shifts, 0, Shifts.Length);
		}

		public int Capacity(int day, ShiftType type)
		{
			return Shift(day, false).Count(t => t == type);
		}

		public int Capacity()
		{
			int capacity = 0;
			for (int day = 1; day <= Days; day++)
			{
				if ( IsHoliday(day) ) { capacity += Is2ndER(day) ? 4 : 2; }
				else                  { capacity += Is2ndER(day) ? 2 : 1; }
			}
			return capacity;
		}

		public int Count(bool distinct)
		{
			return Shifts.SelectMany(s => distinct ? s.Distinct() : s).Count();
		}
	}
	//------------------------------------------------------------------------------------

	public class DoctorCalendar : Calendar
	{
		class Shift
		{
			public readonly int       Day;
			public readonly ShiftType Type;

			public double Score { get; set; }

			public Shift(int day, ShiftType type)
			{
				Day   = day;
				Type  = type;
				Score = 0;
			}
		}
		//----------------------------------------------
		
		private List<Shift> Shifts;				// [枠数] 平坦化したシフトのリスト
		private ShiftType[] Works;				// 勤務日
		private ShiftType[] DayOffs;			// 公休日
		private ShiftType[] Preferables;		// 希望日
		private ShiftType[] Unpreferables;		// できれば避けたい日
		private ShiftType[] Unavailables;		// 不可日

		public readonly Doctor Doctor;		// この予定表の医師
		public readonly int    Count;		// 医師の当直回数(手動割り当てされた回数は差し引き済み)

		public DoctorCalendar(int dr_index, int count, ShiftCalendar calendar)
			: base(calendar.Year, calendar.Month)
		{
			Shifts = new List<Shift>();
			Doctor = Doctor.At(dr_index);
			Count  = count;

			for (int day = 1; day <= Days; day++)
			{
				var shifts = calendar.Shift(day, true).Select(s => new Shift(day, s));

				Shifts.AddRange(shifts);
				
				Assign( day, calendar.Is2ndER(day), calendar.IsHoliday(day) );
			}
		}

		public double Score(int index) => Shifts[index].Score;

		public IEnumerable<double> Scores() => Shifts.Select(s => s.Score);
		
		public bool IsDayOff(int day) => DayOffs[day-1] != ShiftType.None;
		public bool IsWork  (int day) => !IsDayOff(day);

		public bool IsPreferable  (int day, ShiftType type) => Preferables  [day-1].And(type);
		public bool IsUnpreferable(int day, ShiftType type) => Unpreferables[day-1].And(type);
		public bool IsUnavailable (int day, ShiftType type) => Unavailables [day-1].And(type);

		public void Build(string work, string dayoff, string preferable, string unpreferable, string unavailable)
		{
			Works         = Build(work,         'D');
			DayOffs       = Build(dayoff,       'F');
			Preferables   = Build(preferable,   'F');
			Unpreferables = Build(unpreferable, 'F');
			Unavailables  = Build(unavailable,  'F');

			for (int day = 1; day <= Days+1; day++)
			{
				if ( IsHoliday(day) )
				{
					DayOffs[day-1] = ShiftType.Full;
				}
				else if (DayOfWeek(day)==System.DayOfWeek.Saturday && Works[day-1]!=ShiftType.None)
				{
					DayOffs[day-1] = ShiftType.None;	// 出勤日：月-金,土1,  公休日：土日,月2　などの場合
				}
				if ( IsDayOff(day) )
				{
					Works[day-1] = ShiftType.None;
				}
				if ( Preferables[day-1].And(Unpreferables[day-1]) )
				{
					Preferables[day-1] = ShiftType.None;	// 希望日に避けたい日(具体的な日付)が重なっている場合
				}
			}

			if (Count == 0) { Shifts.ForEach(s => s.Score = 0); return; }	// 当直回数0の場合

			foreach (Shift s in Shifts)
			{
				int       day  = s.Day;
				ShiftType type = s.Type;
				
				if ( IsUnavailable(day, type) )
				{
					s.Score = 0;	// 不可日は0に設定
				}
				else
				{
					bool is_2nd    = Is2ndER(day);
					bool is_dayoff = IsDayOff(day);
					bool is_preoff = IsDayOff(day+1);
					bool is_night  = type == ShiftType.Night;

					s.Score = Doctor.Weight(is_2nd, is_dayoff, is_preoff, is_night);
				}
			}

			// 負荷スコアを0.5～3.0の範囲になるように正規化
			var shifts = Shifts.Where(s => s.Score > 0);
			if (shifts.Count() == 0) { return; }

			double min = shifts.Select(s => s.Score).Min();

			foreach (Shift s in shifts) { s.Score /= min; }

			double max = shifts.Select(s => s.Score).Max();

			foreach (Shift s in shifts)
			{
				int       day  = s.Day;
				ShiftType type = s.Type;
				
				s.Score = 1 + (s.Score - 1) / (max - 1) * 0.5;	// 1.0～1.5の範囲に正規化

				if ( IsPreferable(day, type) )
				{
					s.Score *= 0.5;		// 希望日は1/2倍に設定（希望日の中でも負荷に応じて割り当て）
				}
				else if ( IsUnpreferable(day, type) )
				{
					s.Score *= 2.0;		// 避けたい日は2倍に設定
				}
			}
		}

		private ShiftType[] Build(string str, char default_shift)
		{
			char[]      shifts = { 'D', 'N', 'A', 'P', 'L', 'F' };
			ShiftType[] data   = new ShiftType[Days+1];		// 来月1日まで

			foreach (string token in str.Split(new char[] { ',' }, StringSplitOptions.RemoveEmptyEntries))
			{
				string t = token.TrimEnd(shifts);
				char   s = t != token ? token.Last() : default_shift;

				ShiftType shift = ShiftType.None;
				switch (s)
				{
					case 'D': shift = ShiftType.Day;   break;
					case 'N': shift = ShiftType.Night; break;
					case 'A': shift = ShiftType.AM;    break;
					case 'P': shift = ShiftType.PM;    break;
					case 'L': shift = ShiftType.Late;  break;
					case 'F': shift = ShiftType.Full;  break;
				}

				int m, n;
				if ( int.TryParse(t, out n) )		// 具体的な日付指定（ex. 13）
				{
					if (0<n && n<=Days) { data[n-1] = shift; }
				}
				else if ( t.Contains('-') )			// 具体的な日付指定（ex. 2-5）
				{
					string[] ts = t.Split(new char[] { '-' });
					m = int.Parse(ts[0]);
					n = int.Parse(ts[1]);
					
					for (int k = m; k <= n; k++) { data[k-1] = shift; }
				}
				else if ( t.Length == 2 )			// 第n曜日の指定（ex 土2）
				{
					n = int.Parse( t.Substring(1,1) );

					for (int i = 0, j = 0; i <= Days; i++)
					{
						if (Youbi(i+1)==t[0] && ++j==n) { data[i] = shift; break; }
					}
				}
				else								// 曜日指定（ex 月）
				{
					for (int i = 0; i <= Days; i++)	
					{
						if (Youbi(i+1) == t[0]) { data[i] = shift; }
					}
				}
			}
			return data;
		}

		public override string ToString()
		{
			var seq = Shifts.Select(s => Math.Round(s.Score, 2).ToString());
			return string.Join(",", seq);
		}
	}
}
