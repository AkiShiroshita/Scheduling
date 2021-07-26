using System;
using System.IO;
using System.Text;
using System.Linq;
using System.Windows;
using System.ComponentModel;
using System.Threading.Tasks;
using System.Windows.Controls;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Runtime.CompilerServices;
using System.Text.RegularExpressions;
using Microsoft.VisualBasic.FileIO;

namespace 内科
{
	public enum ErType { None, D2 /* 2次 */, H1 /* 1次(祝) */, H2  /* 2次(日祝) */ }

	public class ErTypeComboBoxItem
	{
		public string Label { get; set; }
		public ErType Value { get; set; }
	}
	//------------------------------------------------------------------------------------
	
	public class ShiftItem : INotifyPropertyChanged
	{
		public event PropertyChangedEventHandler PropertyChanged;

        private void NotifyPropertyChanged([CallerMemberName] String propertyName = "")
        {
			PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(propertyName));
        }

		public static readonly char[] Suffixes = { '*', '〇', '×' };

		private ErType _Type;		// 救急区分
		private string _DutyN1;		// 当直1
		private string _DutyN2;		// 当直2
		private string _DutyD1;		// 日直1
		private string _DutyD2;		// 日直2
		
		public int  Day       { get; set; }
		public char DayOfWeek { get; set; }

		public ErType Type
		{
			get { return _Type; }
			set {
				if (_Type != value)
				{
					_Type = DayOfWeek != '日'  ? value       :
						    value == ErType.D2 ? ErType.H2   :
						    value == ErType.H1 ? ErType.None : value;
					NotifyPropertyChanged();
				}
			}
		}
		public string DutyN1
		{
			get { return _DutyN1; }
			set {
				if (_DutyN1!=value && IsValid(value))
				{
					_DutyN1 = Validate(value);
					NotifyPropertyChanged();
				}
			}
		}
		public string DutyN2
		{
			get { return _DutyN2; }
			set {
				if (_DutyN2!=value && Is2ndER && IsValid(value))
				{
					_DutyN2 = Validate(value);
					NotifyPropertyChanged();
				}
			}
		}
		public string DutyD1
		{
			get { return _DutyD1; }
			set {
				if (_DutyD1!=value && IsHoliday && IsValid(value))
				{
					_DutyD1 = Validate(value);
					NotifyPropertyChanged();
				}
			}
		}
		public string DutyD2
		{
			get { return _DutyD2; }
			set {
				if (_DutyD2!=value && IsHoliday && Is2ndER && IsValid(value))
				{
					_DutyD2 = Validate(value);
					NotifyPropertyChanged();
				}
			}
		}

		public Doctor DoctorN1 => Doctor.Find( DutyN1?.TrimEnd(Suffixes) );
		public Doctor DoctorN2 => Doctor.Find( DutyN2?.TrimEnd(Suffixes) );
		public Doctor DoctorD1 => Doctor.Find( DutyD1?.TrimEnd(Suffixes) );
		public Doctor DoctorD2 => Doctor.Find( DutyD2?.TrimEnd(Suffixes) );

		public bool IsHoliday => Type==ErType.H1 || Type==ErType.H2 || DayOfWeek=='日';
		public bool Is2ndER   => Type==ErType.D2 || Type==ErType.H2;

		private string Validate(string value)
		{
			if (string.IsNullOrWhiteSpace(value)) { return null; }
			
			string str = value.TrimEnd(Suffixes);

			for (int i = 0; i < Doctor.Count; i++)
			{
				string name = Doctor.At(i).Name;
				int n;
				if (int.TryParse(str, out n) && n==i+1) { return name + "*"; }	// 手動割り当て
				if (str == name)                        { return value; }		// 「名前*」の手動割り当ての場合もある
			}
			return string.Empty;	// invalid
		}
		private bool IsValid(string value) => Validate(value) != string.Empty;

		public IEnumerable<ShiftType> Shift()
		{
			List<ShiftType> shift;

			if (IsHoliday)
			{
				shift = Is2ndER
					  ? new List<ShiftType> { ShiftType.Day, ShiftType.Day, ShiftType.Night, ShiftType.Night }
					  : new List<ShiftType> { ShiftType.Day, ShiftType.Night };
			}
			else
			{
				shift = Is2ndER
					  ? new List<ShiftType> { ShiftType.Night, ShiftType.Night }
					  : new List<ShiftType> { ShiftType.Night };
			}
			return shift;
		}

		public int Clear(bool clear_all)
		{
			int n = 0;

			if (clear_all)
			{
				if (DutyD1 != null) { DutyD1 = null; n++; }
				if (DutyD2 != null) { DutyD2 = null; n++; }
				if (DutyN1 != null) { DutyN1 = null; n++; }
				if (DutyN2 != null) { DutyN2 = null; n++; }
			}
			else  // 手動割り当て(*付き)はクリアしない
			{
				if (DutyD1!=null && !DutyD1.EndsWith("*")) { DutyD1 = null; n++; }
				if (DutyD2!=null && !DutyD2.EndsWith("*")) { DutyD2 = null; n++; }
				if (DutyN1!=null && !DutyN1.EndsWith("*")) { DutyN1 = null; n++; }
				if (DutyN2!=null && !DutyN2.EndsWith("*")) { DutyN2 = null; n++; }
			}
			return n;
		}
	}
	//------------------------------------------------------------------------------------

	public partial class MainWindow : Window
	{
        ObservableCollection<ShiftItem> ShiftItems;
		ShiftCalendar                   Calendar;
		DoctorCalendar[]                DrCalendars;
		ListWindow                      ListWindow;
		
		public MainWindow()
		{
			InitializeComponent();

			ShiftItems  = new ObservableCollection<ShiftItem>();
			DrCalendars = new DoctorCalendar[Doctor.Count];

			ShiftTable.ItemsSource   = ShiftItems;
			ShiftTable.SelectionMode = DataGridSelectionMode.Single;
			ShiftTable.SelectionUnit = DataGridSelectionUnit.Cell;
			
			DateTime time = DateTime.Now.AddMonths(1);
			MonthBox.Text = $"{time.Year}/{time.Month}";

			ChangeMonth(time);
		}

		private void Window_Closing(object sender, CancelEventArgs e)
		{
			ListWindow.Close();
		}

		private void MonthBox_Enter(object sender, RoutedEventArgs e)
		{
			if (IsCalculating) { return; }

			DateTime time;
			if ( DateTime.TryParse(MonthBox.Text, out time) ) { ChangeMonth(time); }
		}

		private void List_Click(object sender, RoutedEventArgs e)
		{
			BuildCalendars();
			
			ListWindow.Owner = this;
			ListWindow.Show();
		}

		private async void Assign_Click(object sender, RoutedEventArgs e)
		{
			if (IsCalculating) { return; }

			BuildCalendars();

			if ( !ShiftItems.Any(i => i.Is2ndER) )
			{
				MessageBox.Show("救急区分を入力してください。", "エラー");
				return;
			}
			if ( DrCalendars.Sum(d => d.Count) < Calendar.Count(false) )
			{
				MessageBox.Show("当直枠の数が医師の当直回数の合計より大きいです。\n"
					          + "各医師の当直回数や勤務状況を確認してください。", "エラー");
				List_Click(sender, e);
				return;
			}

			/*
			 * recursionはcapacity+3程度までで十分。おそらく最初の数手で勝負が決まっている。
			 * recursionを小さくして、その分trialを大きくとった方が良い解を見つけることができる。
			 * threshold=1の解が一番多く見つかる。
			 * refine_cycleは3程度で十分。残り物の枠は負荷が高い＆不人気なので交代できる人がほとんどいない。
			*/
			int recursion_max = Calendar.Capacity() + 5;	// 再起呼び出し回数の上限
			int trial_max     = 500;						// 再帰呼び出し回数の上限
			int threshold_max = 2;							// 割り当てを優先するシフト枠の残人数の閾値の上限
			int solution_max  = 20;							// 解の個数の上限
			int refine_cycle  = 10;							// 解の洗練工程数

			string[] args = Environment.GetCommandLineArgs();
			if (args.Length > 1)
			{
				Match m1 = Regex.Match(args[1], @"recursion_max\s*=\s*(\d+)");
				Match m2 = Regex.Match(args[1], @"trial_max\s*=\s*(\d+)");
				Match m3 = Regex.Match(args[1], @"threshold_max\s*=\s*(\d+)");
				Match m4 = Regex.Match(args[1], @"solution_max\s*=\s*(\d+)");
				Match m5 = Regex.Match(args[1], @"refine_cycle\s*=\s*(\d+)");

				if (m1.Success) { recursion_max = int.Parse(m1.Groups[1].Value); }
				if (m2.Success) { trial_max     = int.Parse(m2.Groups[1].Value); }
				if (m3.Success) { threshold_max = int.Parse(m3.Groups[1].Value); }
				if (m4.Success) { solution_max  = int.Parse(m4.Groups[1].Value); }
				if (m5.Success) { refine_cycle  = int.Parse(m5.Groups[1].Value); }
			}

			var        matcher = new ShiftMatcher(Calendar, DrCalendars);
			Func<bool> task    = () => matcher.Match(recursion_max, trial_max, threshold_max, solution_max, refine_cycle);

			matcher.ShowMessage += msg => this.Dispatcher.Invoke(() => SetTitle($"計算中... {msg}"));

			// 手動割り当てされたDrをShiftMatcherに割り当て
			foreach (ShiftItem item in ShiftItems)
			{
				matcher.Assign(item.DoctorD1, ShiftType.Day,   item.Day);	// dr==nullなら失敗
				matcher.Assign(item.DoctorD2, ShiftType.Day,   item.Day);
				matcher.Assign(item.DoctorN1, ShiftType.Night, item.Day);
				matcher.Assign(item.DoctorN2, ShiftType.Night, item.Day);
			}
			SetTitle("計算中...");
			
			if ( await Task.Run(task) )
			{
				foreach (ShiftItem item in ShiftItems)
				{
					foreach (ShiftType type in item.Shift())
					{
						foreach (Doctor dr in matcher.Assignment(type, item.Day))
						{
							string duty = DutyString(dr, item.Day, type);

							if (type == ShiftType.Day)
							{
								if      (item.DutyD1==null && item.DoctorD2!=dr) { item.DutyD1 = duty; }
								else if (item.DutyD2==null && item.DoctorD1!=dr) { item.DutyD2 = duty; }
							}
							else
							{
								if      (item.DutyN1==null && item.DoctorN2!=dr) { item.DutyN1 = duty; }
								else if (item.DutyN2==null && item.DoctorN1!=dr) { item.DutyN2 = duty; }
							}

						}
					}
				}
			}			
			else
			{
				MessageBox.Show("割り当てる解が見つかりませんでした。\n"
					          + "医師の当直回数を合計1～3回程度増やして再試行してください。", "エラー");
			}
			SetTitle();
		}

		private void Save_Click(object sender, RoutedEventArgs e)
		{
			if (IsCalculating) { return; }

			string[] lines = { "二次救急", "日", "曜日", "日直1", "当直1", "日直2", "当直2" };
			string   file  = $"当直表({Calendar.Year}年{Calendar.Month:00}月).csv";
			var      enc   = Encoding.GetEncoding("Shift_JIS");

			foreach (ShiftItem item in ShiftItems)
			{
				string er_type;
				if (item.Is2ndER) { er_type = item.IsHoliday ? "2次(日祝)" : "2次"; }
				else              { er_type = item.IsHoliday ? "1次(祝)"   : "";    }

				lines[0] += "," + er_type;
				lines[1] += "," + item.Day;
				lines[2] += "," + item.DayOfWeek;
				lines[3] += "," + item.DutyD1;
				lines[4] += "," + item.DutyN1;
				lines[5] += "," + item.DutyD2;
				lines[6] += "," + item.DutyN2;
			}

			try {
				using (var sr = new StreamWriter(file, false, enc))
				{
					foreach (string line in lines) { sr.WriteLine(line); }
				}
				MessageBox.Show($"{file}にデータを書き込みました。");
			}
			catch (Exception ex)
			{
				MessageBox.Show($"{file}の書き込みに失敗しました。\n"
					          + $"ファイルを開いている場合は閉じてください。\n\n"
							  + $"詳細：{ex.Message}", "エラー", MessageBoxButton.OK, MessageBoxImage.Error);
			}
		}

		private void Clear_Click(object sender, RoutedEventArgs e)
		{
			if (IsCalculating) { return; }

			int n = 0;
			// 手動割り当ての医師以外をクリア
			foreach (ShiftItem item in ShiftItems) { n += item.Clear(false); }

			if (n == 0)
			{
				// 手動割り当ての医師のみの時にクリアを押すと全クリア
				foreach (ShiftItem item in ShiftItems) { item.Clear(true); }
			}
			SetTitle();
		}

		private string DutyString(Doctor dr, int day, ShiftType type)
		{
			if (dr == null) { return null; }

			DoctorCalendar cal = DrCalendars[dr.Index];

			string suffix =  cal.IsUnpreferable(day, type) ? "×"
		                  : (cal.IsPreferable(day, type)   ? "〇" : null);

			return dr.Name + suffix;
		}

		private void SetTitle(string message = null)
		{
			string title = Title.Split(new char[] { ' ' }).First();

			if (message == null)
			{
				int pref = 0, unpref = 0;

				foreach (ShiftItem item in ShiftItems)
				{
					string[] duties = { item.DutyD1, item.DutyD2, item.DutyN1, item.DutyN2 };

					pref   += duties.Count(s => s!=null && s.EndsWith("〇") );
					unpref += duties.Count(s => s!=null && s.EndsWith("×") );
				}
				message = $"〇：{pref}個　×：{unpref}個";
			}
			Title = $"{title} （{message}）";
		}

		private bool IsCalculating => Title.Contains("計算中");

		private bool Load()
		{
			string file  = $"当直表({Calendar.Year}年{Calendar.Month:00}月).csv";
			var    enc   = Encoding.GetEncoding("Shift_JIS");

			if ( !File.Exists(file) ) { return false; }

			try {	
				using (var fs = new FileStream(file, FileMode.Open, FileAccess.Read, FileShare.ReadWrite)) 
				using (var sr = new TextFieldParser(fs, enc))
				{
					sr.TextFieldType = FieldType.Delimited;
					sr.SetDelimiters(",");

					string[] values0 = sr.ReadFields();   sr.ReadLine(); sr.ReadLine(); // skip 2 lines
					string[] values3 = sr.ReadFields();
					string[] values4 = sr.ReadFields();
					string[] values5 = sr.ReadFields();
					string[] values6 = sr.ReadFields();

					for (int i = 0; i < Calendar.Days; i++)
					{
						var item = ShiftItems[i];
						
						ErType type;
						switch (values0[i+1])
						{
						case "2次(日祝)": type = ErType.H2;   break;
						case "2次":       type = ErType.D2;   break;
						case "1次(祝)":   type = ErType.H1;   break;
						default:          type = ErType.None; break;
						}

						item.Type   = type;
						item.DutyD1 = values3[i+1];
						item.DutyN1 = values4[i+1];
						item.DutyD2 = values5[i+1];
						item.DutyN2 = values6[i+1];
					}
					SetTitle();
				}
				return true;
			}
			catch (IOException e)
			{
				MessageBox.Show($"{file}の読み込みに失敗しました。\n"
							  + $"ファイルを開いている場合は閉じてください。\n\n"
							  + $"詳細：{e.Message}", "エラー", MessageBoxButton.OK, MessageBoxImage.Error);
			}
			catch (Exception e)
			{
				MessageBox.Show($"{file}の読み込みに失敗しました。\n"
							  + $"ファイルの内容に不適切な数値や文字列がないかどうか確認してください。\n\n"
							  + $"詳細：{e.Message}", "エラー", MessageBoxButton.OK, MessageBoxImage.Error);
			}
			return false;
		}

		private void ChangeMonth(DateTime date)
		{
			int days    = DateTime.DaysInMonth(date.Year, date.Month);
			var culture = System.Globalization.CultureInfo.GetCultureInfo("ja-JP");

			Calendar = new ShiftCalendar(date.Year, date.Month);

			ShiftItems.Clear();

			for (int i = 1; i <= days; i++)
			{
				var d    = new DateTime(date.Year, date.Month, i);
				var item = new ShiftItem();
				item.Day       = d.Day;
				item.DayOfWeek = d.ToString("ddd", culture).First();
				ShiftItems.Add(item);
			}
			Load();
			
			ListWindow?.Close();
			ListWindow = new ListWindow(Calendar);
		}

		private void BuildCalendars()
		{
			Calendar.Clear();

			foreach (ShiftItem item in ShiftItems)
			{
				Calendar.Assign(item.Day, item.Shift(), item.Is2ndER, item.IsHoliday);
			}
			DrCalendars = ListWindow.BuildDrCalendars();
		}
	}
}
