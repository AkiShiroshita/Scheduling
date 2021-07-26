using System;
using System.Data;
using System.Linq;
using System.Diagnostics;
using System.Data.SqlClient;
using System.Threading.Tasks;
using System.Collections.Generic;
using System.Collections.Concurrent;

namespace 内科
{
	public class ShiftMatcher
	{
		class Shift : ICloneable
		{
			public readonly ShiftType    Type;
			public readonly int          Day;
			public readonly int          Capacity;		// 枠数
			public readonly double[]     Scores;		// [医師]  このシフト枠に対する各医師の負荷スコア
			public readonly List<Doctor> Doctors;		// [枠数]　割り当てられた医師のインデックス

			public Shift(ShiftType type, int day, int capacity)
			{
				Type     = type;
				Day      = day;
				Capacity = capacity;
				Scores   = new double[Doctor.Count];
				Doctors  = new List<Doctor>(Capacity);
			}

			public bool Filled()       => Doctors.Count == Capacity;
			public int  NumAvailable() => Scores.Count(s => s > 0);

			// 貢献度計算に用いる調整負荷スコア
			public double Score()
			{
				// 引き受けられる医師が少ないコマはスコアが上がる。
				var    scores = Scores.Where(s => s > 0);
				return scores.Average() * scores.Count() / Scores.Length;
			}

			// シフト枠間の日数
			public int Interval(Shift shift, bool absolute = false)
			{
				int interval = Day - shift.Day;
				return absolute ? Math.Abs(interval) : interval;
			}

			public int Assign(Doctor dr)
			{
				Debug.Assert( !Filled() && !Doctors.Contains(dr) && IsAssignable(dr) );
				Deactivate(dr);
				Doctors.Add(dr);
				return Capacity - Doctors.Count;
			}

			public int Unassign(Doctor dr)
			{
				Debug.Assert( Doctors.Contains(dr) );
				Activate(dr);
				Doctors.Remove(dr);
				return Capacity - Doctors.Count;
			}

			// 医師をこのシフトに割り当て可能な状態にする
			public void Activate(Doctor dr)
			{
				Scores[dr.Index] = Math.Abs( Scores[dr.Index] );	// 割り当て可能な医師の負荷スコアは正
			}
			
			// 医師をこのシフトに割り当て不能の状態にする（このシフトに割り当て済み or 当直回数の残0）
			public void Deactivate(Doctor dr)
			{
				Scores[dr.Index] = -Math.Abs( Scores[dr.Index] );	// 割り当て不能の医師の負荷スコアは負
			}
			
			public bool IsAssignable(Doctor dr) => Scores[dr.Index] > 0;

			public object Clone()
			{
				Shift shift = new Shift(Type, Day, Capacity);
				Scores.CopyTo(shift.Scores, 0);
				shift.Doctors.AddRange(Doctors);
				return shift;
			}
		}
		//----------------------------------------------

		class DoctorEntry : ICloneable
		{
			public readonly Doctor  Doctor;								// 医師
			public int              Count        { get; private set; }	// 当直回数(残)
			public double           Contribution { get; set; }			// 貢献度

			public DoctorEntry(Doctor dr, int count)
			{
				Doctor = dr;
				Count  = count;
			}

			public int Index => Doctor.Index;

			public int Assign(Shift shift)
			{
				Debug.Assert(Count > 0);
				return --Count;
			}

			public int Unassign(Shift shift)
			{
				return ++Count;
			}

			public object Clone()
			{
				DoctorEntry de  = new DoctorEntry(Doctor, Count);
				de.Contribution = Contribution;
				return de;
			}
		}
		//----------------------------------------------

		class MatchingContext : ICloneable
		{
			public readonly DoctorCalendar[] DrCalendars;		// [医師数]　各医師の予定表
			public readonly DoctorEntry[]    DrEntries;			// [医師数]　各医師のエントリのリスト
			public readonly List<Shift>      Shifts;			// [コマ数]　今月のシフト枠のリスト
			public readonly List<Shift>      FilledShifts;		// [コマ数]　割り当て済みシフト枠のリスト
			
			public MatchingContext(ShiftCalendar calendar, DoctorCalendar[] dr_calendars)
			{
				DrCalendars  = dr_calendars;
				DrEntries    = new DoctorEntry[Doctor.Count];
				Shifts       = new List<Shift>();
				FilledShifts = new List<Shift>();

				// ShiftCalendarからShiftsを作成
				for (int day = 1; day <= calendar.Days; day++)
				{
					foreach (ShiftType type in calendar.Shift(day, true))
					{
						int capacity = calendar.Capacity(day, type);
						Shifts.Add( new Shift(type, day, capacity) );
					}
				}
				double[][] scores = new double[Doctor.Count][];
				
				// 各シフト枠に対するそれぞれの医師のスコア表を登録
				for (int i = 0; i < Doctor.Count; i++)
				{
					DrEntries[i] = new DoctorEntry(Doctor.At(i), dr_calendars[i].Count);
					scores[i]    = dr_calendars[i].Scores().ToArray();

					for (int j = 0; j < Shifts.Count; j++)
					{
						Shifts[j].Scores[i] = scores[i][j];
					}
				}

				// 各シフト枠の貢献度ポイントを合算して医師の貢献度を計算
				// todo: 先月までの累積負荷を反映して貢献度を調整
				for (int i = 0; i < Doctor.Count; i++)
				{
					DoctorEntry de = DrEntries[i];

					for (int j = 0; j < Shifts.Count; j++)
					{
						if (scores[i][j] > 0)
						{
							de.Contribution += Shifts[j].Score();
						}
					}
					de.Contribution *= de.Count; // Math.Sqrt(de.Count);
				}
				// todo: 先月の後半数日の割り当て情報がないと二日連続当直の医師が出てしまう
			}

			private MatchingContext(MatchingContext src)
			{
				DrCalendars  = src.DrCalendars;
				DrEntries    = src.DrEntries   .Select(e => (DoctorEntry)e.Clone()).ToArray();
				Shifts       = src.Shifts      .Select(s => (Shift)      s.Clone()).ToList();
				FilledShifts = src.FilledShifts.Select(s => (Shift)      s.Clone()).ToList();
			}

			public object Clone()   => new MatchingContext(this);

			public int NumFilled()  => FilledShifts.Count(s => s != null);

			public bool Fulfilled() => Shifts.Count == 0;

			public bool Assign(Shift shift, Doctor dr, bool force = false)
			{
				Debug.Assert(Shift(shift.Type, shift.Day) == shift);	// このContextオブジェクトに所属するShift

				if ( !shift.IsAssignable(dr) ) { return false; }
				if ( !force )
				{
					IEnumerable<Shift> assigned_shifts = AssignedShifts(dr);
					IEnumerable<int>   intervals       = assigned_shifts.Select(s => s.Interval(shift, true));
					Func<Shift, bool>  is_dayoff       = s => DrCalendars[dr.Index].IsDayOff(s.Day);
					Func<Shift, bool>  is_unpref       = s => DrCalendars[dr.Index].IsUnpreferable(s.Day, s.Type);

					// 当直と当直の間は5日間以上あける
					if ( intervals.Any(i => i < 5) ) { return false; }

					// 公休日の当直は1回まで
					if ( is_dayoff(shift) && assigned_shifts.Any(s => is_dayoff(s)) ) { return false; }

					// 避けたい日の当直は1回まで
					if ( is_unpref(shift) && assigned_shifts.Any(s => is_unpref(s)) ) { return false; }
				
					if (shift.Doctors.Count > 0)
					{
						// 同じ科のペアは避ける(対応力・翌日の体制を考慮して)
						if ( !shift.Doctors.Any(buddy => dr.Department != buddy.Department) ) { return false; }

						// ビギナー同士のペアを避ける
						if ( dr.Experience<3 && !shift.Doctors.Any(buddy => buddy.Experience>4) ) { return false; }
					}
				}
				int room  = shift.Assign(dr);					// shiftの残りのunit数
				int count = DrEntries[dr.Index].Assign(shift);	// drの残りの当直回数
				
				if (room == 0)		// シフト枠が充足(Filled)
				{
					Debug.Assert( Shifts.Contains(shift) && !FilledShifts.Contains(shift) );
					Shifts.Remove(shift);
					FilledShifts.Add(shift);
				}
				if (count == 0)		// 医師の当直回数に達した
				{
					foreach (Shift s in Shifts)
					{
						s.Deactivate(dr);
						if (s.NumAvailable() <= 0) { return false; }	// 連動して他の枠の割り当て可能医師がゼロになった
					}
				}
				// 連動して割り当て可能なシフト枠の数が当直回数より少なくなった医師がいなければOK
				return !Doctors().Any(e => AssignableShifts(e.Doctor).Count() < e.Count);
			}

			public bool Unassign(Shift shift, Doctor dr)
			{
				Debug.Assert(Shift(shift.Type, shift.Day) == shift);	// このオブジェクトに所属するShift

				int room  = shift.Unassign(dr);
				int count = DrEntries[dr.Index].Unassign(shift);

				if (room == 1)		// シフト枠が充足した状態から1名分空きが出た
				{
					Debug.Assert( !Shifts.Contains(shift) && FilledShifts.Contains(shift) );
					FilledShifts.Remove(shift);
					Shifts.Add(shift);
				}
				if (count == 1)		// 医師が当直回数に達した状態から残り1回となった
				{
					Shifts.ForEach( s => s.Activate(dr) );
				}
				return true;
			}

			// 指定された医師が割り当てられているシフト枠のリスト
			public IEnumerable<Shift> AssignedShifts(Doctor dr)
			{
				var s1 = Shifts      .Where(s => s.Doctors.Contains(dr));
				var s2 = FilledShifts.Where(s => s.Doctors.Contains(dr));
				return s1.Concat(s2);
			}

			// 指定された医師を割り当て可能なシフト枠のリスト
			public IEnumerable<Shift> AssignableShifts(Doctor dr, SortOrder order = SortOrder.Unspecified)
			{
				var shifts = Shifts.Where( s => s.IsAssignable(dr) );

				// ソートするときには同スコアのシフト枠がいつも同じ並び順にならないようシャッフル
				switch (order)
				{
				case SortOrder.Unspecified: return shifts;
				case SortOrder.Ascending:   return shifts.Shuffle().OrderBy          (s => s.Scores[dr.Index]);
				case SortOrder.Descending:  return shifts.Shuffle().OrderByDescending(s => s.Scores[dr.Index]);
				}
				return null;
			}

			// 指定されたシフトに割り当て可能な医師のリスト
			public IEnumerable<Doctor> AvailableDoctors(Shift shift, SortOrder order = SortOrder.Unspecified)
			{
				var doctors = shift.Scores.Select ((s, i) => s > 0 ? Doctor.At(i) : null)
								   .Where  (dr => dr != null);		// 割り当て可能な医師のインデックスを抽出

				// ソートするときには同じ貢献度の医師がいつも同じ並び順にならないようシャッフル
				switch (order)
				{
				case SortOrder.Unspecified: return doctors;
				case SortOrder.Ascending:   return doctors.Shuffle().OrderBy          (dr => DrEntries[dr.Index].Contribution);
				case SortOrder.Descending:  return doctors.Shuffle().OrderByDescending(dr => DrEntries[dr.Index].Contribution);
				}
				return null;
			}

			// 割り当て可能な医師のリスト
			public IEnumerable<Doctor> AvailableDoctors(SortOrder order = SortOrder.Unspecified)
			{
				var doctors = Doctors();

				// ソートするときには同じ貢献度の医師がいつも同じ並び順にならないようシャッフル
				if (order == SortOrder.Ascending)
				{
					doctors = doctors.Shuffle().OrderBy(e => e.Contribution);
				}
				else if (order == SortOrder.Descending)
				{
					doctors = doctors.Shuffle().OrderByDescending(e => e.Contribution);
				}
				return doctors.Select(e => e.Doctor);
			}

			// 割り当て可能なシフトの自由度(選択の余地)がなくなった医師のリスト
			public IEnumerable<Doctor> InflexibleDoctors()
			{
				return Doctors().Where (e => AssignableShifts(e.Doctor).Count() == e.Count)
					            .Select(e => e.Doctor);
			}

			// 割り当て回数が残っている医師のリスト
			private IEnumerable<DoctorEntry> Doctors() => DrEntries.Where(e => e.Count > 0);

			// 指定されたShiftType・日付のシフトを検索
			public Shift Shift(ShiftType type, int day)
			{
				return Shifts      .Find(s => s.Type==type && s.Day==day)
					?? FilledShifts.Find(s => s.Type==type && s.Day==day);
			}

			// どれくらい希望を満たせたかを示すスコア
			public int Score()
			{
				int score = 0;

				foreach (var (shift, dr) in FilledUnits)
				{
					if      ( IsUnpreferable(shift, dr) ) { score -= 10; }
					else if ( IsPreferable  (shift, dr) ) { score += 1;  }
				}
				return score;
			}

			private bool IsPreferable(Shift shift, Doctor dr)
			{
				return DrCalendars[dr.Index].IsPreferable(shift.Day, shift.Type);
			}
			private bool IsUnpreferable(Shift shift, Doctor dr)
			{
				return DrCalendars[dr.Index].IsUnpreferable(shift.Day, shift.Type);
			}

			// シフト枠の組み換えでより高いスコアを持つ割り付けに洗練
			public MatchingContext Refine(int cycle)
			{
				if (cycle <= 0) { return this; }
				double score = Score();

				// Unpreferableなシフト枠を他の医師と交換
				foreach (var (shift, dr) in FilledUnits)
				{
					if ( IsUnpreferable(shift, dr) )
					{
						MatchingContext ctx = TrySwap(shift, dr);
						if (ctx!=null && ctx.Score()>score) { return ctx.Refine(cycle-1); }

						MatchingContext ctx2 = TrySwap3(shift, dr);
						if (ctx2!=null && ctx2.Score()>score) { return ctx2.Refine(cycle-1); }
					}
				}
				
				// Preferableでないシフト枠を他のPreferableな医師と交換
				foreach (var (shift, dr) in FilledUnits)
				{
					if ( !IsUnpreferable(shift, dr) && !IsPreferable(shift, dr) )
					{
						MatchingContext ctx = TrySwap(shift, dr);
						if (ctx!=null && ctx.Score()>score) { return ctx.Refine(cycle-1); }

						MatchingContext ctx2 = TrySwap3(shift, dr);
						if (ctx2!=null && ctx2.Score()>score) { return ctx2.Refine(cycle-1); }
					}
				}
				return this;	// 洗練の余地なし
			}

			// 指定された医師が割り付けられたShiftを他の医師のシフト枠と交換
			private MatchingContext TrySwap(Shift shift, Doctor dr)
			{
				foreach (var (shift2, dr2) in FilledUnits.Where(sd => sd != (shift, dr)))
				{
					// drのshiftとdr2のshift2を交換
					// 交換される医師にとって負荷スコアが小さくなる場合に交換成立
					double score1 = Math.Abs( shift .Scores[dr2.Index] );
					double score2 = Math.Abs( shift2.Scores[dr2.Index] );

					if (score1 <= score2)
					{
						var   ctx = (MatchingContext) this.Clone();
						Shift s1  = ctx.Shift(shift .Type, shift .Day);
						Shift s2  = ctx.Shift(shift2.Type, shift2.Day);

						if ( ctx.Unassign(s1, dr)  && ctx.Unassign(s2, dr2) &&
						     ctx.Assign  (s1, dr2) && ctx.Assign  (s2, dr) ) { return ctx; }
					}
				}
				return null;
			}

			// 三つ巴の交換
			private MatchingContext TrySwap3(Shift shift, Doctor dr)
			{
				foreach (var (shift2, dr2) in FilledUnits.Where(u => u != (shift, dr)))
				{
					foreach (var (shift3, dr3) in FilledUnits.Where(u => u!=(shift, dr) && u!=(shift2, dr2)))
					{
						double score2_1 = Math.Abs( shift .Scores[dr2.Index] );
						double score2_2 = Math.Abs( shift2.Scores[dr2.Index] );
						double score3_1 = Math.Abs( shift2.Scores[dr3.Index] );
						double score3_2 = Math.Abs( shift3.Scores[dr3.Index] );

						if (score2_1<=score2_2 && score3_1<=score3_2)
						{
							var   ctx = (MatchingContext) this.Clone();
							Shift s1  = ctx.Shift(shift .Type, shift .Day);
							Shift s2  = ctx.Shift(shift2.Type, shift2.Day);
							Shift s3  = ctx.Shift(shift3.Type, shift3.Day);

							if ( ctx.Unassign(s1, dr)  && ctx.Unassign(s2, dr2) && ctx.Unassign(s3, dr3) &&
							     ctx.Assign  (s1, dr2) && ctx.Assign  (s2, dr3) && ctx.Assign  (s3, dr) ) { return ctx; }
						}
					}
				}
				return null;
			}

			private IEnumerable<(Shift, Doctor)> FilledUnits
			{
				get {
					foreach (Shift shift in FilledShifts)
					{
						foreach (Doctor dr in shift.Doctors) { yield return (shift, dr); }
					}
				}
			}
		}
		//----------------------------------------------
		
		class MatchingTask
		{
			private MatchingContext                Context;
			private ConcurrentBag<MatchingContext> Solutions;		// 割り当ての解(スレッド共有)
			private int                            Recursion;		// 再起呼び出し回数の上限
			private int                            RecursionMax;	// 再帰呼び出し回数の上限
			private int                            Threshold;		// 割り当てを優先するシフト枠の残人数の閾値の上限

			public  Callback                       ShowMessage = delegate {};

			public MatchingTask(MatchingContext context, ConcurrentBag<MatchingContext> solutions)
			{
				Context   = context;
				Solutions = solutions;
			}

			public int Match(int recursion_max,		// 再起呼び出し回数の上限
							 int trial_max,			// 再帰呼び出し回数の上限
							 int threshold_max,		// 割り当てを優先するシフト枠の残人数の閾値の上限
							 int solution_max,		// 解の個数の上限
							 int refine_cycle)		// 解の洗練工程数
			{
				RecursionMax = recursion_max;
				
				int num = 0;
				for (Threshold = 1; Threshold<=threshold_max && Solutions.Count<solution_max; Threshold++)
				{
					// 同じ優先度の医師やシフト枠はランダムに列挙することで様々なパターンをトライ
					for (int trial = 0; trial<trial_max  && Solutions.Count<solution_max; trial++)
					{
						var ctx = (MatchingContext) Context.Clone();
						Recursion = 0;
					
						if ( Match(ref ctx) )
						{
							Solutions.Add( ctx.Refine(refine_cycle) );
							ShowMessage($"#{Solutions.Count}: Score={ctx.Score()} R={Recursion} Th={Threshold}");
							num++;
						}
					}
				}
				return num;		// このTask(スレッド)が得た解の数
			}

			private bool Match(ref MatchingContext context)
			{
				if ( context.Fulfilled() )       { return true;  }
				if ( Recursion++ > RecursionMax) { return false; }

				// 1. シフトの自由度(選択の余地)がなくなった医師を割り当てる
				foreach (Doctor dr in context.InflexibleDoctors())
				{
					foreach (Shift shift in context.AssignableShifts(dr))
					{
						if ( Match(shift, dr, ref context) ) { return true; }
					}
				}
			
				// 2. 割り当て可能な人数が閾値以下のシフト枠に医師を割り当てる
				foreach (Shift shift in context.Shifts)
				{
					int num_available = shift.NumAvailable();
					if (num_available <= Threshold)
					{
						Debug.Assert(num_available > 0);

						//他枠への影響を抑えるため貢献度の低い医師を優先して割り当てる
						foreach (Doctor dr in context.AvailableDoctors(shift, SortOrder.Ascending))
						{
							if ( Match(shift, dr, ref context) ) { return true; }
						}
						return false;	// 割り当て失敗：「解なし」→ thresholdを上げてリトライ
					}
				}

				// 3. 最も貢献度の高い医師に負荷の低い/希望のシフト枠を割り当てる
				foreach (Doctor dr in context.AvailableDoctors(SortOrder.Descending))
				{
					foreach (Shift shift in context.AssignableShifts(dr, SortOrder.Ascending))
					{
						if ( Match(shift, dr, ref context) ) { return true; }
					}
				}
				return false;	// 割り当て失敗：「解なし」→ thresholdを上げてリトライ
			}

			private bool Match(Shift shift, Doctor dr, ref MatchingContext context)
			{
				var   ctx = (MatchingContext) context.Clone();		// contextをコピー
				Shift sht = ctx.Shift(shift.Type, shift.Day);		// それに所属するShiftオブジェクトを探す
				
				if ( ctx.Assign(sht, dr) && Match(ref ctx) )
				{
					context = ctx;
					return true;
				}
				return false;
			}
		}
		//----------------------------------------------
		
		public delegate void Callback(string message);

		private MatchingContext Context;
		public  Callback        ShowMessage { get; set; }

		public ShiftMatcher(ShiftCalendar calendar, DoctorCalendar[] doctors)
		{
			Context = new MatchingContext(calendar, doctors);
		}

		public bool Match(int recursion_max, int trial_max, int threshold_max, int solution_max, int refine_cycle)
		{
			var solutions = new ConcurrentBag<MatchingContext>();
			
			Parallel.For(0, Environment.ProcessorCount-1, i =>
			{
				var task = new MatchingTask(Context, solutions);
				task.ShowMessage += ShowMessage;
				task.Match(recursion_max, trial_max, threshold_max, solution_max, refine_cycle);
			});

			if (solutions.Count > 0)
			{
				Context = solutions.OrderByDescending(c => c.Score()).First();	// 最もスコアの高い解
				return true;
			}
			return false;	// 割り当て失敗：「解なし」→ 医師の当直回数を増やしてリトライ
		}

		// 医師を手動で割り当てる
		public bool Assign(Doctor dr, ShiftType type, int day)
		{
			Shift shift = Context.Shift(type, day);
			return dr!=null && Context.Assign(shift, dr, force:true);
		}

		// 指定されたシフト枠への医師の割り当てを取得
		public IEnumerable<Doctor> Assignment(ShiftType type, int day)
		{
			return Context.Shift(type, day).Doctors;
		}

		public int Score() => Context.Score();
	}
	//------------------------------------------------------------------------------------

	public static class IEnumerableExt
	{
		public static IEnumerable<T> Shuffle<T>(this IEnumerable<T> sequence)
		{
			return sequence.OrderBy( i => Guid.NewGuid() );
		}
	}
}
