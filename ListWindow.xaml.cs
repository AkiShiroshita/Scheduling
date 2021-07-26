using System;
using System.IO;
using System.Linq;
using System.Text;
using System.Windows;
using System.ComponentModel;
using System.Threading.Tasks;
using System.Windows.Controls;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Runtime.CompilerServices;
using Microsoft.VisualBasic.FileIO;

namespace 内科
{
	public class DoctorItem : INotifyPropertyChanged
	{
		public event PropertyChangedEventHandler PropertyChanged;

        private void NotifyPropertyChanged([CallerMemberName] String propertyName = "")
        {
			PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(propertyName));
        }

		private int    _Count;			// 当直回数
		private string _Work;			// 勤務(曜)日
		private string _Dayoff;			// 公休(曜)日
		private string _Unpreferable;	// できれば避けたい
		private string _Preferable;		// 希望(曜)日
		private string _Unavailable;	// 不可(曜)日

		public int    No   { get; private set; }	// 医師番号（＝医師インデックス＋１）
		public string Name { get; private set; }	// 表示名

		public int Count
		{
			get { return _Count; }
			set { if (_Count != value) { _Count = value; NotifyPropertyChanged(); } }
		}
		public string Work
		{
			get { return _Work; }
			set {
				if ( _Work!=value && IsValid(value) )
				{
					_Work = Validate(value);
					NotifyPropertyChanged();
				}
			}
		}
		public string DayOff
		{
			get { return _Dayoff; }
			set {
				if ( _Dayoff!=value && IsValid(value) )
				{
					_Dayoff = Validate(value);
					NotifyPropertyChanged();
				}
			}
		}
		public string Preferable
		{
			get { return _Preferable; }
			set {
				if ( _Preferable!=value && IsValid(value) )
				{
					_Preferable = Validate(value);
					NotifyPropertyChanged();
				}
			}
		}
		public string Unpreferable
		{
			get { return _Unpreferable; }
			set {
				if ( _Unpreferable!=value  && IsValid(value) )
				{
					_Unpreferable = Validate(value);
					NotifyPropertyChanged();
				}
			}
		}
		public string Unavailable
		{
			get { return _Unavailable; }
			set {
				if ( _Unavailable!=value && IsValid(value) )
				{
					_Unavailable = Validate(value);
					NotifyPropertyChanged();
				}
			}
		}

		public DoctorItem(int index)
		{
			Doctor dr    = Doctor.At(index);
			No           = index + 1;
			Name         = dr.Name;
			Count        = dr.BaseCount;
			Work         = dr.WorkStyle[0];
			DayOff       = dr.WorkStyle[1];
			Preferable   = dr.WorkStyle[2];
			Unpreferable = dr.WorkStyle[3];
			Unavailable  = dr.WorkStyle[4];
		}

		private string Validate(string value)
		{
			string[] seps   = { ",", " ", "、", "　" };
			char[]   shifts = { 'D', 'N', 'A', 'P', 'L', 'F' };
			char[]   days   = { '月', '火', '水', '木', '金', '土', '日' };

			List<string> n_tokens = new List<string>();
			List<string> d_tokens = new List<string>();

			foreach (string token in value.ToUpper().Split(seps, StringSplitOptions.RemoveEmptyEntries))
			{
				int n, m;
				string t = token.TrimEnd(shifts);

				if ( int.TryParse(t, out n) )		// 具体的な日付指定（ex. 13）
				{
					if (n<1 || n>31) { return null; }

					n_tokens.Add(token);
				}
				else if ( t.Contains('-') )			// 具体的な日付指定（ex. 2-5）
				{
					string[] ts = t.Split(new char[] { '-' });
					
					if (ts.Length != 2)                                               { return null; }
					if ( !int.TryParse(ts[0], out m) || !int.TryParse(ts[1], out n) ) { return null; }
					if (m>=n || m<1 || n>31)                                          { return null; }

					n_tokens.Add(token);
				}
				else								// 曜日指定（ex 月）
				{
					int len = t.Length;
					if ( len<1 || 2<len )       { return null; }
					if ( !days.Contains(t[0]) ) { return null; }
					if ( len == 2 )
					{
						if ( !int.TryParse(t.Substring(1,1), out n) ) { return null; }
						if ( n<1 || 5<n )                             { return null; }
					}
					d_tokens.Add(token);
				}
			}
			return string.Join(",", d_tokens.Concat(n_tokens));
		}
		private bool IsValid(string value) => Validate(value) != null;

		public override string ToString()
		{
			return $"{No},{Name},{Count},\"{Work}\",\"{DayOff}\",\"{Preferable}\",\"{Unpreferable}\",\"{Unavailable}\"";
		}

		public override int GetHashCode() => ToString().GetHashCode();
	}
	//------------------------------------------------------------------------------------

	public partial class ListWindow : Window
	{
		readonly ObservableCollection<DoctorItem> DoctorItems;
		readonly ShiftCalendar                    Calendar;
		int                                       Hash;			// 変更確認のためのハッシュ値

		public ListWindow(ShiftCalendar calendar)
		{
			InitializeComponent();

			DoctorItems = new ObservableCollection<DoctorItem>();
			Calendar    = calendar;

			for (int i = 0; i < Doctor.Count; i++)
			{
				DoctorItems.Add( new DoctorItem(i) );
			}

			DoctorTable.ItemsSource   = DoctorItems;
			DoctorTable.SelectionMode = DataGridSelectionMode.Single;
			DoctorTable.SelectionUnit = DataGridSelectionUnit.CellOrRowHeader;

			Load();
		}

		public new void Show()
		{
			Hash = ItemsHash();
			ShowStatus();
			base.Show();
		}

		public async void ShowStatus()
		{
			await Task.Delay(100);	// CellEndEditingはObservableCollectionの変更前に呼ばれるので100ms待つ

			int year       = Calendar.Year;
			int month      = Calendar.Month;
			int capacity   = Calendar.Capacity();
			int count_sum  = DoctorItems.Sum(di => di.Count);
			Status.Text = $"{year}/{month} 月分　当直コマ数：{capacity}　医師回数：{count_sum}";
		}

		public DoctorCalendar[] BuildDrCalendars()
		{
			var calendars = new DoctorCalendar[Doctor.Count];

			for (int i = 0; i < Doctor.Count; i++)
			{
				var item  = DoctorItems[i];

				calendars[i] = new DoctorCalendar(i, item.Count, Calendar);
				calendars[i].Build(item.Work, item.DayOff, item.Preferable, item.Unpreferable, item.Unavailable);
			}
			return calendars;
		}

		private bool Load()
		{
			string file = $"勤務・当直希望({Calendar.Year}年{Calendar.Month:00}月).csv";
			var    enc  = Encoding.GetEncoding("Shift_JIS");

			Clear_Click(null, null);
			if ( !File.Exists(file) ) { return false; }

			try {	
				using (var fs = new FileStream(file, FileMode.Open, FileAccess.Read, FileShare.ReadWrite)) 
				using (var sr = new TextFieldParser(fs, enc))
				{
					sr.TextFieldType = FieldType.Delimited;
					sr.SetDelimiters(",");

					sr.ReadLine();	// skip header

					for (int i = 0; !sr.EndOfData; i++)
					{
						string[]   values    = sr.ReadFields();
						int        index     = int.Parse(values[0]) - 1;
						string     name      = values[1];
						int        count     = int.Parse(values[2]);
						string[]   workstyle = values.Skip(3).Take(5).ToArray();
						DoctorItem item      = DoctorItems[index];

						if (item.Name == name)
						{
							item.Count        = count;
							item.Work         = workstyle[0];
							item.DayOff       = workstyle[1];
							item.Preferable   = workstyle[2];
							item.Unpreferable = workstyle[3];
							item.Unavailable  = workstyle[4];
						}
					}
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

		private void Save_Click(object sender, RoutedEventArgs e)
		{
			string file = $"勤務・当直希望({Calendar.Year}年{Calendar.Month:00}月).csv";
			var    enc  = Encoding.GetEncoding("Shift_JIS");

			try {
				using (var sr = new StreamWriter(file, false, enc))
				{
					sr.WriteLine("No,名前,回数,勤務曜日,公休曜日,希望曜日,避ける曜日,不可曜日");

					foreach (DoctorItem item in DoctorItems) { sr.WriteLine(item); }
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
			for (int i = 0; i < Doctor.Count; i++)
			{
				DoctorItems[i] = new DoctorItem(i);
			}
			ShowStatus();
		}

		private void Window_Closing(object sender, CancelEventArgs e)
		{
			if (this.Visibility == Visibility.Visible)
			{
				if (ItemsHash() != Hash)
				{
					var result =  MessageBox.Show($"変更をファイルに保存しますか？", "確認", MessageBoxButton.YesNo);
					if (result == MessageBoxResult.Yes) { Save_Click(sender, null); }
				}
				e.Cancel = true;
				this.Visibility = Visibility.Collapsed;
			}
		}

		private void CellEditEnding(object sender, DataGridCellEditEndingEventArgs e)
		{
			ShowStatus();
		}
		
		private int ItemsHash()
		{
			return DoctorItems.Select(item => item.GetHashCode())
				              .Aggregate((sum, hash) => unchecked(sum + hash));
		}
	}
}