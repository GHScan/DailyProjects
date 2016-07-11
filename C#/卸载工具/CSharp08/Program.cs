using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using System.Windows.Forms;
using Microsoft.Win32;
using System.Threading;
using System.Text.RegularExpressions;
using System.IO;
using System.ComponentModel;
using System.Collections;
using System.Drawing;

using System.Linq.Expressions;

namespace CSharp08
{
    partial class Program
    {
        // 这一系列用不上了->
        [AttributeUsage(AttributeTargets.Property, Inherited=false, AllowMultiple=true)]
        class LocalizationAttribute : Attribute
        {
            public LocalizationAttribute(string localText)
            {
                LocalizationText = localText;
            }
            public string LocalizationText { get; private set; }
        }

        public class MyCustomTypeDescriptor : ICustomTypeDescriptor
        {
            public MyCustomTypeDescriptor(object o) { m_o = o; }
            public virtual AttributeCollection GetAttributes() { return TypeDescriptor.GetAttributes(m_o.GetType()); }
            public virtual string GetClassName() { return TypeDescriptor.GetClassName(m_o.GetType()); }
            public virtual string GetComponentName() { return TypeDescriptor.GetComponentName(m_o.GetType()); }
            public virtual TypeConverter GetConverter() { return TypeDescriptor.GetConverter(m_o.GetType()); }
            public virtual EventDescriptor GetDefaultEvent() { return TypeDescriptor.GetDefaultEvent(m_o.GetType()); }
            public virtual PropertyDescriptor GetDefaultProperty() { return TypeDescriptor.GetDefaultProperty(m_o.GetType()); }
            public virtual object GetEditor(Type editorBaseType) { return TypeDescriptor.GetEditor(m_o.GetType(), editorBaseType); }
            public virtual EventDescriptorCollection GetEvents() { return TypeDescriptor.GetEvents(m_o.GetType()); }
            public virtual EventDescriptorCollection GetEvents(Attribute[] attributes) { return TypeDescriptor.GetEvents(m_o.GetType(), attributes); }
            public virtual PropertyDescriptorCollection GetProperties() { return TypeDescriptor.GetProperties(m_o.GetType()); }
            public virtual PropertyDescriptorCollection GetProperties(Attribute[] attributes) { return TypeDescriptor.GetProperties(m_o.GetType(), attributes); }
            public virtual object GetPropertyOwner(PropertyDescriptor pd) { return m_o; }
            private object m_o;
        }

        class LocalizationMyCustomTypeDescriptor : MyCustomTypeDescriptor
        {
            public LocalizationMyCustomTypeDescriptor(object o) : base(o) { }
            public override PropertyDescriptorCollection GetProperties()
            {
                return
                new PropertyDescriptorCollection(
                    base.GetProperties().Cast<PropertyDescriptor>().
                    Select(p => new LocalizationPropertyDescriptor(p)).ToArray());
            }
            public override PropertyDescriptorCollection GetProperties(Attribute[] attributes)
            {
                return
                new PropertyDescriptorCollection(
                    base.GetProperties(attributes).Cast<PropertyDescriptor>().
                    Select(p => new LocalizationPropertyDescriptor(p)).ToArray());
            }
        }

        class LocalizationPropertyDescriptor : PropertyDescriptor
        {
            public LocalizationPropertyDescriptor(PropertyDescriptor realDescriptor)
                : base(realDescriptor)
            {
                m_realDescriptor = realDescriptor;
            }
            public override Type ComponentType { get { return m_realDescriptor.ComponentType; } }
            public override bool IsReadOnly { get { return m_realDescriptor.IsReadOnly; } }
            public override Type PropertyType { get { return m_realDescriptor.PropertyType; } }
            public override bool CanResetValue(object component) { return m_realDescriptor.CanResetValue(component); }
            public override object GetValue(object component) { return m_realDescriptor.GetValue(component); }
            public override void ResetValue(object component) { m_realDescriptor.ResetValue(component); }
            public override void SetValue(object component, object value) { m_realDescriptor.SetValue(component, value); }
            public override bool ShouldSerializeValue(object component) { return m_realDescriptor.ShouldSerializeValue(component); }

            public override string DisplayName
            {
                get
                {
                    object[] attris =
                    m_realDescriptor.ComponentType.
                        GetProperty(m_realDescriptor.Name).
                        GetCustomAttributes(typeof(LocalizationAttribute), false);
                    if (attris == null || attris.Length == 0) return m_realDescriptor.DisplayName;
                    return (attris[0] as LocalizationAttribute).LocalizationText;
                }
            }

            private PropertyDescriptor m_realDescriptor;
        }
        // 这一系列用不上了<-

        [AttributeUsage(AttributeTargets.Property, Inherited = false, AllowMultiple = false)]
        class DisplayWidth : Attribute
        {
            public DisplayWidth(int w)
            {
                Width = w;
            }
            public int Width { get; private set; }
        }

        struct UinstallItem
        {
            [DisplayName("."), DisplayWidth(25)]
            public Icon PEIcon { get; set; }
            [DisplayName("产品名"), DisplayWidth(320)]
            public string DisplayName { get; set; }
            [DisplayName("描述"), DisplayWidth(240)]
            public string Description { get; set; }
            [DisplayName("安装日期"), DisplayWidth(110)]
            public DateTime? InstallDate { get; set; }
            [DisplayName("公司"), DisplayWidth(160)]
            public string CompanyName { get; set; }
            [DisplayName("版本"), DisplayWidth(120)]
            public string Version { get; set; }
            [DisplayName("注册表项"), DisplayWidth(180)]
            public string RegistryKey { get; set; }
            [DisplayName("安装路径"), DisplayWidth(320)]
            public string InstallFolder { get; set; }
            // public string QuietUninstall { get; set; }
            [DisplayName("卸载命令"), DisplayWidth(320)]
            public string UninstallCmd { get; set; }
        }

        static class AppConfig
        {
            static AppConfig()
            {
                ConfigImpl = new AppRegistryConfig("Scan", "UninstallerByCS");
            }
            private static AppRegistryConfig ConfigImpl;

            public static bool BrowseWow64
            {
                get
                {
                    string s =
                        ConfigImpl.GetConfig("BrowseWow64",
                        System.IO.Directory.Exists(Environment.ExpandEnvironmentVariables(@"%windir%\SysWOW64")) ? "True" : "False");
                    return bool.Parse(s);
                }
                set
                {
                    ConfigImpl.SetConfig("BrowseWow64", value);
                }
            }
        }

        static string GetStringValueFromRegistry(RegistryKey key, string valueName)
        {
            object val = key.GetValue(valueName);
            if (val == null) return string.Empty;

            RegistryValueKind kind = key.GetValueKind(valueName);
            if (kind == RegistryValueKind.DWord ||
                kind == RegistryValueKind.QWord ||
                kind == RegistryValueKind.String)
            {
                return val.ToString();
            }
            else if (kind == RegistryValueKind.ExpandString)
            {
                return Environment.ExpandEnvironmentVariables(val as string);
            }
            else if (kind == RegistryValueKind.MultiString)
            {
                StringBuilder buf = new StringBuilder();
                foreach (var s in val as string[])
                {
                    buf.AppendFormat("{0}, ", s);
                }
                return buf.ToString();
            }
            else if (kind == RegistryValueKind.Binary)
            {
                StringBuilder buf = new StringBuilder();
                foreach (var b in val as byte[])
                {
                    buf.AppendFormat("{0:x2} ", b);
                }
                if (buf.Length > 0) buf.Remove(buf.Length - 1, 1);
                return buf.ToString();
            }
            else return string.Empty;
        }

        static RegistryKey OpenUninstallRegistryKey(bool writable)
        {
            return 
            Registry.LocalMachine.OpenSubKey(AppConfig.BrowseWow64 ?
                @"Software\Wow6432Node\Microsoft\Windows\CurrentVersion\Uninstall" :
                @"Software\Microsoft\Windows\CurrentVersion\Uninstall", 
                writable);
        }

        static bool BuildUinstallItem(RegistryKey itemKey, ref UinstallItem item)
        {
            string IconPEPath = string.Empty;

            // 先准备最容易得到的数据
            item.RegistryKey = itemKey.Name.Substring(itemKey.Name.LastIndexOf('\\') + 1);

            item.DisplayName = GetStringValueFromRegistry(itemKey, "DisplayName");
            // item.QuietUninstall = itemKey.GetValue("QuietUninstallString", string.Empty).ToString();
            item.UninstallCmd = GetStringValueFromRegistry(itemKey, "UninstallString");
            IconPEPath = GetStringValueFromRegistry(itemKey, "DisplayIcon");
            item.InstallFolder = GetStringValueFromRegistry(itemKey, "InstallLocation");
            item.Version = GetStringValueFromRegistry(itemKey, "DisplayVersion");
            item.CompanyName = GetStringValueFromRegistry(itemKey, "Publisher");

            try
            {
                string s = GetStringValueFromRegistry(itemKey, "InstallDate");
                if (!string.IsNullOrEmpty(s))
                {
                    item.InstallDate = DateTime.ParseExact(s, "yyyyMMdd", null);
                }
            }
            catch { item.InstallDate = null; }

            // 得到重要的中间数据PEPath
            string PEPath = string.Empty;
            if (!string.IsNullOrEmpty(IconPEPath))
            {
                Match m = Regex.Match(IconPEPath, "\\p{L}:[^?<>|:\"]+?\\.exe");
                if (m.Success) PEPath = m.Groups[0].ToString();
            }
            if (string.IsNullOrEmpty(PEPath) &&
                !string.IsNullOrEmpty(item.UninstallCmd))
            {
                Match m = Regex.Match(item.UninstallCmd, "\\p{L}:[^?<>|:\"]+?\\.exe");
                if (m.Success) PEPath = m.Groups[0].ToString();
            }
            if (string.IsNullOrEmpty(PEPath) &&
                !string.IsNullOrEmpty(item.InstallFolder))
            {
                string[] files = Directory.GetFiles(item.InstallFolder, "*.exe");
                if (files != null && files.Length > 0)
                {
                    PEPath = files[0];
                }
            }

            // 根据PEPath反过来可以尝试修正一些数据
            if (!string.IsNullOrEmpty(PEPath))
            {
                if (string.IsNullOrEmpty(IconPEPath))
                {
                    IconPEPath = PEPath;
                }
                if (string.IsNullOrEmpty(item.InstallFolder))
                {
                    item.InstallFolder = Path.GetDirectoryName(PEPath);
                }
            }

            // 读取PEPath文件中的内容，修正一些数据
            if (!string.IsNullOrEmpty(PEPath))
            {
                Win32Wrap.FileVersionInfo info = Win32Wrap.GetFileVersionInfo(PEPath);
                if (info.langBasedInfoList != null && info.langBasedInfoList.Length > 0)
                {
                    Win32Wrap.FileVersionInfo.LanguageBasedFileInfo lanBasedInfo = info.langBasedInfoList[0];

                    item.Description = lanBasedInfo.fileDescription;
                    if (string.IsNullOrEmpty(item.DisplayName) && 
                        !string.IsNullOrEmpty(lanBasedInfo.productName))
                    {
                        item.DisplayName = lanBasedInfo.productName;
                    }
                    if (string.IsNullOrEmpty(item.CompanyName) && 
                        !string.IsNullOrEmpty(lanBasedInfo.companyName))
                    {
                        item.CompanyName = lanBasedInfo.companyName;
                    }
                    if (string.IsNullOrEmpty(item.Version) &&
                        !string.IsNullOrEmpty(lanBasedInfo.productVersion))
                    {
                        item.Version = lanBasedInfo.productVersion;
                    }
                }
            }

            if (!string.IsNullOrEmpty(IconPEPath))
            {
                int iconIdx = 0;
                IntPtr[] smallIcon = new IntPtr[1];

                {
                    Match m = Regex.Match(IconPEPath, "(.+),(\\d)$");
                    if (m.Success)
                    {
                        try
                        {
                            IconPEPath = m.Groups[1].ToString();
                            iconIdx = int.Parse(m.Groups[2].ToString());
                        }
                        catch { }
                    }
                }

                if (IconPEPath.Length > 2 &&
                    IconPEPath.StartsWith("\"") &&
                    IconPEPath.EndsWith("\""))
                {
                    IconPEPath = IconPEPath.Substring(1, IconPEPath.Length - 2);
                }

                if (Win32Import.ExtractIconEx(IconPEPath, 0, null, smallIcon, 1) > 0 &&
                    smallIcon[0] != IntPtr.Zero)
                {
                    item.PEIcon = Icon.FromHandle(smallIcon[0]);
                }
            }
            if (item.PEIcon == null)
            {
                IntPtr[] smallIcon = new IntPtr[1];
                if (Win32Import.ExtractIconEx("shell32.dll", 2, null, smallIcon, 1) > 0 &&
                smallIcon[0] != IntPtr.Zero)
                {
                    item.PEIcon = Icon.FromHandle(smallIcon[0]);
                }
            }

            // 修正安装日期
            if (item.InstallDate == null)
            {
                item.InstallDate = Win32Wrap.GetRegstryKeyLastWriteTime(
                    Win32Import.HKEY_LOCAL_MACHINE, 
                    itemKey.Name.Substring(itemKey.Name.IndexOf('\\') + 1));
            }

            // 保证显示的数据存在
            if (string.IsNullOrEmpty(item.DisplayName) &&
                !string.IsNullOrEmpty(PEPath))
            {
                item.DisplayName = Path.GetFileNameWithoutExtension(PEPath);
            }

            return !string.IsNullOrEmpty(item.DisplayName);
        }

        static List<UinstallItem> PrepareUinstallList()
        {
            List<UinstallItem> list = new List<UinstallItem>();
            using (RegistryKey uinstallKey = OpenUninstallRegistryKey(false))
            {
                string[] subKeyNames = uinstallKey.GetSubKeyNames();

                // 显示等待对话框
                Form form = new Form();
                form.StartPosition = FormStartPosition.CenterScreen;
                form.Size = new Size(250, 50);
                form.MinimizeBox = false;
                form.MaximizeBox = false;
                form.Text = "正在查找已经安装的软件...";

                ProgressBar bar = new ProgressBar();
                bar.Dock = DockStyle.Fill;
                bar.Maximum = 100;
                bar.Minimum = 0;
                bar.Value = bar.Minimum;
                form.Controls.Add(bar);

                BackgroundWorker worker = new BackgroundWorker();
                worker.WorkerReportsProgress = true;
                worker.DoWork += (o, e) =>
                {
                    for (int i = 0; i < subKeyNames.Length; ++i)
                    {
                        string subKeyName = subKeyNames[i];
                        using (RegistryKey subKey = uinstallKey.OpenSubKey(subKeyName, false))
                        {
                            UinstallItem item = new UinstallItem();
                            if (BuildUinstallItem(subKey, ref item))
                            {
                                list.Add(item);
                            }
                        }
                        worker.ReportProgress(i * 100 / subKeyNames.Length);
                    }
                };
                worker.ProgressChanged += (o, e) =>
                {
                    bar.Value = e.ProgressPercentage;
                };
                worker.RunWorkerCompleted += (o, e) =>
                {
                    form.Close();
                };
                worker.RunWorkerAsync();

                form.ShowDialog();
                worker.Dispose();
                form.Dispose();
            }
            return list;
        }

        static void TryUninstallItem(UinstallItem item)
        {
            Form form = new Form();
            form.Size = new Size(350, 300);
            form.Text = item.DisplayName;
            form.StartPosition = FormStartPosition.CenterScreen;

            FlowLayoutPanel btnPanel = new FlowLayoutPanel();
            btnPanel.Dock = DockStyle.Bottom;
            btnPanel.FlowDirection = FlowDirection.LeftToRight;
            btnPanel.BorderStyle = BorderStyle.FixedSingle;
            btnPanel.Height = 30;

            Button btnInstallFolder = new Button() { Text = "安装目录" };
            btnInstallFolder.Enabled = !string.IsNullOrEmpty(item.InstallFolder);
            btnInstallFolder.Click += (o, e) => 
            {
                Win32Import.ShellExecute(form.Handle, "open", item.InstallFolder, null, null, 5);
            };
            btnPanel.Controls.Add(btnInstallFolder);

            Button btnUninstall = new Button() { Text = "卸载" };
            btnUninstall.Enabled = !string.IsNullOrEmpty(item.UninstallCmd);
            btnUninstall.Click += (o, e) =>
            {
                if (Win32Import.WinExec(item.UninstallCmd, 5) > 31)
                {
                    form.Close();
                }
                else MessageBox.Show(
                    "卸载的过程中发生异常！", 
                    "警告", MessageBoxButtons.OK, MessageBoxIcon.Warning);
            };
            btnPanel.Controls.Add(btnUninstall);

            Button btnDel = new Button() { Text = "删除" };
            btnDel.Click += (o, e) =>
            {
                if (!string.IsNullOrEmpty(item.UninstallCmd) &&
                    MessageBox.Show(
                "检测到该软件自带卸载功能，仍然要强制从安装列表中删除项目吗？", "确认",
                MessageBoxButtons.YesNo, MessageBoxIcon.Question) == DialogResult.No) return;

                if (MessageBox.Show("这项操作具有很高的风险，你仍然要继续吗？", "确认",
                MessageBoxButtons.YesNo, MessageBoxIcon.Question) == DialogResult.No) return;
                using (RegistryKey k = OpenUninstallRegistryKey(true))
                {
                    k.DeleteSubKey(item.RegistryKey);
                    form.Close();
                }
            };
            btnPanel.Controls.Add(btnDel);

            Button btnClose = new Button() { Text = "关闭" };
            btnClose.Click += (o, e) => { form.Close(); };
            btnPanel.Controls.Add(btnClose);

            PropertyGrid grid = new PropertyGrid();
            grid.Dock = DockStyle.Fill;
            grid.PropertySort = PropertySort.NoSort;
            grid.ToolbarVisible = false;
            grid.SelectedObject = item;

            form.Controls.Add(grid);
            form.Controls.Add(btnPanel);

            // esc处理
            KeyEventHandler escKeyupHandler = (o, e) =>
            {
                if (e.KeyCode == Keys.Escape) form.Close();
            };
            form.KeyPreview = true;
            form.KeyUp += escKeyupHandler;

            form.ShowDialog();
            form.Dispose();
        }

        static Func<object, object> MakeGetPropertyFunc(System.Reflection.PropertyInfo info)
        {
            var par_o = Expression.Parameter(typeof(object), "o");
            var method = Expression.Call(Expression.Convert(par_o, info.DeclaringType), info.GetGetMethod());
            return Expression.Lambda<Func<object, object>>(
                Expression.Convert(method, typeof(object)), new ParameterExpression[]{par_o}).Compile();
        }

        [STAThread]
        static void Main(string[] args)
        {
            AppDomain.CurrentDomain.UnhandledException += (o, e) =>
            {
                MessageBox.Show(e.ExceptionObject.ToString(), "发生错误！");
            };

            Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);

            List<UinstallItem> itemList = PrepareUinstallList();

            Form form = new Form();
            form.Text = "软件卸载助手 v1.0";
            form.Size = new Size(800, 500);
            form.StartPosition = FormStartPosition.CenterScreen;

            DataGridView grid = new DataGridView();
            grid.Dock = DockStyle.Fill;
            grid.ReadOnly = true;
            grid.SelectionMode = DataGridViewSelectionMode.FullRowSelect;
            grid.MultiSelect = false;
            grid.AutoSizeColumnsMode = DataGridViewAutoSizeColumnsMode.None;
            grid.RowHeadersVisible = false;
            grid.AllowUserToResizeRows = false;
            grid.AllowUserToResizeColumns = false;
            grid.CellBorderStyle = DataGridViewCellBorderStyle.RaisedHorizontal;
            grid.DataSource = itemList;
            form.Controls.Add(grid);

            StatusStrip statusBar = new StatusStrip();
            statusBar.Dock = DockStyle.Bottom;
            form.Controls.Add(statusBar);

            ToolStripStatusLabel statusLabelItemCnt = new ToolStripStatusLabel();
            statusLabelItemCnt.TextAlign = ContentAlignment.MiddleLeft;
            statusLabelItemCnt.Width = 90;
            statusLabelItemCnt.BorderSides = ToolStripStatusLabelBorderSides.All;
            statusBar.Items.Add(statusLabelItemCnt);
            ToolStripStatusLabel statusLabelSearchStr = new ToolStripStatusLabel();
            statusLabelSearchStr.Spring = true;
            statusLabelSearchStr.TextAlign = ContentAlignment.MiddleLeft;
            statusBar.Items.Add(statusLabelSearchStr);

            // 单击列头，排序
            bool ascendingSort = true;
            grid.ColumnHeaderMouseClick += (o, e) =>
            {
                var column = grid.Columns[e.ColumnIndex];

                {
                    Type t = column.ValueType;
                    if (t.IsGenericType && t.GetGenericTypeDefinition().FullName == "System.Nullable`1")
                    {
                        t = t.GetGenericArguments()[0];
                    }
                    if (t.GetInterface("IComparable") == null) return;
                }

                grid.ScrollBars = ScrollBars.None;
                grid.DataSource = null;
                ascendingSort = !ascendingSort;
                var fGet = MakeGetPropertyFunc(typeof(UinstallItem).GetProperty(column.DataPropertyName));
                itemList.Sort((x, y) =>
                {
                    int r = 0;
                    object valX = fGet(x);
                    object valY = fGet(y);
                    if (valX == null && valY == null) r = 0;
                    else if (valX == null) r = -1;
                    else if (valY == null) r = 1;
                    else r = (valX as IComparable).CompareTo(valY);
                    return ascendingSort ? r : -r;
                });
                grid.DataSource = itemList;
                grid.ScrollBars = ScrollBars.Both;
            };

            // 控制显示的列数
            int visibleColumn = 4;
            Action adjustGridColumnFunc = () =>
            {
                for (int i = 0; i < grid.Columns.Count; ++i)
                {
                    var column = grid.Columns[i];
                    column.Width = 
                        (typeof(UinstallItem).GetProperty(column.DataPropertyName).
                        GetCustomAttributes(typeof(DisplayWidth), false)[0] as DisplayWidth).Width;
                    column.Visible = i < visibleColumn;
                }
            };
            grid.DataBindingComplete += (o, e) =>
            {
                adjustGridColumnFunc();
                statusLabelItemCnt.Text = string.Format("共有{0}个项目", itemList.Count);
            };

            // 右键菜单
            ContextMenuStrip contextMenu = new ContextMenuStrip();
            contextMenu.Items.Add("详细信息").Click += (o, e) =>
            {
                visibleColumn = 104 - visibleColumn;
                adjustGridColumnFunc();
            };
            contextMenu.Items.Add(string.Format(AppConfig.BrowseWow64 ? "兼容软件" : "非兼容软件")).Click += (o, e) =>
            {
                AppConfig.BrowseWow64 = !AppConfig.BrowseWow64;
                Win32Import.WinExec(Environment.CommandLine, 5);
                form.Close();
            };
            contextMenu.Items.Add("关于").Click += (o, e) =>
            {
                MessageBox.Show("Scan制作！", "关于");
            };
            MouseEventHandler mouseHandlerShowMenu = (o, e) =>
            {
                if (e.Button == MouseButtons.Right)
                {
                    contextMenu.Show(grid, e.Location);
                }
            };
            grid.MouseClick += mouseHandlerShowMenu;
            form.MouseClick += mouseHandlerShowMenu;

            // 输入定位; 包括把form和grid的输入字符都交给输入框
            TextBox searchTextBox = new TextBox();
            searchTextBox.Location = new Point(-5, -5);
            searchTextBox.Size = new Size(1, 1);
            searchTextBox.ImeMode = ImeMode.On;
            searchTextBox.TextChanged += (o, e) =>
            {
                statusLabelSearchStr.Text = "";
                if (string.IsNullOrEmpty(searchTextBox.Text)) return;
                statusLabelSearchStr.Text = string.Format("搜索：{0}", searchTextBox.Text);
                int idx = 0;
                foreach (string name in itemList.Select(n => n.DisplayName))
                {
                    if (name.StartsWith(searchTextBox.Text, StringComparison.OrdinalIgnoreCase))
                    {
                        Program.Assert(idx >= 0 && idx < grid.Rows.Count);
                        grid.CurrentCell = grid.Rows[idx].Cells[0];
                        break;
                    }
                    ++idx;
                }
            };
            form.Controls.Add(searchTextBox);
            grid.MouseClick += (o, e) => { searchTextBox.Text = string.Empty; };
            KeyPressEventHandler keyPressFunc = (o, e) =>
            {
                if (char.IsControl(e.KeyChar)) return;
                searchTextBox.Text += e.KeyChar;
                searchTextBox.SelectionStart = searchTextBox.Text.Length;
            };
            PreviewKeyDownEventHandler previewKeyDownFunc = (o, e) =>
            {
                if (e.KeyCode == Keys.Up || e.KeyCode == Keys.Down ||
                    e.KeyCode == Keys.Escape || e.KeyCode == Keys.Enter) return;
                searchTextBox.Focus();
            };
            form.KeyPress += keyPressFunc;
            grid.KeyPress += keyPressFunc;
            form.PreviewKeyDown += previewKeyDownFunc;
            grid.PreviewKeyDown += previewKeyDownFunc;

            // 弹出详细信息
            grid.DoubleClick += (o, e) =>
            {
                if (grid.SelectedRows == null || grid.SelectedRows.Count == 0) return;
                int idx = grid.SelectedRows[0].Index;
                Program.Assert(idx >= 0 && idx < itemList.Count);
                TryUninstallItem(itemList[idx]);
            };

            // esc，退出
            KeyEventHandler escHandler = (o, e) =>
            {
                if (e.KeyCode == Keys.Escape) form.Close();
            };
            form.KeyUp += escHandler;
            grid.KeyUp += escHandler;
            searchTextBox.KeyUp += escHandler;

            // 检测列表中的项目是否已经从注册表中删除，是的话重启
            var registryChangeDetectTimer = new System.Windows.Forms.Timer();
            registryChangeDetectTimer.Interval = 1000;
            registryChangeDetectTimer.Tick += (o, e) =>
            {
                using (RegistryKey uninstallKey = OpenUninstallRegistryKey(false))
                {
                    List<string> subKeyNames = uninstallKey.GetSubKeyNames().ToList();
                    subKeyNames.Sort();
                    foreach (UinstallItem item in itemList)
                    {
                        if (subKeyNames.BinarySearch(item.RegistryKey) < 0)
                        {
                            Win32Import.WinExec(Environment.CommandLine, 5);
                            form.Close();
                            break;
                        }
                    }
                }
            };
            registryChangeDetectTimer.Start();

            Application.Run(form);
        }
    }
}