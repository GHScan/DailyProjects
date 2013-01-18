using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;
using System.Diagnostics;
using System.Windows.Forms;
using System.Threading;
using System.Drawing;

namespace CSharp08
{
    partial class Program
    {
        static List<string> GetGarbageFileNames()
        {
            var fileNames = new List<string>();

            {
                // 整个文件夹的所有文件
                string[] envVals = 
                {
                    "windir",
                    "userprofile",
                    "userprofile",
                    "userprofile",
                };
                string[] dirs = 
                {
                    "temp",
                    "local Settings/temporary internet files",
                    "local Settings/temp",
                    "recent",
                };

                for (int i = 0; i < envVals.Length; ++i)
                {
                    string path = Environment.GetEnvironmentVariable(envVals[i]) + "/" + dirs[i];
                    var _fileNames = Directory.GetFiles(path, "*", SearchOption.AllDirectories);
                    fileNames.InsertRange(
                        fileNames.Count > 0 ? fileNames.Count - 1 : 0, _fileNames);
                }
            }

            {
                // 文件夹下的特定类型文件
                string[] envVals = 
                {
                    "systemdrive",
                    "systemdrive",
                    "systemdrive",
                    "systemdrive",
                    "systemdrive",
                    "systemdrive",
                    "windir",
                };
                string[] dirs =
                {
                    "",
                    "",
                    "",
                    "",
                    "",
                    "",
                    "",
                };
                string[] exts =
                {
                    "*.tmp",
                    "*._mp",
                    "*.log",
                    "*.gid",
                    "*.chk",
                    "*.old",
                    "*.bak",
                };
                for (int i = 0; i < envVals.Length; ++i)
                {
                    string path = Environment.GetEnvironmentVariable(envVals[i]) + (dirs[i].Length > 0 ? "/" + dirs[i] + "/" : "");
                    var _fileNames = Directory.GetFiles(path, exts[i]);
                    fileNames.InsertRange(fileNames.Count > 0 ? fileNames.Count - 1 : 0, _fileNames);
                }
            }

            return fileNames;
        }

        [STAThread]
        static void Main(string[] args)
        {
            AppDomain.CurrentDomain.UnhandledException += (o, e) =>
                {
                    MessageBox.Show(e.ExceptionObject.ToString(), "发现异常！");
                };

            Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);

            Form form = new Form();
            form.Size = new Size(600, 400);
            form.StartPosition = FormStartPosition.CenterScreen;
            form.Text = "垃圾文件清理";

            ListBox listBox = new ListBox();
            listBox.Dock = DockStyle.Fill;
            listBox.HorizontalScrollbar = true;
            form.Controls.Add(listBox);

            FlowLayoutPanel panel = new FlowLayoutPanel();
            panel.Dock = DockStyle.Bottom;

            Button btnFind = new Button();
            btnFind.Text = "查找";
            panel.Controls.Add(btnFind);

            Button btnDel = new Button();
            btnDel.Text = "删除";
            panel.Controls.Add(btnDel);

            panel.AutoSize = true;
            form.Controls.Add(panel);

            StatusStrip statuBar = new StatusStrip();
            ToolStripProgressBar statuProgress = new ToolStripProgressBar();
            statuBar.Items.Add(statuProgress);
            ToolStripStatusLabel statuText = new ToolStripStatusLabel("");
            statuBar.Items.Add(statuText);
            form.Controls.Add(statuBar);

            btnFind.Click += (o, e) =>
            {
                btnFind.Enabled = false;
                btnDel.Enabled = false;

                ThreadPool.QueueUserWorkItem(state =>
                    {
                        var names = GetGarbageFileNames();
                        form.Invoke((Action)(() =>
                            {
                                listBox.DataSource = names;
                                statuText.Text = string.Format("找到垃圾文件 {0} 个", names.Count);

                                btnFind.Enabled = true;
                                btnDel.Enabled = true;
                            }));
                    });
            };
            btnDel.Click += (o, e) =>
                {
                    if (listBox.DataSource == null) return;

                    btnFind.Enabled = false;
                    btnDel.Enabled = false;

                    List<string> names = (List<string>)listBox.DataSource;

                    ThreadPool.QueueUserWorkItem(state =>
                    {
                        int leftPercent = 100;
                        int leftFileCnt = names.Count;

                        foreach (var name in names)
                        {
                            try
                            {
                                File.Delete(name);
                            }
                            catch { }

                            --leftFileCnt;
                            int newLeftPercent = (int)((float)100 * leftFileCnt / names.Count);
                            if (newLeftPercent != leftPercent)
                            {
                                leftPercent = newLeftPercent;
                                form.Invoke((Action)(() =>
                                    {
                                        statuProgress.Value = 100 - leftPercent;
                                    }));
                            }
                        }

                        form.Invoke((Action)(() =>
                        {
                            listBox.DataSource = null;
                            statuText.Text = "垃圾清理完成";
                            statuProgress.Value = 0;

                            btnFind.Enabled = true;
                            btnDel.Enabled = true;
                        }));
                    });
                };

            Application.Run(form);
        }
    }
}