using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using System.IO;
using System.Diagnostics;

namespace LANFileTransfer
{
    public partial class FormMain : Form
    {
        public FormMain()
        {
            InitializeComponent();
            

            if (components == null) components = new Container();

#if DEBUG
            Text += " " + Path.GetPathRoot(Application.ExecutablePath);
#endif

            this.FormClosing += (o, e) => 
            {
                if (m_appLogic.State != AppLogicState.WaitingForDropFile)
                {
                    if (MessageBox.Show("正在传输文件，确认要退出吗？", "确认", MessageBoxButtons.YesNo) != DialogResult.Yes)
                    {
                        e.Cancel = true;
                    }
                }
            };

            m_textBox.MouseDoubleClick += FormMain_MouseDoubleClick;
            m_textBox.KeyUp += FormMain_KeyUp;

            m_appLogic = new AppLogic();
            m_appLogic.FileTransFound += s =>
                {
                    return MessageBox.Show(s, "提示", MessageBoxButtons.YesNo) == DialogResult.Yes;
                };
            m_appLogic.ErrorHandler += error => MessageBox.Show(error);
            m_appLogic.ProgressChanged += (percent) =>
                {
                    if (m_enableUpdateSpecUI != null && m_enableUpdateSpecUI.HasSig)
                    {
                        switch (m_appLogic.State)
                        {
                            case AppLogicState.SendingFile:
                                m_textBox.Lines = new string[]
                            { 
                                "正在发送文件:", 
                                m_appLogic.TransingFileName,
                                "已发送文件（个）:",
                                m_appLogic.TransedFileCount.ToString(),
                                "总耗时（秒）:",
                                m_appLogic.TransSeconds.ToString(),
                            };
                                break;
                            case AppLogicState.ReceivingFile:
                                m_textBox.Lines = new string[] 
                            { 
                                "正在接受文件:", 
                                m_appLogic.TransingFileName,
                                "已接受文件（个）:",
                                m_appLogic.TransedFileCount.ToString(),
                                "总耗时（秒）:",
                                m_appLogic.TransSeconds.ToString(),
                            };
                                break;
                            default:
                                break;
                        }

                        m_enableUpdateSpecUI.ClearSig();
                    }
                    
                    ((ToolStripProgressBar)m_statusStrip.Items[0]).Value = percent;
                };
            m_appLogic.StateChanged += () =>
                {
                    switch (m_appLogic.State)
                    {
                        case AppLogicState.WaitingForDropFile:
                            m_textBox.Lines = new string[]{ "拖拽文件到这里" };
                            ((ToolStripProgressBar)m_statusStrip.Items[0]).Value = 0;

                            if (m_enableUpdateSpecUI != null)
                            {
                                m_enableUpdateSpecUI.Dispose();
                                m_enableUpdateSpecUI = null;
                            }
                            break;
                        case AppLogicState.SendingFile:
                        case AppLogicState.ReceivingFile:
                            Program.Assert(m_enableUpdateSpecUI == null);
                            m_enableUpdateSpecUI = new AutoResetSig(250);
                            break;
                        default:
                            break;
                    }
                };
            m_textBox.Lines = new string[] { "拖拽文件到这里" };

            {
                Timer t = new Timer();
                this.components.Add(t);
                t.Tick += (o, e) => 
                {
                    if (m_appLogic.State != AppLogicState.WaitingForDropFile)
                    {
                        ((ToolStripStatusLabel)m_statusStrip.Items[1]).Text =
                            string.Format("{0,3} MB/S", m_appLogic.TransSpeed);
                    }
                };
                t.Interval = 1000;
                t.Start();
            }
        }

        private void FormMain_DragEnter(object sender, DragEventArgs e)
        {
            if (m_appLogic.State == AppLogicState.WaitingForDropFile &&
                e.Data.GetDataPresent(DataFormats.FileDrop))
            {
                e.Effect = DragDropEffects.Link;
            }
        }

        private void FormMain_DragDrop(object sender, DragEventArgs e)
        {
            if (m_appLogic.State == AppLogicState.WaitingForDropFile &&
                e.Data.GetDataPresent(DataFormats.FileDrop))
            {
                e.Effect = DragDropEffects.Link;

                string[] s = (string[])e.Data.GetData(DataFormats.FileDrop);
                if (s != null && s.Length > 0) m_appLogic.SendFiles(s[0]);
            }
        }

        private void FormMain_FormClosed(object sender, FormClosedEventArgs e)
        {
            m_appLogic.Dispose();
        }

        private void FormMain_MouseDoubleClick(object sender, MouseEventArgs e)
        {
            if (m_appLogic.State != AppLogicState.WaitingForDropFile)
            {
                if (MessageBox.Show("要中断传输吗？", "确认", MessageBoxButtons.YesNo) == DialogResult.Yes)
                {
                    m_appLogic.CancelWork();
                }
            }
        }

        private void FormMain_KeyUp(object sender, KeyEventArgs e)
        {
            switch (e.KeyCode)
            {
                case Keys.F1:
                    MessageBox.Show(@"
1. 将文件或文件夹拖到窗体上，可以开始传输文件。
2. 在传输过程中双击窗体可以取消传输。
3. 端口冲突时尝试配置'LANFT_config.xml'文件。
4. ESC退出。
", "帮助");
                    break;
                case Keys.Escape:
                    Close();
                    break;
                default:
                    break;
            }
        } 

        private AppLogic m_appLogic;
        // 用于避免ui刷新过快影响效率
        private AutoResetSig m_enableUpdateSpecUI;
    }

    // 定时有信号;
    class AutoResetSig : IDisposable
    {
        public bool HasSig { get; private set; }

        public AutoResetSig(int interval)
        {
            HasSig = true;

            m_timer = new Timer();
            m_timer.Interval = interval;
            m_timer.Tick += (o, e) => { HasSig = true; };
            m_timer.Start();
        }

        public void ClearSig()
        {
            HasSig = false;
        }

        public void Dispose()
        {
            m_timer.Stop();
            m_timer.Dispose();
        }

        Timer m_timer;
    }
}
