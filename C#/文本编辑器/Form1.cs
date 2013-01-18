using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.IO;
using System.Linq;
using System.Diagnostics;

namespace CSharp08Form
{
    public partial class Form1 : Form
    {
        Form3 m_propertyGrid;
        Form4 m_treeViewList;

        public Form1()
        {
            InitializeComponent();

            m_propertyGrid = new Form3();
            m_propertyGrid.Show(dockPanel1);
            m_propertyGrid.DockTo(dockPanel1, DockStyle.Right);

            this.MdiChildActivate += (o, e) => 
            {
                if (this.ActiveMdiChild == null) return;

                PropertyGrid grid = m_propertyGrid.GetPropertyGrid();
                grid.SelectedObject = new FileInfo((this.ActiveMdiChild as Form2).Tag.ToString());

                m_treeViewList.SelectFilePath((this.ActiveMdiChild as Form2).Tag.ToString());
            };

            m_treeViewList = new Form4();
            m_treeViewList.Show(dockPanel1);
            m_treeViewList.DockTo(dockPanel1, DockStyle.Left);

            m_treeViewList.GetFileSysTreeView().NodeMouseDoubleClick += (o, e) =>
                {
                    string path = m_treeViewList.GetPathByNode(e.Node);
                    Form2 form = GetForm2ByPath(path);
                    if (form == null) return;
                    this.ActivateMdiChild(form);
                };

            m_treeViewList.FileChanged += s =>
                {
                    Form2 f2 = GetForm2ByPath(s);
                    if (f2 == null) return;
                    if (f2.GetRichTextBox().Text == File.ReadAllText(s, Encoding.Default).Replace("\r\n", "\n")) return;

                    if (MessageBox.Show(s + "被从外部改变，要重新加载吗？", "确认", MessageBoxButtons.YesNo) == DialogResult.Yes)
                    {
                        ActivateMdiChild(f2);
                        this.closeToolStripMenuItem_Click(null, null);
                        OpenFile(s);
                    }
                };
            m_treeViewList.FileDeleted += s =>
                {
                    Form2 f2 = GetForm2ByPath(s);
                    if (f2 == null) return;
                    if (MessageBox.Show(s + "被从外部删除，要关闭文件吗？", "确认", MessageBoxButtons.YesNo) == DialogResult.Yes)
                    {
                        ActivateMdiChild(f2);
                        this.closeToolStripMenuItem_Click(null, null);
                    }
                };
        }

        private void newToolStripMenuItem_Click(object sender, EventArgs e)
        {
            OpenFileDialog dlg = new OpenFileDialog();
            if (dlg.ShowDialog() != DialogResult.OK) return;

            OpenFile(dlg.FileName);
        }

        private void closeToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (this.ActiveMdiChild != null)
            {
                Form formToClose = this.ActiveMdiChild;

                Form[] otherChild = (from i in this.MdiChildren where i != formToClose select i).ToArray();
                if (otherChild.Length > 0)
                {
                    this.ActivateMdiChild(otherChild[0]);
                }

                formToClose.Close();
                formToClose.MdiParent = null;
            }
        }

        private void OpenFile(string path)
        {
            if (!File.Exists(path)) return;
            if (GetForm2ByPath(path) != null) return;

            Form2 child = new Form2();
            child.MdiParent = this;
            child.TabText = Path.GetFileName(path);
            child.Tag = path;

            RichTextBox box = child.GetRichTextBox();
            box.AllowDrop = true;
            box.DragEnter += this.dockPanel1_DragEnter;
            box.DragDrop += this.dockPanel1_DragDrop;
            box.Text = File.ReadAllText(path, Encoding.Default).Replace("\r\n", "\n");

            {
                ContextMenuStrip menu = new ContextMenuStrip();
                menu.Items.Add("Cut").Click += (o, e) => { box.Cut(); };
                menu.Items.Add("Copy").Click += (o, e) => { box.Copy(); };
                menu.Items.Add("Paste").Click += (o, e) => { box.Paste(); };
                box.ContextMenuStrip = menu;
            }

            box.KeyUp += this.Form1_KeyUp;
            box.TextChanged += (o, e) => { if (child.TabText.EndsWith("*")) return; child.TabText = child.TabText + "*"; };
            

            {
                ContextMenuStrip menu = new ContextMenuStrip();
                menu.Items.Add("Close").Click += this.closeToolStripMenuItem_Click;
                menu.Items.Add("Close All But This").Click += (o, e) =>
                    {
                        this.ActivateMdiChild(child);
                        foreach (var i in this.MdiChildren)
                        {
                            if (i != child)
                            {
                                i.Close();
                                i.MdiParent = null;
                            }
                        }
                    };
                child.TabPageContextMenuStrip = menu;
            }

            child.DockAreas = WeifenLuo.WinFormsUI.Docking.DockAreas.Document;
            child.Show(dockPanel1);

            m_treeViewList.AddFilePath(path);
            child.Disposed += (o, e) => { if(m_treeViewList != null && !m_treeViewList.IsDisposed) m_treeViewList.RemoveFilePath(child.Tag.ToString()); };
        }

        private void aboutToolStripMenuItem_Click(object sender, EventArgs e)
        {
            MessageBox.Show("测试!!");
        }

        private void dockPanel1_DragDrop(object sender, DragEventArgs e)
        {
            string[] s = (string[])e.Data.GetData(DataFormats.FileDrop);
            if (s == null) return;
            foreach (var i in s) OpenFile(i);
        }

        private void dockPanel1_DragEnter(object sender, DragEventArgs e)
        {
            if (e.Data.GetDataPresent(DataFormats.FileDrop))
            {
                e.Effect |= DragDropEffects.Move;
            }
            else e.Effect = DragDropEffects.None;
        }

        private void Form1_KeyUp(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Escape)
            {
                if (MessageBox.Show("确定要退出吗?", "确认", MessageBoxButtons.YesNo) ==
                    DialogResult.Yes) Close();
                return;
            }
        }

        private void exitToolStripMenuItem_Click(object sender, EventArgs e)
        {
            Close();
        }

        private void saveToolStripMenuItem_Click(object sender, EventArgs e)
        {
            Form2 f2 = (Form2)this.ActiveMdiChild;
            if (f2 == null) return;
            if (!f2.TabText.EndsWith("*")) return;
            File.WriteAllText((string)f2.Tag, f2.GetRichTextBox().Text, Encoding.Default);
            f2.TabText = f2.TabText.Substring(0, f2.TabText.Length - 1);
        }

        private void timer1_Tick(object sender, EventArgs e)
        {
            Form2 f2 = this.ActiveMdiChild as Form2;
            if (f2 == null) return;
            RichTextBox box = f2.GetRichTextBox();
            int line = box.GetLineFromCharIndex(box.SelectionStart);
            int column = box.SelectionStart - box.GetFirstCharIndexFromLine(line);
            this.statusStrip1.Items[0].Text = string.Format("{0}, {1}", line, column);
        }

        private Form2 GetForm2ByPath(string path)
        {
            var forms = (from i in this.MdiChildren where (string)(i as Form2).Tag == path select i).ToArray();
            if (forms != null && forms.Length > 0) return forms[0] as Form2;
            return null;
        }
    }
}
