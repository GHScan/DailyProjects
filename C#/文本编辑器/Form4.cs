using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using System.Runtime.InteropServices;
using System.IO;

namespace CSharp08Form
{
    public partial class Form4 : WeifenLuo.WinFormsUI.Docking.DockContent
    {
        public Form4()
        {
            InitializeComponent();
        }

        public void AddFilePath(string path)
        {
            TreeNodeCollection nodes = treeView1.Nodes;
            foreach (var i in path.Split(new char[] { Path.DirectorySeparatorChar }))
            {
                if (!nodes.ContainsKey(i))
                {
                    TreeNode node = nodes.Add(i, i);
                    node.ImageKey = GetImageKey(GetPathByNode(node));
                    node.SelectedImageKey = node.ImageKey;
                }
                nodes = nodes[i].Nodes;
            }

            string dir = Path.GetDirectoryName(path);
            if (!m_fileSysWatchers.ContainsKey(dir))
            {
                FileSystemWatcher watcher = new FileSystemWatcher(dir);
                watcher.EnableRaisingEvents = true;
                watcher.IncludeSubdirectories = false;
                watcher.Changed += (o, e) => { DelegateQueue.PostDelegate(() => { if (this.FileChanged != null) this.FileChanged(e.FullPath); }); };
                watcher.Deleted += (o, e) => { DelegateQueue.PostDelegate(() => { if (this.FileDeleted != null) this.FileDeleted(e.FullPath); }); };
                m_fileSysWatchers.Add(dir, watcher);
            }
        }

        public void RemoveFilePath(string path)
        {
            TreeNode node = GetNodeByPath(path);
            while (node != null && node.Nodes.Count == 0)
            {
                if (node.Parent == null)
                {
                    treeView1.Nodes.Remove(node);
                    node = null;
                }
                else
                {
                    TreeNode parent = node.Parent;
                    parent.Nodes.Remove(node);
                    node = parent;
                }
            }

            string dir = Path.GetDirectoryName(path);
            if (GetNodeByPath(dir) == null)
            {
                m_fileSysWatchers.Remove(dir);
            }
        }

        public void SelectFilePath(string path)
        {
            AddFilePath(path);
            treeView1.SelectedNode = GetNodeByPath(path);
        }

        public string GetPathByNode(TreeNode node)
        {
            List<string> l = new List<string>();
            while (node != null)
            {
                l.Add("" + Path.DirectorySeparatorChar);
                l.Add(node.Text); 
                node = node.Parent;
            }
            l.Reverse();
            StringBuilder builder = new StringBuilder();
            foreach (var i in l) builder.Append(i);
            string r = builder.ToString();
            if (File.Exists(r) && r.EndsWith("" + Path.DirectorySeparatorChar))
            {
                r = r.Substring(0, r.Length - 1);
            }
            return r;
        }

        private string GetImageKey(string path)
        {
            IntPtr h = SysIconHelper.GetIconHandle(path);
            string key = h.ToString();
            if (treeView1.ImageList == null)
            {
                treeView1.ImageList = new ImageList();
                treeView1.ImageList.ColorDepth = ColorDepth.Depth32Bit;
            }
            if (!treeView1.ImageList.Images.ContainsKey(key))
            {
                treeView1.ImageList.Images.Add(key, Icon.FromHandle(h));
            }
            return key;
        }

        private TreeNode GetNodeByPath(string path)
        {
            TreeNode node = null;
            foreach (var i in path.Split(new char[] { Path.DirectorySeparatorChar }))
            {
                if (node == null) node = treeView1.Nodes[i];
                else node = node.Nodes[i];
            }
            return node;
        }

        public TreeView GetFileSysTreeView() { return treeView1;  }


        private Dictionary<string, FileSystemWatcher> m_fileSysWatchers = new Dictionary<string, FileSystemWatcher>();

        public event Action<string> FileChanged;
        public event Action<string> FileDeleted;
    }

    static class SysIconHelper
    {
        [StructLayout(LayoutKind.Sequential)]
        struct SHFILEINFO
        {
            public const int NAMESIZE = 80;
            public const int MAX_PATH = 260;
            public IntPtr hIcon;
            public int iIcon;
            public uint dwAttributes;
            [MarshalAs(UnmanagedType.ByValTStr, SizeConst = MAX_PATH)]
            public string szDisplayName;
            [MarshalAs(UnmanagedType.ByValTStr, SizeConst = NAMESIZE)]
            public string szTypeName;
        };
        [DllImport("Shell32.dll")]
        static extern IntPtr SHGetFileInfo(
            string pszPath,
            uint dwFileAttributes,
            out SHFILEINFO psfi,
            uint cbFileInfo,
            uint uFlags
        );

        public static IntPtr GetIconHandle(string path)
        {
            SHFILEINFO info;
            SHGetFileInfo(path, 0, out info, (uint)Marshal.SizeOf(typeof(SHFILEINFO)), 0x101u);
            return info.hIcon;
        }
    }
}
