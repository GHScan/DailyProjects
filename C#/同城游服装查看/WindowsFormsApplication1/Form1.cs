using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using System.IO;
using System.Text.RegularExpressions;
using System.Drawing.Imaging;

namespace WindowsFormsApplication1
{
    public partial class Form1 : Form
    {
        public Form1(string path)
        {
            InitializeComponent();

            m_imageRootPath = path;
            if (m_imageRootPath == null) m_imageRootPath = @"G:\软件\GameChannel\share\Image\Clothing\";
            m_imageRootPath =
            Regex.Replace(m_imageRootPath, "GameChannel.*", @"GameChannel\share\Image\Clothing\");

            listBox1.DataSource = Directory.GetDirectories(m_imageRootPath).Select(i=>i.Substring(m_imageRootPath.Length)).ToArray();
        }

        private string m_imageRootPath;

        private void listBox1_SelectedIndexChanged(object sender, EventArgs e)
        {
            flowLayoutPanel2.Controls.Clear();
            string s = listBox1.SelectedItem as string;
            if (s == null) return;
            string path = m_imageRootPath + s;

            foreach (var file in Directory.GetFiles(path))
            {
                if (Regex.IsMatch(file, @"\d+_\d+_p.bmp"))
                {
                    flowLayoutPanel2.Controls.Add(new DrawPanel(file));
                }
            }
        }

        private void timer1_Tick(object sender, EventArgs e)
        {
            foreach (Control panel in flowLayoutPanel2.Controls)
            {
                DrawPanel drawPanel = panel as DrawPanel;
                if (drawPanel != null) drawPanel.NextImage();
            }
        }
    }

    class DrawPanel : Control
    {
        public DrawPanel(string path)
        {
            m_img1 = new Bitmap(Image.FromFile(path));
            m_img2 = new Bitmap(Image.FromFile(path.Replace("_p.bmp", "_w.bmp")));

            Image2Paint = m_img1;

            DoubleBuffered = true;

            Size = new Size(Image2Paint.Width / 2, Image2Paint.Height);
        }

        public void NextImage()
        {
            Image2Paint = Image2Paint == m_img1 ? m_img2 : m_img1;
        }

        protected override void OnPaint(PaintEventArgs e)
        {
            if (Image2Paint == null) return;

            Graphics srcGraphics = Graphics.FromImage(Image2Paint);

            IntPtr srcDC = IntPtr.Zero;
            IntPtr bm = IntPtr.Zero;
            IntPtr oldBm = IntPtr.Zero;

            IntPtr destDC = IntPtr.Zero;

            try
            {
                srcDC = srcGraphics.GetHdc();
                bm = (Image2Paint as Bitmap).GetHbitmap();
                oldBm = SelectObject(srcDC, bm);

                destDC = e.Graphics.GetHdc();

                BitBlt(destDC, 0, 0, Image2Paint.Width / 2, Image2Paint.Height, srcDC, Image2Paint.Width / 2, 0, 0x008800C6);
                BitBlt(destDC, 0, 0, Image2Paint.Width / 2, Image2Paint.Height, srcDC, 0, 0, 0x00EE0086);
            }
            finally
            {
                if (destDC != IntPtr.Zero) e.Graphics.ReleaseHdc();
                if (oldBm != IntPtr.Zero) bm = SelectObject(srcDC, oldBm);
                if (bm != IntPtr.Zero) DeleteObject(bm);
                if (srcDC != IntPtr.Zero) srcGraphics.ReleaseHdc();
            }
        }

        Image Image2Paint
        {
            get { return m_img2Paint; }
            set 
            {
                m_img2Paint = value;
                if (m_img2Paint != null) Invalidate();
            }
        }
        Image m_img1, m_img2;
        Image m_img2Paint;

        [System.Runtime.InteropServices.DllImport("Gdi32.dll")]
        static extern int BitBlt(IntPtr hdcDest, 
          int nXDest, int nYDest, int nWidth, int nHeight,
          IntPtr hdcSrc, 
          int nXSrc, int nYSrc,  
          uint dwRop);
        [System.Runtime.InteropServices.DllImport("Gdi32.dll")]
        static extern IntPtr SelectObject(IntPtr hdc, IntPtr hgdiobj);
        [System.Runtime.InteropServices.DllImport("Gdi32.dll")]
        static extern int DeleteObject(IntPtr hObject);
    }
}
