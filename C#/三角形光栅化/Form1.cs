using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.Drawing.Imaging;
using System.Diagnostics;

namespace CSharp08UI
{
    public partial class Form1 : Form
    {
        public Form1()
        {
            InitializeComponent();
            InitializeTriangles();

            ResetBitmap();
        }

        private void Form1_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Escape) Close();
            else if (e.KeyCode == Keys.Z)
            {
                foreach (var i in m_triangles) i.Rotate(1.0f);
            }
            else if (e.KeyCode == Keys.Q)
            {
                foreach (var i in m_triangles) i.Scale(0.9f);
            }
            else if (e.KeyCode == Keys.E)
            {
                foreach (var i in m_triangles) i.Scale(1.1f);
            }
            else if (e.KeyCode == Keys.A)
            {
                foreach (var i in m_triangles) i.Translate(-10, 0);
            }
            else if (e.KeyCode == Keys.S)
            {
                foreach (var i in m_triangles) i.Translate(0, 10);
            }
            else if (e.KeyCode == Keys.W)
            {
                foreach (var i in m_triangles) i.Translate(0, -10);
            }
            else if (e.KeyCode == Keys.D)
            {
                foreach (var i in m_triangles) i.Translate(10, 0);
            }

            Invalidate();
        }

        private void Form1_SizeChanged(object sender, EventArgs e)
        {
            ResetBitmap();
            Invalidate();
        }

        private void ResetBitmap()
        {
            m_bm = new Bitmap(ClientSize.Width, ClientSize.Height);
            m_bmGraphics = Graphics.FromImage(m_bm);
            m_bmBrush = new SolidBrush(BackColor);
        }

        private void Form1_Paint(object sender, PaintEventArgs e)
        {
            if (m_bm == null) return;

            m_bmGraphics.FillRectangle(m_bmBrush, e.ClipRectangle);

            BitmapData bmData = m_bm.LockBits(ClientRectangle, ImageLockMode.WriteOnly, PixelFormat.Format32bppArgb);
            foreach(var i in m_triangles) 
            {
                if (i != null) i.Draw(m_bm, bmData, Color.Red);
            }
            m_bm.UnlockBits(bmData);

            e.Graphics.DrawImage(m_bm, Point.Empty);
        }

        private void InitializeTriangles()
        {
            m_triangles = new Graphics2D.Triangle[]
            {
                new Graphics2D.Triangle(new Point(20, 20), new Point(20, 40), new Point(30, 30)),
                new Graphics2D.Triangle(new Point(100, 50), new Point(200, 50), new Point(150, 100)),
                new Graphics2D.Triangle(new Point(100, 100), new Point(100, 200), new Point(300, 100)),
                new Graphics2D.Triangle(new Point(10, 210), new Point(50, 230), new Point(10, 250)),
            };
        }

        private Graphics m_bmGraphics;
        private Brush m_bmBrush;
        private Bitmap m_bm;
        private Graphics2D.Triangle[] m_triangles;
    }
}
