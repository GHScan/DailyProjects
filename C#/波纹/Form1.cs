using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.Diagnostics;
using System.Drawing.Imaging;

namespace CSharp08UI
{
    public partial class Form1 : Form
    {
        public Form1()
        {
            InitializeComponent();

            m_backgroundBm = new Bitmap(Image.FromFile("me.jpg"));
            m_frameBuf = new Bitmap(m_backgroundBm.Width, m_backgroundBm.Height);

            m_rippleBufs = new short[2][,] { new short[m_backgroundBm.Height, m_backgroundBm.Width], null };
            for (int y = 0; y < m_rippleBufs[0].GetLength(0); ++y)
            {
                for (int x = 0; x < m_rippleBufs[0].GetLength(1); ++x)
                {
                    m_rippleBufs[0][y, x] = 0;
                }
            }
            m_rippleBufs[1] = (short[,])m_rippleBufs[0].Clone();
            m_activeRippleBuf = 0;

            ClientSize = new Size(m_backgroundBm.Width, m_backgroundBm.Height);
            FormBorderStyle = FormBorderStyle.FixedDialog;
            MinimizeBox = MaximizeBox = false;

            if (components == null) components = new Container();
            Timer timer = new Timer(components);
            timer.Tick += (sender, e) => this.UpdateFrame();
            timer.Interval = 10;
            timer.Start();

            m_paintFps = 0;
            timer = new Timer(components);
            // timer.Tick += (sender, e) => { Debug.Print("paintFps: " + m_paintFps + " updateFps: " + m_updateFps);  m_paintFps = m_updateFps = 0; };
            timer.Interval = 1000;
            timer.Start();
        }

        private void UpdateFrame()
        {
            ++m_updateFps;

            Stopwatch watch = new Stopwatch();
            watch.Start();

            int w = m_backgroundBm.Width;
            int h = m_backgroundBm.Height;
            unsafe
            {
                fixed (short* oldPtr = m_rippleBufs[m_activeRippleBuf])
                fixed (short* newPtr = m_rippleBufs[1 - m_activeRippleBuf])
                {
                    short* _oldPtr = oldPtr + w, _newPtr = newPtr + w;

                    for (int y = 1; y < h - 1; ++y)
                    {
                        for (int x = 1; x < w - 1; ++x)
                        checked
                        {
                            int r = 
                                ((_oldPtr[x - 1] + _oldPtr[x + 1] + _oldPtr[x - w] + _oldPtr[x + w]) >> 1)
                                - _newPtr[x];
                            r -= r >> 5;
                            _newPtr[x] = unchecked((short)r);
                        }

                        _oldPtr += w;_newPtr += w;
                    }
                }
            }

            m_activeRippleBuf = 1 - m_activeRippleBuf;

            // Debug.Print("UpdateFrame watch.ElapsedMilliseconds: " + watch.ElapsedMilliseconds);

            Refresh();
        }

        private void Form1_Paint(object sender, PaintEventArgs e)
        {
            ++m_paintFps;

            Stopwatch watch = new Stopwatch();
            watch.Start();

            BitmapData destData = m_frameBuf.LockBits(ClientRectangle, ImageLockMode.WriteOnly, PixelFormat.Format32bppArgb);
            BitmapData srcData = m_backgroundBm.LockBits(ClientRectangle, ImageLockMode.ReadOnly, PixelFormat.Format32bppArgb);
            int w = m_backgroundBm.Width;
            int h = m_backgroundBm.Height;
            unsafe
            {
                fixed(short *ripBufPtr = m_rippleBufs[m_activeRippleBuf])
                {
                    short *_ripBufPtr = ripBufPtr + w;
                    int* destPtr = (int*)destData.Scan0 + destData.Stride / 4;

                    for (int y = 1; y < h - 1; ++y)
                    {
                        for (int x = 1; x < w - 1; ++x)
                        {
                            int mapX = x +
                                (_ripBufPtr[x - 1] - _ripBufPtr[x + 1]) / 8;
                            int mapY = 
                                y +
                                (_ripBufPtr[x - w] - _ripBufPtr[x + w]) / 8;
                            mapX = Math.Min(Math.Max(mapX, 0), w - 1);
                            mapY = Math.Min(Math.Max(mapY, 0), h - 1);

                            destPtr[x] = ((int*)srcData.Scan0 + srcData.Stride / 4 * mapY)[mapX];
                        }

                        destPtr += destData.Stride / 4;
                        _ripBufPtr += w;
                    }
                }
            }
            m_backgroundBm.UnlockBits(srcData);
            m_frameBuf.UnlockBits(destData);

            e.Graphics.DrawImage(m_frameBuf, 0, 0);

            // Debug.Print("Form1_Paint watch.ElapsedMilliseconds: " + watch.ElapsedMilliseconds);
        }

        private void Form1_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Escape) Close();
        }

        private void Form1_MouseDown(object sender, MouseEventArgs e)
        {
            short h = -128;
            int radius = 16;
            for (int i = -radius; i <= radius; ++i)
            {
                for (int j = -radius; j <= radius; ++j)
                {
                    if (i * i + j * j <= radius * radius)
                    {
                        if (e.Y + i < 0 || e.Y + i >= m_backgroundBm.Height ||
                            e.X + j < 0 || e.X + j >= m_backgroundBm.Width) continue;
                        m_rippleBufs[m_activeRippleBuf][e.Y + i, e.X + j] = h;
                    }
                }
            }

        }

        Bitmap m_backgroundBm;
        Bitmap m_frameBuf;

        short[][,] m_rippleBufs;
        int m_activeRippleBuf;

        int m_paintFps;
        int m_updateFps;
    }
}
