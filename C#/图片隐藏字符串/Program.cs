using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using System.Drawing;
using System.Drawing.Imaging;
using System.Collections;

namespace CSharp08
{
    partial class Program
    {
        static byte[] HEADER = new byte[] { 13, 235, 33, 183 };

        static string loadText(string path)
        {
            Bitmap bm = null;
            using (Image img = Image.FromFile(path))
            {
                bm = new Bitmap(img);
            }
            BitmapData data = bm.LockBits(new Rectangle(0, 0, bm.Width, bm.Height), ImageLockMode.ReadOnly, PixelFormat.Format24bppRgb);

            BitArray a = new BitArray(bm.Width * bm.Height * 3 / 8 * 8);

            unsafe
            {
                for (int y = 0; y < bm.Height; ++y)
                {
                    byte *p = (byte*)data.Scan0.ToPointer() + data.Stride * y;
                    for (int x = 0; x < bm.Width; ++x)
                    {
                        int bitArrayIdx = (y * bm.Width + x) * 3;
                        if (bitArrayIdx < a.Length) a[bitArrayIdx] = (p[0] & 1) == 1;
                        ++bitArrayIdx;
                        if (bitArrayIdx < a.Length) a[bitArrayIdx] = (p[1] & 1) == 1;
                        ++bitArrayIdx;
                        if (bitArrayIdx < a.Length) a[bitArrayIdx] = (p[2] & 1) == 1;
                        p += 3;
                    }
                }
            }

            bm.UnlockBits(data);

            byte[] bytes = new byte[bm.Width * bm.Height * 3 / 8];
            a.CopyTo(bytes, 0);

            bool yes = bytes.Take(4).Select((v, i) => v == HEADER[i]).All(i => i);
            if (!yes) return "";

            bm.Dispose();

            bytes = bytes.Skip(4).ToArray();
            bytes = bytes.Take(Array.IndexOf(bytes, (byte)0)).ToArray();

            return Encoding.Default.GetString(bytes);
        }

        static bool saveText(string path, string text)
        {
            byte[] bytes = Encoding.Default.GetBytes(text);

            Bitmap bm = null;
            using (Image img = Image.FromFile(path))
            {
                bm = new Bitmap(img);
            }
            if (bm.Width * bm.Height * 3 / 8 < bytes.Length + 4) return false;

            bytes = HEADER.Concat(bytes).ToArray();
            {
                byte[] newBytes = new byte[bm.Width * bm.Height * 3 / 8];
                Array.Copy(bytes, newBytes, bytes.Length);
                bytes = newBytes;
            }
            BitArray a = new BitArray(bytes);

            BitmapData data = bm.LockBits(new Rectangle(0, 0, bm.Width, bm.Height), ImageLockMode.ReadWrite, PixelFormat.Format24bppRgb);

            unsafe
            {
                for (int y = 0; y < bm.Height; ++y)
                {
                    byte* p = (byte*)data.Scan0.ToPointer() + data.Stride * y;
                    for (int x = 0; x < bm.Width; ++x)
                    {
                        int bitArrayIdx = (bm.Width * y + x) * 3;
                        if (bitArrayIdx < a.Length) if (a[bitArrayIdx]) p[0] |= 1; else p[0] &= 0xfe;
                        ++bitArrayIdx;
                        if (bitArrayIdx < a.Length) if (a[bitArrayIdx]) p[1] |= 1; else p[1] &= 0xfe;
                        ++bitArrayIdx;
                        if (bitArrayIdx < a.Length) if (a[bitArrayIdx]) p[2] |= 1; else p[2] &= 0xfe;
                        p += 3;
                    }
                }
            }

            bm.UnlockBits(data);
            bm.Save("_" + path);
            bm.Dispose();

            return true;
        }

        static void Main(string[] args)
        {
            string s = "原文"; 
            Print(saveText("a.png", s));
            Print(loadText("_a.png"));
        }
    }
}