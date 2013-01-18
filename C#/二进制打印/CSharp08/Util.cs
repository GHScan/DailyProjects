using System;

using System.Diagnostics;
using System.Runtime.InteropServices;

namespace CSharp08
{
    partial class Program
    {
        public static void Print(params object[] a)
        {
            foreach (var i in a)
            {
                Console.Write("{0}\t", i ?? "null");
            }
            Console.WriteLine("");
        }

        public static void Assert(bool b)
        {
            Debug.Assert(b);
        }

        public static void PerfTimer(Action f, params string[] name)
        {
            Assert(name.Length <= 1);

            Stopwatch watch = new Stopwatch();
            watch.Start();
            f();
            watch.Stop();
            float seconds = (watch.ElapsedMilliseconds / 1000.0f);

            if (name.Length > 0) Print(name[0], ":", seconds);
            else Print(seconds);
        }

        public static byte[] ToByteArray<T>(T a)
        where T : struct
        {
            int size = Marshal.SizeOf(typeof(T));
            IntPtr p = Marshal.AllocHGlobal(size);
            try
            {
                Marshal.StructureToPtr(a, p, false);

                byte[] r = new byte[size];
                Marshal.Copy(p, r, 0, r.Length);
                return r;
            }
            finally
            {
                Marshal.FreeHGlobal(p);
            }
        }

        public static T FromByteArray<T>(byte[] a)
        where T : struct
        {
            Assert(a.Length == Marshal.SizeOf(typeof(T)));

            IntPtr p = Marshal.AllocHGlobal(a.Length);
            try
            {
                Marshal.Copy(a, 0, p, a.Length);

                T r = (T)Marshal.PtrToStructure(p, typeof(T));
                return r;
            }
            finally
            {
                Marshal.FreeHGlobal(p);
            }
        }
    }
}