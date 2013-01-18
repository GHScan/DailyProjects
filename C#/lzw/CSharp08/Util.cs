using System;
using System.Diagnostics;

namespace CSharp08
{
    partial class Program
    {
        public static void print(object s) { Console.WriteLine(s ?? "null"); }
        public static void print(params object[] a) { foreach (var i in a) { Console.Write(i ?? "null"); Console.Write(" "); } print(""); }

        public static void assert(bool b)
        {
            Debug.Assert(b);
        }

        public delegate void __forTimer();
        public static void timer(__forTimer f, params string[] name)
        {
            assert(name.Length <= 1);

            Stopwatch watch = new Stopwatch();
            watch.Start();
            f();
            watch.Stop();
            float seconds = (watch.ElapsedMilliseconds / 1000.0f);

            if (name.Length > 0) print(name[0], ":", seconds);
            else print(seconds);
        }
    }
}