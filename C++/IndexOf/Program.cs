using System;
using System.Diagnostics;
using System.Runtime.InteropServices;
using System.Threading;

namespace App
{
    public static class Native
    {
        [DllImport("NativeDll.dll", EntryPoint = "IndexOf16")]
        public static extern int IndexOf16(IntPtr array, int length, ushort value);

        [DllImport("NativeDll.dll")]
        public static extern int IndexOf16_SSE2(IntPtr array, int length, ushort value);

        [DllImport("NativeDll.dll")]
        public static extern int IndexOf16_AVX(IntPtr array, int length, ushort value);
    }

    internal class Program
    {
        private static int IndexOf16_Managed(ushort[] array, int length, ushort value)
        {
            for (var i = 0; i < length; ++i)
                if (array[i] == value) return i;
            return -1;
        }

        private static unsafe int IndexOf16(ushort* array, int length, ushort value)
        {
            for (var i = 0; i < length; ++i)
                if (array[i] == value) return i;
            return -1;
        }

        private static void Timing(string name, int times, Action func)
        {
            if (times > 1) func();

            var stopwatch = Stopwatch.StartNew();
            for (var i = 0; i < times; ++i)
            {
                func();
            }
            stopwatch.Stop();

            Console.WriteLine("{0} : {1} s", name, stopwatch.Elapsed.TotalSeconds);
        }

        private static void Main(string[] args)
        {
            var array = new ushort[2048];
            for (var i = 0; i < 256; ++i)
                array[i] = 0;
            for (var i = 256; i < 512; ++i)
                array[i] = 1;
            for (var i = 512; i < array.Length - 512; ++i)
                array[i] = (ushort)i;
            for (var i = array.Length - 512; i < array.Length; ++i)
                array[i] = 2;

            const int repeat = 10;
            var sideEffect = 0;

            var lens = new[] { 1, 3, 5, 10, 20, 30, 45, 60, 100, 200, 300, 500, 1000, array.Length };
            foreach (var len in lens)
            {
                Console.WriteLine("Len - {0}", len);

                Timing("\tIndexOf16 Managed", 3, () =>
                {
                    var counter = 0;
                    for (var _ = 0; _ < repeat; ++_)
                    {
                        for (var off = 0; off <= array.Length - len; ++off)
                        {
                            for (var v = 0; v < array.Length; ++v)
                            {
                                counter += IndexOf16_Managed(array, len, (ushort)v);
                            }
                        }
                    }
                    Volatile.Write(ref sideEffect, sideEffect + counter);
                });

                Timing("\tIndexOf16", 3, () =>
                {
                    unsafe
                    {
                        fixed (ushort* p = array)
                        {
                            var counter = 0;
                            for (var _ = 0; _ < repeat; ++_)
                            {
                                for (var off = 0; off <= array.Length - len; ++off)
                                {
                                    for (var v = 0; v < array.Length; ++v)
                                    {
                                        counter += IndexOf16(p, len, (ushort)v);
                                    }
                                }
                            }
                            Volatile.Write(ref sideEffect, sideEffect + counter);
                        }
                    }
                });

                Timing("\tNative IndexOf16", 3, () =>
                {
                    unsafe
                    {
                        fixed (ushort* p = array)
                        {
                            var counter = 0;
                            for (var _ = 0; _ < repeat; ++_)
                            {
                                for (var off = 0; off <= array.Length - len; ++off)
                                {
                                    for (var v = 0; v < array.Length; ++v)
                                    {
                                        counter += Native.IndexOf16((IntPtr)p, len, (ushort)v);
                                    }
                                }
                            }
                            Volatile.Write(ref sideEffect, sideEffect + counter);
                        }
                    }
                });

                Timing("\tNative IndexOf16_SSE2", 3, () =>
                {
                    unsafe
                    {
                        fixed (ushort* p = array)
                        {
                            var counter = 0;
                            for (var _ = 0; _ < repeat; ++_)
                            {
                                for (var off = 0; off <= array.Length - len; ++off)
                                {
                                    for (var v = 0; v < array.Length; ++v)
                                    {
                                        counter += Native.IndexOf16_SSE2((IntPtr)p, len, (ushort)v);
                                    }
                                }
                            }
                            Volatile.Write(ref sideEffect, sideEffect + counter);
                        }
                    }
                });

                Timing("\tNative IndexOf16_AVX", 3, () =>
                {
                    unsafe
                    {
                        fixed (ushort* p = array)
                        {
                            var counter = 0;
                            for (var _ = 0; _ < repeat; ++_)
                            {
                                for (var off = 0; off <= array.Length - len; ++off)
                                {
                                    for (var v = 0; v < array.Length; ++v)
                                    {
                                        counter += Native.IndexOf16_AVX((IntPtr)p, len, (ushort)v);
                                    }
                                }
                            }
                            Volatile.Write(ref sideEffect, sideEffect + counter);
                        }
                    }
                });
            }
        }
    }
}