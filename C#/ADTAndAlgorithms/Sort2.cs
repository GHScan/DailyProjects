using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;
using System.Diagnostics;

using System.Runtime.CompilerServices; 

class Program
{
    static Random sRandom = new Random();
    static int RandomInt(int max) 
    {
        return sRandom.Next(max);
    }
     [MethodImpl(MethodImplOptions.AggressiveInlining)]
    static void Swap<T>(T[] a, int i, int j)
    {
        T t = a[i];
        a[i] = a[j];
        a[j] = t;
    }
     [MethodImpl(MethodImplOptions.AggressiveInlining)]
    static void SwapIfLess<T>(T[] a, int i, int j) where T : IComparable<T>
    {
        if (a[i].CompareTo(a[j]) < 0) Swap(a, i, j);
    }
    static void BuiltinArraySort<T>(T[] a, int begin, int end) where T : IComparable<T>
    {
        Array.Sort(a, begin, end - begin);
    }
    static void BuiltinLinqSort<T>(T[] a, int begin, int end) where T : IComparable<T>
    {
        foreach (var v in a.Skip(begin).Take(end - begin).OrderBy(i => i))
        {
            a[begin++] = v;
        }
    }
    static void Qsort<T>(T[] a, int begin, int end) where T : IComparable<T>
    {
        if (end - begin <= 1) return;

        int p = begin;
        for (var j = begin + 1; j < end; ++j)
        {
            if (a[j].CompareTo(a[begin]) < 0) Swap(a, j, ++p);
        }
        Swap(a, begin, p);

        Qsort(a, begin, p);
        Qsort(a, p + 1, end);
    }
    static void Qsort2<T>(T[] a, int begin, int end) where T : IComparable<T>
    {
        if (end - begin <= 1) return;

        Swap(a, begin, begin + (end - begin) / 2);
        SwapIfLess(a, end - 1, begin);

        int lo = begin + 1;
        int hi = end - 2;
        while (lo <= hi)
        {
            while (a[begin].CompareTo(a[hi]) < 0) --hi;
            while (a[lo].CompareTo(a[begin]) < 0) ++lo;
            if (lo <= hi) Swap(a, lo++, hi--);
        }
        --lo;
        Swap(a, begin, lo);

        Qsort2(a, begin, lo);
        Qsort2(a, lo + 1, end);
    }
    static void Qsort3<T>(T[] a, int begin, int end) where T : IComparable<T>
    {
        switch (end - begin)
        {
            case 0:
            case 1:
                return;
            case 2:
                SwapIfLess(a, begin + 1, begin);
                return;
            case 3:
                SwapIfLess(a, begin + 1, begin);
                SwapIfLess(a, begin + 2, begin);
                SwapIfLess(a, begin + 2, begin + 1);
                return;
            default:
                break;
        }

        while (end - begin > 3)
        {
            Swap(a, begin, begin + (end - begin) / 2);
            SwapIfLess(a, end - 1, begin);

            int lo = begin + 1;
            int hi = end - 2;
            while (lo <= hi)
            {
                while (a[begin].CompareTo(a[hi]) < 0) --hi;
                while (a[lo].CompareTo(a[begin]) < 0) ++lo;
                if (lo <= hi) Swap(a, lo++, hi--);
            }
            --lo;
            Swap(a, begin, lo);

            if (lo - begin < (end - (lo + 1)))
            {
                Qsort3(a, begin, lo);
                begin = lo + 1;
            }
            else
            {
                Qsort3(a, lo + 1, end);
                end = lo;
            }
        }

        Qsort3(a, begin, end);
    }
    static void CorrectnessTest(Action<int[], int, int>[] funcs)
    {
        Console.WriteLine("\n!!!!!!!!!!!! CorrectnessTest");

        var datas = new[] 
            {
                new int[0],
                new int[]{3},
                new int[]{3,2},
                new int[]{3,2,4},
                new int[]{3,2,4,0},
                new int[]{3,2,4,0,0},
                new int[]{5,3,2,4,0,0},
                Enumerable.Range(0,8).Select(i=>RandomInt(8)).ToArray(),
                Enumerable.Range(0,15).Select(i=>RandomInt(15)).ToArray(),
                Enumerable.Range(0,31).Select(i=>RandomInt(31)).ToArray(),
                Enumerable.Range(0,64).Select(i=>RandomInt(64)).ToArray(),
            };

        foreach (var f in funcs)
        {
            Console.WriteLine("\tTest => {0}...", f.Method.Name);
            foreach (var data in datas)
            {
                var d1 = data.Clone() as int[];
                var d2 = data.Clone() as int[];
                f(d1, 0, d1.Length);
                Array.Sort(d2);
                Debug.Assert(d1.SequenceEqual(d2));
            }
        }
    }
    static void Benchmark(Action<int[], int, int>[] funcs)
    {
        Console.WriteLine("\n!!!!!!!!!!!! Benchmark");

        var datas = new Dictionary<string, int[]>() 
        {
            {"Same 8K", Enumerable.Range(0, 8* 1024).Select(i=>53).ToArray() },
            {"Ordered 8K", Enumerable.Range(0, 8* 1024).ToArray() },
            {"Random 256K", Enumerable.Range(0, 256* 1024).Select(i=>RandomInt(256*1024)).ToArray() },
            {"Random 1M", Enumerable.Range(0, 1024* 1024).Select(i=>RandomInt(1024*1024)).ToArray() },
        };
        foreach (var kv in datas)
        {
            Console.WriteLine(string.Format("#{0}# =>", kv.Key));
            foreach (var f in funcs)
            {
                Console.Write("\t{0,-20} : ", f.Method.Name);

                var clones = Enumerable.Range(0,4).Select(_=>kv.Value.Clone() as int[]).ToArray();
                var i = 0;
                Utils.Timeit(3, () => { 
                    var d = clones[i++];
                    f(d, 0, d.Length);
                });
            }
        }
    }
    
    static void Main(string[] args)
    {
        var funcs = new Action<int[], int, int>[]
        {
            BuiltinArraySort,
            BuiltinLinqSort,
            Qsort,
            Qsort2,
            Qsort3,
        };

        CorrectnessTest(funcs);
#if !DEBUG
        Benchmark(funcs);
#endif
    }
}
