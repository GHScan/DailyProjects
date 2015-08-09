using System;
using System.Diagnostics;

namespace CSharp2013
{
    public static class Utility
    {
        public static void Timeit(string name, int times, Action action)
        {
            // JIT、Warm up cache
            if (times > 1) action();

            GC.Collect(GC.MaxGeneration, GCCollectionMode.Forced);
            var gcCounts = new int[GC.MaxGeneration + 1];
            for (var i = 0; i < gcCounts.Length; ++i) gcCounts[i] = GC.CollectionCount(i);

            var stopwatch = new Stopwatch();
            stopwatch.Start();
            for (var i = 0; i < times; ++i) action();
            stopwatch.Stop();

            for (var i = 0; i < gcCounts.Length; ++i) gcCounts[i] = GC.CollectionCount(i) - gcCounts[i];

            Console.WriteLine(
                "{0,-24} => Time: {1:0.######}ms, GC counts: {2}", 
                name,
                stopwatch.Elapsed.TotalMilliseconds/times,
                string.Join(",", gcCounts));
        }
    }
}
