using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Diagnostics;

namespace CSharp13
{
    public static class Utils
    {
        public static void Print(params object[] args)
        {
            Console.WriteLine(string.Join("\t", args));
        }

        public static void Timeit(int times, Action a)        
        {
            // JIT、Warm up cache
            if (times > 1) a();

            GC.Collect(GC.MaxGeneration, GCCollectionMode.Forced);
            var gcCounts = new int[GC.MaxGeneration + 1];
            for (var i = 0; i < gcCounts.Length; ++i) gcCounts[i] = GC.CollectionCount(i);

            var stopwatch = new Stopwatch();
            stopwatch.Start();
            for (var i = 0; i < times; ++i) a();
            stopwatch.Stop();

            for (var i = 0; i < gcCounts.Length; ++i) gcCounts[i] = GC.CollectionCount(i) - gcCounts[i];
            Console.WriteLine(string.Format("Time: {0:0.######}ms, GC counts: {1}", stopwatch.Elapsed.TotalMilliseconds / times, string.Join(",", gcCounts)));
        }
    }
}
