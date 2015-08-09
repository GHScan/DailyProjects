using System;
using System.Diagnostics;
using System.Linq;
using System.Threading;

namespace CSharp2013
{
    internal class Program
    {
        private const int kSampleCount = 1024 * 1024 * 60;

        [Conditional("DEBUG")]
        private static void DebugWriteLine(object obj)
        {
            Console.WriteLine(obj);
        }

        private static void Naive()
        {
            var random = new Random();

            var totalCount = 0;
            for (var i = 0; i < kSampleCount; ++i)
            {
                var x = random.NextDouble();
                var y = random.NextDouble();
                if (x * x + y * y < 1) ++totalCount;
            }

            DebugWriteLine(totalCount * 4.0 / kSampleCount);
        }

        private static void Parallel()
        {
            var kThreadCount = System.Environment.ProcessorCount;
            var totalCount = Enumerable.Range(0, kThreadCount)
                .AsParallel()
                .Select(threadIdx =>
                {
                    var random = new Random(threadIdx);
                    var count = 0;
                    for (var i = 0; i < kSampleCount / kThreadCount; ++i)
                    {
                        var x = random.NextDouble();
                        var y = random.NextDouble();
                        if (x * x + y * y < 1) ++count;
                    }
                    return count;
                }).Sum();

            DebugWriteLine(totalCount * 4.0 / kSampleCount);
        }

        private static void Main(string[] args)
        {
#if DEBUG
            const int LOOP = 1;
#else
            const int LOOP = 5;
#endif

            Utility.Timeit("Naive", LOOP, Naive);
            Utility.Timeit("Parallel", LOOP, Parallel);
        }
    }
}
