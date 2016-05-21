using System.Collections;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;

namespace CSharp2013
{
    internal class Program
    {
        private static void ConvertReport(string inputTxt, string outputTsv)
        {
            var funcNames = new List<string>();
            var result =
                new ConcurrentDictionary<string, ConcurrentDictionary<int, double>>();

            using (var input = File.OpenText(inputTxt))
            {
                var line = input.ReadLine();
                while (line != null)
                {
                    var len = int.Parse(line.Split()[2]);
                    while ((line = input.ReadLine()) != null && !line.Trim().StartsWith("Len -"))
                    {
                        var funcName = line.Trim();
                        var times = new List<double>();
                        for (var i = 0; i < 4; ++i)
                        {
                            var tokens = input.ReadLine().Split(':');
                            times.Add(double.Parse(tokens[1].Trim().Split()[0]));
                        }
                        result.GetOrAdd(funcName,
                            s =>
                            {
                                funcNames.Add(s);
                                return new ConcurrentDictionary<int, double>();
                            })
                            .TryAdd(len, times.Average());
                    }
                }
            }

            using (var output = File.CreateText(outputTsv))
            {
                output.WriteLine("#\t" + string.Join("\t", funcNames));

                foreach (var len in result.First().Value.Keys.OrderBy(v => v))
                {
                    output.WriteLine(string.Format("{0:f3} KB\t", len / 1000.0) + string.Join("\t", funcNames.Select(func => result[func][len])));
                }
            }
        }

        private static readonly bool gEnableAvx = true;
        private static void PrintCaseStatements(TextWriter output, int branchCount, int value, int off)
        {
            for (var i = branchCount; i > 0; i >>= 1)
            {
                if ((i & value) != 0)
                {
                    switch (i)
                    {
                        case 32:
                            if (gEnableAvx) output.WriteLine("\t_memcpy32_avx(dst + {0}, src + {0});", off);
                            else output.WriteLine("\t_memcpy32_sse2(dst + {0}, src + {0});", off);
                            break;
                        case 16:
                            output.WriteLine("\t_memcpy16_sse2(dst + {0}, src + {0});", off);
                            break;
                        case 8:
                            output.WriteLine("\t*reinterpret_cast<uint64_t*>(dst + {0}) = *reinterpret_cast<uint64_t const*>(src + {0});", off);
                            break;
                        case 4:
                            output.WriteLine("\t*reinterpret_cast<uint32_t*>(dst + {0}) = *reinterpret_cast<uint32_t const*>(src + {0});", off);
                            break;
                        case 2:
                            output.WriteLine("\t*reinterpret_cast<uint16_t*>(dst + {0}) = *reinterpret_cast<uint16_t const*>(src + {0});", off);
                            break;
                        case 1:
                            output.WriteLine("\tdst[{0}] = src[{0}];", off);
                            break;
                    }
                    off += i;
                }
            }
        }

        private static void PrintFallthrough(string outputFile, int branchCount)
        {
            Debug.Assert((branchCount & (branchCount - 1)) == 0);

            var currFlag = branchCount - 1;
            var currList = new List<int> { currFlag };
            var result = new List<List<int>> { currList };
            var used = new BitArray(branchCount);
            used.Set(currFlag, true);
            for (var i = 1; i < branchCount; ++i)
            {
                var j = branchCount - 1;
                for (; j >= 0 && (used[j] || (currFlag & j) != j); --j) ;
                if (j < 0)
                {
                    j = branchCount - 1;
                    for (; j >= 0 && used[j]; --j) ;
                    currList = new List<int>();
                    result.Add(currList);
                }
                used.Set(j, true);
                currFlag = j;
                currList.Add(j);
            }

            using (var fo = File.CreateText(outputFile))
            {
                foreach (var list in result)
                {
                    for (var i = 0; i < list.Count - 1; ++i)
                    {
                        fo.WriteLine("case {0}:", list[i]);
                        PrintCaseStatements(fo, branchCount, list[i] & ~list[i + 1], list[i + 1]);
                    }
                    fo.WriteLine("case {0}:", list.Last());
                    PrintCaseStatements(fo, branchCount, list.Last(), 0);
                    fo.WriteLine("break;");
                    fo.WriteLine();
                }
            }
        }

        private static void Main(string[] args)
        {
            // PrintFallthrough("1.txt", 64);
            ConvertReport(args[0], args[1]);
        }
    }
}