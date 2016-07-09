using System;
using System.IO;
using System.Text;
using System.Linq;

namespace CSharp2013
{
    internal class Program
    {
        static void Test()
        {
            using (var dict = new PersistentDictionary("1.txt"))
            {
                string line;
                while ((line = Console.ReadLine()) != null)
                {
                    try
                    {
                        var tokens = line.Split();
                        switch (tokens[0].ToLower())
                        {
                            case "add":
                                dict.Add(tokens[1], Encoding.UTF8.GetBytes(tokens[2]));
                                break;
                            case "update":
                                dict[tokens[1]] = Encoding.UTF8.GetBytes(tokens[2]);
                                break;
                            case "remove":
                                dict.Remove(tokens[1]);
                                break;
                            case "dump":
                                {
                                    foreach (var kv in dict.OrderBy(kv => kv.Key))
                                        Console.WriteLine("\t{0}: {1}", kv.Key, Encoding.UTF8.GetString(kv.Value));
                                }
                                break;
                            default:
                                throw new NotSupportedException();
                        }

                        Console.WriteLine("Done.");
                    }
                    catch (Exception e)
                    {
                        Console.WriteLine(e);
                    }
                }
            }
        }

        static void Benchmark()
        {
            var payload = new byte[4 * 1024];

            var dir = "temp";
            if (!Directory.Exists(dir))
                Directory.CreateDirectory(dir);

            Utility.Timeit("File system", 1, () =>
            {
                var random = new Random();
                for (var i = 0; i < 40000; ++i)
                {
                    var path = string.Format("{0}/{1}.txt", dir, random.Next(1000));
                    switch (random.Next(2))
                    {
                        case 0:
                            File.WriteAllBytes(path, payload);
                            break;
                        case 1:
                            if (File.Exists(path)) File.Delete(path);
                            break;
                        default:
                            throw new NotSupportedException();
                    }
                }
            });

            Utility.Timeit("PersistentDictionary", 1, () =>
            {
                if (File.Exists("1.txt"))
                    File.Delete("1.txt");

                var random = new Random();
                using (var dict = new PersistentDictionary("1.txt"))
                {
                    for (var i = 0; i < 40000; ++i)
                    {
                        var key = string.Format("{0}/{1}.txt", dir, random.Next(1000));
                        switch (random.Next(2))
                        {
                            case 0:
                                if (dict.ContainsKey(key)) dict[key] = payload;
                                else dict.Add(key, payload);
                                break;
                            case 1:
                                if (dict.ContainsKey(key)) dict.Remove(key);
                                break;
                            default:
                                throw new NotSupportedException();
                        }
                    }
                }
            });
        }

        private static void Main(string[] args)
        {
            Test();
            // Benchmark();
        }
    }
}