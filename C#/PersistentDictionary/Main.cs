using System;
using System.Collections;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net;
using System.Text;

namespace CSharp2013
{
    public class Program
    {
        public sealed class PersistentDictionary : IEnumerable<KeyValuePair<string, byte[]>>, IDisposable
        {
            private readonly string mWriteAheadLogPath;
            private BinaryWriter mWriteAheadLogWriter;
            private readonly Dictionary<string, byte[]> mDictionary = new Dictionary<string, byte[]>();
            private int mVersion;

            public PersistentDictionary(string writeAheadLogPath)
            {
                mWriteAheadLogPath = writeAheadLogPath;
                AutoFlush = true;
                ShrinkWriteAheadLogInterval = 10000;

                if (File.Exists(mWriteAheadLogPath))
                {
                    mDictionary = ReplayWriteAheadLog(mWriteAheadLogPath);
                }

                ShrinkWriteAheadLog();
            }

            public bool AutoFlush { get; set; }

            public int ShrinkWriteAheadLogInterval { get; set; }

            public bool Contains(string key)
            {
                return mDictionary.ContainsKey(key);
            }

            public byte[] this[string key]
            {
                get { return mDictionary[key]; }
                set
                {
                    AppendWriteAheadLog('U', key, value);
                    mDictionary[key] = value;
                }
            }

            public void Add(string key, byte[] bytes)
            {
                AppendWriteAheadLog('A', key, bytes);
                mDictionary.Add(key, bytes);
            }

            public void Remove(string key)
            {
                AppendWriteAheadLog('R', key, null);
                mDictionary.Remove(key);
            }

            public IEnumerator<KeyValuePair<string, byte[]>> GetEnumerator()
            {
                var version = mVersion;
                foreach (var kv in mDictionary)
                {
                    if (version != mVersion)
                        throw new InvalidOperationException();

                    yield return kv;
                }
            }

            IEnumerator IEnumerable.GetEnumerator()
            {
                return GetEnumerator();
            }

            private static Dictionary<string, byte[]> ReplayWriteAheadLog(string path)
            {
                var dict = new Dictionary<string, byte[]>();
                using (var reader = new BinaryReader(File.OpenRead(path)))
                {
                    try
                    {
                        while (reader.BaseStream.Position < reader.BaseStream.Length)
                        {
                            switch (reader.ReadChar())
                            {
                                case 'A':
                                    {
                                        var key = reader.ReadString();
                                        var bytes = reader.ReadBytes(reader.ReadInt32());
                                        dict.Add(key, bytes);
                                    }
                                    break;
                                case 'U':
                                    {
                                        var key = reader.ReadString();
                                        var bytes = reader.ReadBytes(reader.ReadInt32());
                                        dict[key] = bytes;
                                    }
                                    break;
                                case 'R':
                                    dict.Remove(reader.ReadString());
                                    break;
                            }
                        }
                    }
                    catch (EndOfStreamException)
                    {
                    }
                }
                return dict;
            }

            private void AppendWriteAheadLog(char action, string key, byte[] bytes)
            {
                mWriteAheadLogWriter.Write(action);
                mWriteAheadLogWriter.Write(key);
                if (bytes != null)
                {
                    mWriteAheadLogWriter.Write(bytes.Length);
                    mWriteAheadLogWriter.Write(bytes);
                }

                if (AutoFlush) mWriteAheadLogWriter.Flush();

                if (++mVersion > ShrinkWriteAheadLogInterval)
                {
                    ShrinkWriteAheadLog();
                    mVersion = 0;
                }
            }

            public void Flush()
            {
                mWriteAheadLogWriter.Flush();
            }

            public void ShrinkWriteAheadLog()
            {
                var tempWriteAheadLogPath = mWriteAheadLogPath + "_";
                if (File.Exists(tempWriteAheadLogPath))
                    File.Delete(tempWriteAheadLogPath);

                using (var writer = new BinaryWriter(File.OpenWrite(tempWriteAheadLogPath)))
                {
                    foreach (var kv in mDictionary)
                    {
                        writer.Write('A');
                        writer.Write(kv.Key);
                        writer.Write(kv.Value.Length);
                        writer.Write(kv.Value);
                    }
                }

                if (mWriteAheadLogWriter != null)
                {
                    mWriteAheadLogWriter.Dispose();
                    mWriteAheadLogWriter = null;
                }

                if (File.Exists(mWriteAheadLogPath))
                    File.Delete(mWriteAheadLogPath);

                File.Move(tempWriteAheadLogPath, mWriteAheadLogPath);

                mWriteAheadLogWriter = new BinaryWriter(File.OpenWrite(mWriteAheadLogPath));
                mWriteAheadLogWriter.Seek(0, SeekOrigin.End);
            }

            public void Dispose()
            {
                if (mWriteAheadLogWriter != null)
                {
                    ShrinkWriteAheadLog();

                    mWriteAheadLogWriter.Dispose();
                    mWriteAheadLogWriter = null;
                }
            }
        }

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
                                throw new NotSupportedException("Unsupport command");
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
                    switch (random.Next(3))
                    {
                        case 0:
                            File.WriteAllBytes(path, payload);
                            break;
                        case 1:
                            if (File.Exists(path)) File.Delete(path);
                            break;
                        case 2:
                            if (File.Exists(path)) File.ReadAllText(path);
                            break;
                    }
                }
            });

            Utility.Timeit("PersistentDictionary", 1, () =>
            {
                var random = new Random();
                using (var dict = new PersistentDictionary("1.txt"))
                {
                    for (var i = 0; i < 40000; ++i)
                    {
                        var key = string.Format("{0}/{1}.txt", dir, random.Next(1000));
                        switch (random.Next(3))
                        {
                            case 0:
                                if (dict.Contains(key)) dict[key] = payload;
                                else dict.Add(key, payload);
                                break;
                            case 1:
                                if (dict.Contains(key)) dict.Remove(key);
                                break;
                            case 2:
                                if (dict.Contains(key))
                                {
                                    var v = dict[key];
                                }
                                break;
                        }
                    }
                }
            });
        }

        private static void Main(string[] args)
        {
            Benchmark();
        }
    }
}
