using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;
using System.Diagnostics;

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

    public static Encoding DetectFileEncoding(string path)
    {
        byte[] bytes = new byte[4];
        using (var file = File.Open(path, FileMode.Open))
        {
            if (file.Read(bytes, 0, 4) < 4) return Encoding.Default;
        }
        if (bytes[0] == 0xEF && bytes[1] == 0xBB && bytes[2] == 0xBF) return Encoding.UTF8;
        if (bytes[0] == 0xFE && bytes[1] == 0xFF) return Encoding.BigEndianUnicode;
        if (bytes[0] == 0xFF && bytes[1] == 0xFE) return Encoding.Unicode;
        if (bytes[0] == 0x00 && bytes[1] == 0x00 && bytes[2] == 0xFE && bytes[3] == 0xFF) return Encoding.GetEncoding(12001);
        if (bytes[0] == 0xFF && bytes[1] == 0xFE && bytes[2] == 0x00 && bytes[3] == 0x00) return Encoding.UTF32;
        return Encoding.Default;
    }
}