using System.IO;
using System.Linq;
using System.Collections.Generic;

namespace Tool
{
    public static class TextFile
    {
        public static void Sort(string inputFilePath, string outputFilePath, int bufferSize)
        {
            var subFilePaths = new List<string>();

            try
            {
                using (var inputFile = File.OpenText(inputFilePath))
                {
                    var bufferLines = new List<string>();
                    var currentSize = 0;

                    string line;
                    do
                    {
                        line = inputFile.ReadLine();
                        if (line != null)
                        {
                            bufferLines.Add(line);
                            currentSize += line.Length * 2;
                        }

                        if (line == null || currentSize > bufferSize)
                        {
                            var subFilePath = string.Format("{0}_{1}", inputFilePath, subFilePaths.Count);
                            bufferLines.Sort(string.CompareOrdinal);
                            File.WriteAllLines(subFilePath, bufferLines);
                            subFilePaths.Add(subFilePath);

                            bufferLines.Clear();
                            currentSize = 0;
                        }

                    } while (line != null);
                }


                using (var outputFile = File.CreateText(outputFilePath))
                {
                    var subFiles = subFilePaths.Select(File.OpenText).ToList();
                    var currentLines = subFiles.Select(file => file.ReadLine()).ToList();

                    for (var i = subFiles.Count - 1; i >= 0; --i)
                    {
                        if (currentLines[i] == null)
                        {
                            currentLines.RemoveAt(i);
                            subFiles[i].Dispose();
                            subFiles.RemoveAt(i);
                        }
                    }

                    while (subFiles.Count > 0)
                    {
                        var miniIndex = 0;
                        for (var i = 1; i < currentLines.Count; ++i)
                        {
                            if (string.CompareOrdinal(currentLines[i], currentLines[miniIndex]) < 0)
                                miniIndex = i;
                        }

                        outputFile.WriteLine(currentLines[miniIndex]);

                        currentLines[miniIndex] = subFiles[miniIndex].ReadLine();
                        if (currentLines[miniIndex] == null)
                        {
                            currentLines.RemoveAt(miniIndex);
                            subFiles[miniIndex].Dispose();
                            subFiles.RemoveAt(miniIndex);
                        }
                    }
                }
            }
            finally
            {
                foreach (var subFilePath in subFilePaths)
                    File.Delete(subFilePath);
            }
        }

        public static int Compare(string filePath1, string filePath2)
        {
            using (var file1 = File.OpenText(filePath1))
            using (var file2 = File.OpenText(filePath2))
            {
                var line1 = file1.ReadLine();
                var line2 = file2.ReadLine();
                while (line1 != null && line2 != null && string.CompareOrdinal(line1, line2) == 0)
                {
                    line1 = file1.ReadLine();
                    line2 = file2.ReadLine();
                }

                return string.CompareOrdinal(line1, line2);
            }
        }

        public static int OrderIgnoredCompare(string filePath1, string filePath2, int bufferSize)
        {
            var orderedFilePath1 = filePath1 + "_ordered";
            var orderedFilePath2 = filePath2 + "_ordered";

            if (File.Exists(orderedFilePath1))
                throw new IOException(string.Format("Ordered file already exists: {0}", orderedFilePath1));
            if (File.Exists(orderedFilePath2))
                throw new IOException(string.Format("Ordered file already exists: {0}", orderedFilePath2));

            try
            {
                Sort(filePath1, orderedFilePath1, bufferSize);
                Sort(filePath2, orderedFilePath2, bufferSize);

                return Compare(orderedFilePath1, orderedFilePath2);
            }
            finally
            {
                if (File.Exists(orderedFilePath1))
                    File.Delete(orderedFilePath1);

                if (File.Exists(orderedFilePath2))
                    File.Delete(orderedFilePath2);
            }
        }
    }
}
