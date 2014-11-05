using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;
using System.Diagnostics;
using System.Reflection;

public class Program
{
    public const string kNameKey = "Name";
    public const string kDefaultLangKey = "zh_cn";

    public static IEnumerable<string> GetCSharpFiles(IEnumerable<string> directories)
    {
        return from dir in directories
               from path in Directory.GetFiles(dir, "*.cs", SearchOption.AllDirectories)
               select path;
    }

    public class Command_FindoutStringLiterals : Command
    {
        public List<string> Macros;
        public List<string> CSharpSrcDirectories;
        public string ResultPath;
        public static Command_FindoutStringLiterals CreateTemplate()
        {
            return new Command_FindoutStringLiterals()
            {
                Macros = new List<string>() { "Macro1", },
                CSharpSrcDirectories = new List<string>() { "Test.cs", },
                ResultPath = "Filter.txt",
            };
        }
        public override void Execute()
        {
            Log.Info("Start to find out string literals...");

            using (var resultFile = new StreamWriter(new BufferedStream(File.Create(ResultPath)), Encoding.UTF8))
            {
                foreach (var path in GetCSharpFiles(CSharpSrcDirectories))
                {
                    Log.Info("\tProcessing file: {0}", path);

                    var encoding = Utils.DetectFileEncoding(path);
                    RoslynTool.CodeTransform.ReplaceStringLiteralsWithVariables(File.ReadAllText(path, encoding), Macros,
                    (line, column, lineText, s) =>
                    {
                        resultFile.WriteLine(string.Format("{0}({1},{2}):{3}", path, line, column, lineText));
                        return null;
                    });
                }
            }

            Log.Info("Done.");
        }
    }
    public class Command_ReplaceStringLiteralsWithVariables : Command
    {
        public List<string> Macros;
        public List<string> CSharpSrcDirectories;
        public string FilterPath;
        public string ResultPath;
        public static Command_ReplaceStringLiteralsWithVariables CreateTemplate()
        {
            return new Command_ReplaceStringLiteralsWithVariables()
            {
                Macros = new List<string>() { "Macro1", },
                CSharpSrcDirectories = new List<string>() { "Test.cs", },
                FilterPath = "Filter.txt",
                ResultPath = "Localization.json",
            };
        }
        public override void Execute()
        {
            Log.Info("Start to replace string literals ...");

            Log.Info("Read filter from: {0}", FilterPath);

            string className = Path.GetFileNameWithoutExtension(ResultPath);
            var filter = new HashSet<string>(File.ReadAllLines(FilterPath, Encoding.UTF8));
            var str2Field = new Dictionary<string, string>(1024);

            foreach (var path in GetCSharpFiles(CSharpSrcDirectories))
            {
                Log.Info("\tProcessing file: {0}", path);

                var encoding = Utils.DetectFileEncoding(path);
                var oldText = File.ReadAllText(path, encoding);
                var newText = RoslynTool.CodeTransform.ReplaceStringLiteralsWithVariables(oldText, Macros,
                (line, column, lineText, s) =>
                {
                    if (!filter.Contains(string.Format("{0}({1},{2}):{3}", path, line, column, lineText))) return null;

                    string field;
                    if (str2Field.TryGetValue(s, out field)) return field;

                    field = string.Format("{0}.kString_{1}", className, str2Field.Count);
                    str2Field.Add(s, field);
                    return field;
                });

                if (oldText != newText)
                {
                    Log.Info("\tPerform replacement!");
                    File.WriteAllText(path, newText, encoding);
                }
            }

            Log.Info("Write sheet to: {0}", ResultPath);

            var sheet = new DataSheet
            {
                Name = className,
                Rows = (from kv in str2Field
                        let fieldName = kv.Value.Split('.').Last()
                        orderby fieldName
                        select new Dictionary<string, object>(){
                              {kNameKey, fieldName },
                              {kDefaultLangKey, kv.Key}
                          }).ToList(),
            };
            File.WriteAllText(ResultPath, JsonOperation.SerializeSheets(new[] { sheet }), Encoding.UTF8);

            Log.Info("Done.");
        }
    }
    public class Command_GenerateClassFile : Command
    {
        public string JsonPath;
        public string Language;
        public string ResultPath;
        public static Command_GenerateClassFile CreateTemplate()
        {
            return new Command_GenerateClassFile()
            {
                JsonPath = "Localization.json",
                Language = kDefaultLangKey,
                ResultPath = "Localization.cs",
            };
        }
        public override void Execute()
        {
            Log.Info("Start to generate localization class...");

            Log.Info("Read sheets from: {0}", JsonPath);

            var sheet = JsonOperation.DeserializeSheets(File.ReadAllText(JsonPath, Encoding.UTF8))[0];

            using (var outputFile = new StreamWriter(new BufferedStream(File.Create(ResultPath)), Encoding.UTF8))
            {
                outputFile.WriteLine("public static class {0} ", sheet.Name);
                outputFile.WriteLine("{");
                foreach (var row in sheet.Rows)
                {
                    outputFile.WriteLine(string.Format("\tpublic const string {0} = \"{1}\";", row[kNameKey], row[Language]));
                }
                outputFile.WriteLine("}");
            }

            Log.Info("Done.");
        }
    }

    public static void Main(string[] args)
    {
        Command.RunMain(args);
    }
}
