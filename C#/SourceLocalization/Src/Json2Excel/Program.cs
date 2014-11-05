using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.IO;

public class Program
{
    public class Command_Excel2Json : Command
    {
        public string ExcelPath;
        public int ExcelTitleRow;
        public int ExcelContentRow;
        public List<string> ExcelSheetNames;
        public string JsonPath;
        public static Command_Excel2Json CreateTemplate()
        {
            return new Command_Excel2Json()
            {
                ExcelPath = "1.xls",
                ExcelTitleRow = 1,
                ExcelContentRow = 2,
                ExcelSheetNames = new List<string> { "Sheet1" },
                JsonPath = "1.json",
            };
        }
        public override void Execute()
        {
            Log.Info("Start to export excel to json...");

            using (var reader = new ExcelOperation.ExcelReader(ExcelPath, ExcelTitleRow, ExcelContentRow))
            {
                var sheets = new List<DataSheet>();
                foreach (var sheetName in ExcelSheetNames)
                {
                    Log.Info("Read sheet: {0}", sheetName);
                    sheets.Add(reader.Read(sheetName));
                }

                Log.Info("Write sheets to: {0}", JsonPath);
                File.WriteAllText(JsonPath, JsonOperation.SerializeSheets(sheets), Encoding.UTF8);
            }

            Log.Info("Done.");
        }
    }

    public class Command_Json2Excel : Command
    {
        public string JsonPath;
        public string ExcelPath;
        public int ExcelTitleRow;
        public int ExcelContentRow;
        public static Command_Json2Excel CreateTemplate()
        {
            return new Command_Json2Excel()
            {
                ExcelPath = "1.xls",
                ExcelTitleRow = 1,
                ExcelContentRow = 2,
                JsonPath = "1.json",
            };
        }
        public override void Execute()
        {
            Log.Info("Start to import json to excel...");

            using (var writer = new ExcelOperation.ExcelWriter(ExcelPath, ExcelTitleRow, ExcelContentRow))
            {
                Log.Info("Read sheets from: {0}", JsonPath);
                var sheets = JsonOperation.DeserializeSheets(File.ReadAllText(JsonPath, Encoding.UTF8));

                foreach (var sheet in sheets)
                {
                    Log.Info("Write sheet : {0}", sheet.Name);
                    writer.Write(sheet);
                }
            }

            Log.Info("Done.");
        }
    }

    static void Main(string[] args)
    {
        Command.RunMain(args);
    }
}