using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.IO;

class Program
{
    public class Command_JsonMerge : Command
    {
        public string SrcPath;
        public string DestPath;
        public Dictionary<string, string> SheetsPrimaryKey;

        public static Command_JsonMerge CreateTemplate()
        {
            return new Command_JsonMerge()
            {
                SrcPath = "1.json",
                DestPath = "2.json",
                SheetsPrimaryKey = new Dictionary<string, string> { { "Sheet1", "Name" } },
            };
        }
        public override void Execute()
        {
            Log.Info("Start to merge json ...");

            Log.Info("Read src sheets: {0}", SrcPath);
            Log.Info("Read dest sheets: {0}", DestPath);

            var srcSheets = JsonOperation.DeserializeSheets(File.ReadAllText(SrcPath, Encoding.UTF8));
            var destSheets = JsonOperation.DeserializeSheets(File.ReadAllText(DestPath, Encoding.UTF8));

            foreach (var kv in SheetsPrimaryKey)
            {
                var destSheet = destSheets.Find(s => s.Name == kv.Key);
                var srcSheet = srcSheets.Find(s => s.Name == kv.Key);
                if (destSheet == null || srcSheet == null) throw new Exception("Invalid sheet name :" + kv.Key);
                if (destSheet.Rows.Count == 0 || srcSheet.Rows.Count == 0) continue;

                Log.Info("Merge sheet: {0}", kv.Key);
                MergeSheet(destSheet, srcSheet, kv.Value);
            }

            Log.Info("Write merged sheets to: {0}", DestPath);

            File.WriteAllText(DestPath, JsonOperation.SerializeSheets(destSheets), Encoding.UTF8);

            Log.Info("Done.");
        }
        private void MergeSheet(DataSheet destSheet, DataSheet srcSheet, string primaryKey)
        {
            CheckSheetConsistence(destSheet);
            CheckSheetConsistence(srcSheet);

            EnsureSheetHaveColumns(destSheet, srcSheet.Rows[0].Keys);

            var id2DestRow = new Dictionary<object, Dictionary<string, object>>();
            foreach (var destRow in destSheet.Rows) id2DestRow[destRow[primaryKey]] = destRow;

            foreach (var srcRow in srcSheet.Rows)
            {
                var id = srcRow[primaryKey];
                if (!id2DestRow.ContainsKey(id))
                {
                    throw new Exception(string.Format("Can't find primary key in dest sheet: {0}", id));
                }

                Dictionary<string, object> destRow = id2DestRow[srcRow[primaryKey]];
                foreach (var kv in srcRow)
                {
                    destRow[kv.Key] = kv.Value;
                }
            }
        }
        private void CheckSheetConsistence(DataSheet sheet)
        {
            var columnNames = new HashSet<string>(sheet.Rows[0].Keys);
            foreach (var row in sheet.Rows)
            {
                if (!columnNames.SetEquals(row.Keys))
                {
                    throw new Exception(
                        string.Format("{0} columns mismatch: ({1}),({2})",
                        sheet.Name, JsonOperation.Serialize(columnNames), JsonOperation.Serialize(row)));
                }
            }
        }
        private void EnsureSheetHaveColumns(DataSheet sheet, IEnumerable<string> columnNames)
        {
            var newNames = columnNames.Except(sheet.Rows[0].Keys);
            foreach (var name in newNames)
            {
                foreach (var row in sheet.Rows) row.Add(name, string.Empty);
            }
        }
    }

    static void Main(string[] args)
    {
        Command.RunMain(args);
    }
}