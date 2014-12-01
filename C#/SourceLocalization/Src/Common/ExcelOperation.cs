using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.IO;
using NPOI.SS.UserModel;
using NPOI.HSSF.UserModel;

namespace ExcelOperation
{
    public class ExcelReader : IDisposable
    {
        private IWorkbook mWorkbook;
        private int mTitleRow;
        private int mContentRow;
        public ExcelReader(string fileName, int titleRow, int contentRow)
        {
            mTitleRow = titleRow;
            mContentRow = contentRow;
            mWorkbook = WorkbookFactory.Create(fileName);
        }
        public IEnumerable<string> GetSheetNames()
        {
            foreach (ISheet sheet in mWorkbook) yield return sheet.SheetName;
        }
        public DataSheet Read(string name)
        {
            var sheet = mWorkbook.GetSheet(name);

            var dsheet = new DataSheet() { Name = sheet.SheetName };
            var titles = (from cell in sheet.GetRow(mTitleRow).Cells
                          where !string.IsNullOrEmpty(cell.StringCellValue)
                          select new KeyValuePair<int, string>(cell.ColumnIndex, cell.StringCellValue)).ToArray();
            for (int i = mContentRow, last = sheet.LastRowNum; i <= last; ++i)
            {
                var row = sheet.GetRow(i);
                if (row != null) dsheet.Rows.Add(ReadRow(row, titles));
            }
            return dsheet;
        }
        private Dictionary<string, object> ReadRow(IRow row, KeyValuePair<int, string>[] titles)
        {
            var data = new Dictionary<string, object>();
            foreach (var title in titles)
            {
                var cell = row.GetCell(title.Key);
                if (cell == null)
                {
                    data.Add(title.Value, string.Empty);
                    continue;
                }

                switch (cell.CellType)
                {
                    case CellType.Numeric:
                        data.Add(title.Value, cell.NumericCellValue);
                        break;
                    case CellType.String:
                        data.Add(title.Value, cell.StringCellValue);
                        break;
                    case CellType.Blank:
                        data.Add(title.Value, string.Empty);
                        break;
                    default:
                        throw new Exception(string.Format("Invalid excel cell type: {0} ({1}.{2}.{3})", cell.CellType, cell.Row.Sheet.SheetName, cell.RowIndex, cell.ColumnIndex));
                }
            }
            return data;
        }

        public void Dispose() {
            if (mWorkbook == null) return;

            mWorkbook = null;
        }
    }

    public class ExcelWriter : IDisposable
    {
        private IWorkbook mWorkbook;
        private string mFileName;
        private int mTitleRow;
        private int mContentRow;
        public ExcelWriter(string fileName, int titleRow, int contentRow)
        {
            mFileName = fileName;
            mTitleRow = titleRow;
            mContentRow = contentRow;
            mWorkbook = File.Exists(fileName) ? WorkbookFactory.Create(fileName) : new HSSFWorkbook();
        }
        public void Write(DataSheet dsheet)
        {
            if (dsheet.Rows.Count == 0) return;

            var sheet = mWorkbook.GetSheet(dsheet.Name);
            if (sheet == null) sheet = CreateSheet(dsheet);

            var titleRow = sheet.GetRow(mTitleRow);
            foreach (var newTitle in dsheet.Rows[0].Keys.Except(titleRow.Cells.Select(c => c.StringCellValue)))
            {
                titleRow.CreateCell(titleRow.LastCellNum).SetCellValue(newTitle);
            }

            var titles = (from cell in titleRow.Cells
                          where !string.IsNullOrEmpty(cell.StringCellValue)
                          select new KeyValuePair<int, string>(cell.ColumnIndex, cell.StringCellValue)).ToArray();
            for (int i = mContentRow, end = mContentRow + dsheet.Rows.Count; i < end; ++i)
            {
                var row = sheet.GetRow(i);
                if (row == null) row = sheet.CreateRow(i);
                WriteRow(row, dsheet.Rows[i - mContentRow], titles);
            }
            for (int i = mContentRow + dsheet.Rows.Count, last = sheet.LastRowNum; i <= last; ++i)
            {
                var row = sheet.GetRow(i);
                if (row != null) sheet.RemoveRow(row);
            }
        }
        private ISheet CreateSheet(DataSheet dsheet)
        {
            var sheet = mWorkbook.CreateSheet(dsheet.Name);
            for (var i = 0; i < mContentRow; ++i) sheet.CreateRow(i);

            var columnNames = dsheet.Rows[0].Keys.OrderBy(k => k).ToArray();
            var titleRow = sheet.GetRow(mTitleRow);
            for (var i = 0; i < columnNames.Length; ++i)
            {
                titleRow.CreateCell(i).SetCellValue(columnNames[i]);
            }

            return sheet;
        }
        private void WriteRow(IRow row, Dictionary<string, object> data, KeyValuePair<int, string>[] titles)
        {
            foreach (var title in titles)
            {
                object value;
                if (!data.TryGetValue(title.Value, out value)) continue;

                var cell = row.GetCell(title.Key);
                if (cell == null) cell = row.CreateCell(title.Key);

                if (value is string)
                {
                    cell.SetCellValue(value as string);
                }
                else
                {
                    cell.SetCellValue(Convert.ToDouble(value));
                }
            }
        }
        public void Save() {
            using (var fs = new BufferedStream(File.OpenWrite(mFileName))) {
                mWorkbook.Write(fs);
            }
        }

        public void Dispose()
        {
            if (mWorkbook == null) return;

            mWorkbook = null;
        }
    }
}
