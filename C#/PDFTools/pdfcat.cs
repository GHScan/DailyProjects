using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using PdfSharp.Drawing;
using PdfSharp.Pdf;
using PdfSharp.Pdf.IO;

namespace CSharp2015
{
    public class Program
    {
        private static void MergePdfs(string outputPath, IEnumerable<string> pdfPaths)
        {
            using (var outputDoc = new PdfDocument())
            {
                var title2page = new Dictionary<string, int>();

                foreach (var path in pdfPaths)
                {
                    title2page.Add(Path.GetFileNameWithoutExtension(path), outputDoc.PageCount + 2);

                    using (var fileDoc = PdfReader.Open(path, PdfDocumentOpenMode.Import))
                    {
                        var pageCount = fileDoc.PageCount;
                        for (var i = 0; i < pageCount; i++)
                        {
                            outputDoc.AddPage(fileDoc.Pages[i]);
                        }
                    }
                }

                var contentPage = outputDoc.InsertPage(0);

                var fontSize = 50;
                for (; fontSize >= 10; fontSize -= 5)
                {
                    if (RenderContentPage(contentPage, title2page, fontSize))
                        break;
                }
                if (fontSize < 10) throw new InvalidDataException("failed to render content page");

                outputDoc.Save(outputPath);
            }
        }

        private static bool RenderContentPage(PdfPage page, IDictionary<string, int> title2page, int frontSize)
        {
            var font = new XFont("Monaco", frontSize, XFontStyle.Bold, 
                    new XPdfFontOptions(PdfFontEncoding.Unicode, PdfFontEmbedding.Always));
            var brush = XBrushes.Black;
            var pen = new XPen(brush.Color) { DashStyle = XDashStyle.Dot };
            var border = 4;

            using (var g = XGraphics.FromPdfPage(page))
            {
                var maxWidth = .0;
                var totalHeight = .0;
                var height = .0;
                foreach (var kv in title2page)
                {
                    var size = g.MeasureString(kv.Key, font);
                    maxWidth = Math.Max(size.Width, maxWidth);
                    height = size.Height;
                    totalHeight += height + border;
                }

                var x = (page.Width.Point - maxWidth) / 2;
                var y = (page.Height.Point - totalHeight) / 2;
                if (x < 0 || y < 0) return false;

                foreach (var kv in title2page.OrderBy(kv => kv.Key))
                {
                    var rect = new XRect(new XPoint(x, y), new XSize(maxWidth, height));

                    g.DrawString(kv.Key, font, brush, rect, XStringFormats.TopLeft);
                    g.DrawRectangle(pen, rect);
                    page.AddDocumentLink(new PdfRectangle(g.Transformer.WorldToDefaultPage(rect)), kv.Value);

                    y += height + border;
                }
            }

            return true;
        }

        public static void Main(string[] args)
        {
            if (args.Length < 1)
            {
                Console.Error.WriteLine("pdfcat.exe pdfDir");
                return;
            }

            MergePdfs("cat.pdf", Directory.GetFiles(args[0]));
        }
    }
}