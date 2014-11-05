using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.IO;

class Program
{
    const string constEnglish = "Hello";
    const string constMultilineEnglish = @"Hello
            Haha, \t
            \n";
    const string constChinese = "你好";
    const string constChinese2 = "你好";
    static readonly string constMultilineChinese = @"你好
哈哈，，
    \r\n
";

    static void Main(string[] args)
    {
        Console.WriteLine("Argument count: " + args.Length);
        Console.WriteLine("参数个数: " + args.Length);

        Console.WriteLine("\tformat: {0}\n {1}\n {2}\n", constEnglish, constChinese2, "格式");
        Console.WriteLine("\t格式: {0}\n {1}\n {2}\n", constEnglish, constChinese2, "format");

        var multilineChinese = @"这是一条
    多行
中文文本";
        var multilineChinese2 = "这是一条\n" +
       "多行" +
        "中文文本2";

        Console.WriteLine(File.ReadAllText(@"E:\ejoy\act\tool\Localization\Src\CSharp_StringLiteralReplace_Test\Program.cs", Encoding.Default));
    }
}
