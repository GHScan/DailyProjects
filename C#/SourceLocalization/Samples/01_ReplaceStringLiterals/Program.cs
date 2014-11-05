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
    const string constChinese = "���";
    const string constChinese2 = "���";
    static readonly string constMultilineChinese = @"���
��������
    \r\n
";

    static void Main(string[] args)
    {
        Console.WriteLine("Argument count: " + args.Length);
        Console.WriteLine("��������: " + args.Length);

        Console.WriteLine("\tformat: {0}\n {1}\n {2}\n", constEnglish, constChinese2, "��ʽ");
        Console.WriteLine("\t��ʽ: {0}\n {1}\n {2}\n", constEnglish, constChinese2, "format");

        var multilineChinese = @"����һ��
    ����
�����ı�";
        var multilineChinese2 = "����һ��\n" +
       "����" +
        "�����ı�2";

        Console.WriteLine(File.ReadAllText(@"E:\ejoy\act\tool\Localization\Src\CSharp_StringLiteralReplace_Test\Program.cs", Encoding.Default));
    }
}
