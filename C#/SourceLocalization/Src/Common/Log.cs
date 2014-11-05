using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;
using System.Diagnostics;

public static class Log
{
    public static void Info(object o)
    {
        Info(o.ToString());
    }
    public static void Info(string format, params object[] args)
    {
        Info(string.Format(format, args));
    }
    public static void Info(string s)
    {
        Console.WriteLine(s);
    }
    public static void Error(object o)
    {
        Error(o.ToString());
    }
    public static void Error(string format, params object[] args)
    {
        Error(string.Format(format, args));
    }
    public static void Error(string s)
    {
        var color = Console.ForegroundColor;
        Console.ForegroundColor = ConsoleColor.Red;
        Console.WriteLine(s);
        Console.ForegroundColor = color;
    }
}
