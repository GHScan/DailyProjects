#define USE_MONO_CECIL
// #define USE_NREFACTORY
// #define USE_ROSLYN

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;
using System.Diagnostics;
using System.Reflection;
using System.Text.RegularExpressions;

namespace CSharp13
{
    public class Program
    {
        public static void Main(string[] args)
        {
            using (var output = new StreamWriter(new BufferedStream(File.Create("ChineseStr.txt"), 16 * 1024), Encoding.Default))
            {
                FindOutChineseStringInAssemblys(
                    new[] { @"E:\ejoy\act\Game\ActProject\Library\ScriptAssemblies\Assembly-CSharp.dll" },
                    output.WriteLine);

                FindOutChineseStringInSources(
                    Directory.EnumerateFiles(@"E:\ejoy\act", "*.cs", SearchOption.AllDirectories),
                    new[] { "MyGame", "A1_SCRIPT_IMUPDATABLE", "A1_NONPLATFORM" },
                    output.WriteLine);
            }
        }

        public static void FindOutChineseStringInAssemblys(IEnumerable<string> assemblys, Action<string> callback)
        {
#if USE_MONO_CECIL
            foreach (var assemblyLoc in assemblys)
            {
                MonoCecilTool.AssemblyAnalysis.EnumStringLiterals(assemblyLoc, (fname, sline, scolumn, eline, ecolumn, str) =>
                {
                    if (sline != -1 && str.Any(c => (int)c > 127))
                    {
                        callback(string.Format("{0}({1},{2}): {3}", fname, sline, scolumn, str));
                    }
                });
            }
#endif
        }
        public static void FindOutChineseStringInSources(IEnumerable<string> sourceFiles, IEnumerable<string> macros, Action<string> callback)
        {
#if USE_NREFACTORY || USE_ROSLYN
            Action<string, int, int, int, int, string> callback2 = (fname, sline, scolumn, eline, ecolumn, str) =>
            {
                if (sline != -1 && str.Any(c => (int)c > 127))
                {
                    callback(string.Format("{0}({1},{2}): {3}", fname, sline, scolumn, str));
                }
            };

            foreach (var fname in sourceFiles)
            {
                try
                {
#if USE_NREFACTORY
                    NRefactoryTool.CodeAnalysis.EnumStringLiterals(fname, macros, callback2);
#else
                    RoslynTool.CodeAnalysis.EnumStringLiterals(fname, macros, callback2);
#endif
                }
                catch(Exception e)
                {
                    Console.Error.WriteLine(e);
                }
            }
#endif
        }
    }
}

#if USE_MONO_CECIL
namespace MonoCecilTool
{
    using Mono.Cecil;
    using Mono.Cecil.Cil;
    using Mono.Cecil.Mdb;
    // using Mono.Cecil.Pdb;

    public static class AssemblyAnalysis
    {
        public static void EnumStringLiterals(string assemblyLocation, Action<string, int, int, int, int, string> callback)
        {
            var assembly = AssemblyDefinition.ReadAssembly(assemblyLocation,
                new ReaderParameters()
                {
                    SymbolReaderProvider = (ISymbolReaderProvider)Activator.CreateInstance(typeof(MdbReaderProvider)),
                    // SymbolReaderProvider = (ISymbolReaderProvider)Activator.CreateInstance(typeof(PdbReaderProvider)),
                    ReadingMode = ReadingMode.Deferred,
                });

            var instructions = from module in assembly.Modules
                               from type in module.Types
                               from method in type.Methods
                               where method.Body != null
                               from ins in method.Body.Instructions
                               select ins;
            foreach (var ins in instructions)
            {
                if (ins.OpCode != OpCodes.Ldstr) continue;

                SequencePoint sp;
                {
                    int backStep = 0;
                    var tins = ins;
                    while (tins.SequencePoint == null && tins.Previous != null)
                    {
                        tins = tins.Previous;
                        if (++backStep > 64) break;
                    }
                    sp = tins.SequencePoint;
                }

                if (sp != null)
                {
                    callback(sp.Document.Url, sp.StartLine, sp.StartColumn, sp.EndLine, sp.EndColumn, ins.Operand.ToString());
                }
                else
                {
                    callback("?", -1, -1, -1, -1, ins.Operand.ToString());
                }
            }
        }
    }
}

namespace TestMonoCecilTool
{
    public static class TestAssemblyAnalysis
    {
        public static void TestEnumStringLiterals()
        {
            MonoCecilTool.AssemblyAnalysis.EnumStringLiterals(Assembly.GetCallingAssembly().Location,
            (fname, sline, scolumn, eline, ecolumn, str) =>
            {
                if (sline == -1) return;
                Console.WriteLine("{0}({1},{2}): {3}", fname, sline, scolumn, str);
            });
        }
    }
}
#endif

#if USE_NREFACTORY
namespace NRefactoryTool
{
    using ICSharpCode.NRefactory.CSharp;
    public static class CodeAnalysis
    {
        public static void EnumStringLiterals(string fileName, IEnumerable<string> macros, Action<string, int, int, int, int, string> callback)
        {
            var setting = new CompilerSettings();
            foreach (var macro in macros) setting.ConditionalSymbols.Add(macro);

            var parser = new CSharpParser(setting);
            var tree = parser.Parse(File.ReadAllText(fileName, Encoding.Default), fileName);
            if (tree.Errors.Count > 0)
            {
                throw new Exception("Compile failed: " + string.Join("\n", tree.Errors.Select(e => e.Message)));
            }

            foreach (var node in tree.Descendants.OfType<PrimitiveExpression>().Where(n => n.Value is string))
            {
                callback(fileName, node.StartLocation.Line, node.StartLocation.Column, node.EndLocation.Line, node.EndLocation.Column, node.Value as string);
            }
        }
    }
}

namespace TestNRefactoryTool
{
    public static class TestCodeAnalysis
    {
        public static void TestEnumStringLiterals()
        {
            NRefactoryTool.CodeAnalysis.EnumStringLiterals(new StackFrame(true).GetFileName(), Enumerable.Empty<string>(),
           (fname, sline, scolumn, eline, ecolumn, str) =>
           {
               Console.WriteLine("{0}({1},{2}): {3}", fname, sline, scolumn, str);
           });
        }
    }
}
#endif

#if USE_ROSLYN
namespace RoslynTool
{
    using Microsoft.CodeAnalysis;
    using Microsoft.CodeAnalysis.CSharp;
    using Microsoft.CodeAnalysis.CSharp.Syntax;
    public static class CodeAnalysis
    {
        public static void EnumStringLiterals(string fileName, IEnumerable<string> macros, Action<string, int, int, int, int, string> callback)
        {
            var tree = CSharpSyntaxTree.ParseText(File.ReadAllText(fileName, Encoding.Default), new CSharpParseOptions(preprocessorSymbols: macros), fileName);
            var root = tree.GetRoot() as CompilationUnitSyntax;
            foreach (var node in root.DescendantNodes().OfType<LiteralExpressionSyntax>().Where(n => n.CSharpKind() == SyntaxKind.StringLiteralExpression))
            {
                var pos = node.Token.GetLocation().GetMappedLineSpan();
                callback(
                    pos.Path,
                    pos.StartLinePosition.Line + 1, pos.StartLinePosition.Character + 1, pos.EndLinePosition.Line + 1, pos.EndLinePosition.Character + 1,
                    node.Token.Value as string);
            }
        }
    }
}

namespace TestRoslynTool
{
    public static class TestCodeAnalysis
    {
        public static void TestEnumStringLiterals()
        {
            RoslynTool.CodeAnalysis.EnumStringLiterals(new StackFrame(true).GetFileName(), Enumerable.Empty<string>(),
           (fname, sline, scolumn, eline, ecolumn, str) =>
           {
               Console.WriteLine("{0}({1},{2}): {3}", fname, sline, scolumn, str);
           });
        }
    }
}
#endif