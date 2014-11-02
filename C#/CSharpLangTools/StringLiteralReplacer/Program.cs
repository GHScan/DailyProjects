using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;
using System.Diagnostics;
using System.Reflection;

namespace CSharp13 {
    public class Program {
        public static void Main(string[] args) {
            // FindOutStringLiterals("Filter.txt", new []{@"F:\Projects\ConsoleApplication1\ConsoleApplication1\Program.cs"}, new string[0]);
            // ReplaceStringLiteralsWithVariables("Filter.txt", @"F:\Projects\ConsoleApplication1\ConsoleApplication1\Localization.cs", new[] { @"F:\Projects\ConsoleApplication1\ConsoleApplication1\Program.cs" }, new string[0]);
        }

        public static void FindOutStringLiterals(string resultPath, IEnumerable<string> sourcePaths, IEnumerable<string> macros) {
            using (var resultFile = new StreamWriter(new BufferedStream(File.Create(resultPath)), Encoding.Default)) {
                foreach (var path in sourcePaths) {
                    RoslynTool.CodeTransform.ReplaceStringLiteralsWithVariables(File.ReadAllText(path, Encoding.Default), macros,
                    (line, column, s) => {
                        resultFile.WriteLine(string.Format("{0}({1},{2}):{3}", path, line, column, s));
                        return null;
                    });
                }
            }
        }
        public static void ReplaceStringLiteralsWithVariables(string filterPath, string varSourcePath, IEnumerable<string> sourcePaths, IEnumerable<string> macros) {
            string varClassName = Path.GetFileNameWithoutExtension(varSourcePath);
            var filter = new HashSet<string>(File.ReadAllLines(filterPath, Encoding.Default));
            var str2VarName = new Dictionary<string, string>(1024);

            foreach (var path in sourcePaths) {
                var oldText = File.ReadAllText(path, Encoding.Default);
                var newText = RoslynTool.CodeTransform.ReplaceStringLiteralsWithVariables(oldText, macros,
                (line, column, s) => {
                    if (!filter.Contains(string.Format("{0}({1},{2}):{3}", path, line, column, s))) return null;

                    string varName;
                    if (str2VarName.TryGetValue(s, out varName)) return varName;

                    varName = string.Format("{0}.kString_{1}", varClassName, str2VarName.Count);
                    str2VarName.Add(s, varName);
                    return varName;
                });
                if (oldText != newText) File.WriteAllText(path, newText, Encoding.Default);
            }

            using (var varFile = new StreamWriter(File.Create(varSourcePath, 4 * 1024), Encoding.Default)) {
                varFile.WriteLine(string.Format("public static class {0} {{", varClassName));
                foreach (var kv in str2VarName) {
                    varFile.WriteLine("\tpublic const string {0} = {1};", kv.Value.Split('.').Last(), kv.Key);
                }
                varFile.WriteLine("}");
            }
        }
    }
}

namespace RoslynTool {
    using Microsoft.CodeAnalysis;
    using Microsoft.CodeAnalysis.CSharp;
    using Microsoft.CodeAnalysis.CSharp.Syntax;
    public static class Utils {
        public static string EscapeString(string s) {
            return SymbolDisplay.FormatLiteral(s, true);
        }
        public static string UnescapeString(string s) {
            return (SyntaxFactory.ParseExpression(s) as LiteralExpressionSyntax).Token.ValueText;
        }
    }

    class StringLiteralToVariableRewriter: CSharpSyntaxRewriter {
        public Func<int, int, string, string> OnRewrite;
        public override SyntaxNode VisitLiteralExpression(LiteralExpressionSyntax node) {
            if (node.CSharpKind() != SyntaxKind.StringLiteralExpression) return node;

            var pos = node.GetLocation().GetMappedLineSpan();
            var result = OnRewrite(pos.StartLinePosition.Line + 1, pos.StartLinePosition.Character + 1, Utils.EscapeString(node.Token.ValueText));
            if (result == null) return node;

            ExpressionSyntax newNode = null;
            foreach (var name in result.Split('.')) {
                if (newNode == null) newNode = SyntaxFactory.IdentifierName(name);
                else newNode = SyntaxFactory.MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, newNode, SyntaxFactory.IdentifierName(name));
            }

            return newNode.WithLeadingTrivia(node.GetLeadingTrivia()).WithTrailingTrivia(node.GetTrailingTrivia());
        }
    }

    public static class CodeTransform {
        public static string ReplaceStringLiteralsWithVariables(string text, IEnumerable<string> macros, Func<int, int, string, string> onReplace) {
            var tree = CSharpSyntaxTree.ParseText(text, new CSharpParseOptions(preprocessorSymbols : macros));
            var newRoot = new StringLiteralToVariableRewriter() { OnRewrite = onReplace }.Visit(tree.GetRoot());
            return tree.WithRootAndOptions(newRoot, tree.Options).ToString();
        }
    }
}