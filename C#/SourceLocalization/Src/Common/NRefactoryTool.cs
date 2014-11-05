using System;
using System.Collections.Generic;
using System.Linq;
using ICSharpCode.NRefactory.CSharp;
using ICSharpCode.NRefactory.CSharp.Refactoring;
using ICSharpCode.NRefactory.Editor;

namespace NRefactoryTool
{
    public static class Utils
    {
        public static string EscapeString(string s)
        {
            return new PrimitiveExpression(s).ToString();
        }
        public static string UnescapeString(string s)
        {
            return (new CSharpParser().ParseExpression(s) as PrimitiveExpression).Value as string;
        }
    }

    public static class CodeTransform
    {
        public static string ReplaceStringLiteralsWithVariables(string text, IEnumerable<string> macros, Func<int, int, string, string, string> onReplace)
        {
            var setting = new CompilerSettings();
            foreach (var macro in macros) setting.ConditionalSymbols.Add(macro);

            var tree = SyntaxTree.Parse(text, string.Empty, setting);
            tree.Freeze();

            var doc = new StringBuilderDocument(text);
            using (var editor = new DocumentScript(doc, FormattingOptionsFactory.CreateAllman(), TextEditorOptions.Default)) {
                var originDoc = editor.OriginalDocument;

                foreach (var node in tree.Descendants.OfType<PrimitiveExpression>().Where(e => e.Value is string)) {
                    var line = originDoc.GetLineByNumber(node.StartLocation.Line);
                    var result = onReplace(node.StartLocation.Line, node.StartLocation.Column, originDoc.GetText(line), node.Value as string);
                    if (result != null) {
                        var names = result.Split('.');
                        Expression exp = new IdentifierExpression(names.First());
                        foreach (var name in names.Skip(1)) exp = exp.Member(name);

                        editor.Replace(node, exp);
                    }
                }
            }

            return doc.Text;
        }
    }
}
