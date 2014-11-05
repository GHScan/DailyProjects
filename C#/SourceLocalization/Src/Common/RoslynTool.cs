using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;

namespace RoslynTool
{
    public static class Utils
    {
        public static string EscapeString(string s)
        {
            return SymbolDisplay.FormatLiteral(s, false);
        }
        public static string UnescapeString(string s)
        {
            return (SyntaxFactory.ParseExpression(s) as LiteralExpressionSyntax).Token.ValueText;
        }
    }

    class StringLiteralToVariableRewriter : CSharpSyntaxRewriter
    {
        public Func<int, int, string, string, string> OnRewrite;
        public SourceText Source;
        public override SyntaxNode VisitLiteralExpression(LiteralExpressionSyntax node)
        {
            if (node.CSharpKind() != SyntaxKind.StringLiteralExpression) return node;

            var pos = node.GetLocation().GetMappedLineSpan();
            var result = OnRewrite(pos.StartLinePosition.Line + 1, pos.StartLinePosition.Character + 1, Utils.EscapeString(Source.Lines[pos.StartLinePosition.Line].ToString()), Utils.EscapeString(node.Token.ValueText));
            if (result == null) return node;

            var names = result.Split('.');
            ExpressionSyntax newNode = SyntaxFactory.IdentifierName(names.First());
            foreach (var name in names.Skip(1))
            {
                newNode = SyntaxFactory.MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, newNode, SyntaxFactory.IdentifierName(name));
            }

            return newNode.WithLeadingTrivia(node.GetLeadingTrivia()).WithTrailingTrivia(node.GetTrailingTrivia());
        }
    }

    public static class CodeTransform
    {
        public static string ReplaceStringLiteralsWithVariables(string text, IEnumerable<string> macros, Func<int, int, string, string, string> onReplace)
        {
            var sourceText = SourceText.From(text);
            var tree = CSharpSyntaxTree.ParseText(sourceText, new CSharpParseOptions(preprocessorSymbols: macros));
            var newRoot = new StringLiteralToVariableRewriter() { OnRewrite = onReplace, Source = sourceText, }.Visit(tree.GetRoot());
            return tree.WithRootAndOptions(newRoot, tree.Options).ToString();
        }
    }
}
