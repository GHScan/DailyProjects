namespace Antlr.Examples.Expr
{
	using System;
	using System.IO;
	using Antlr.Runtime;

	public class ExprMain
	{
		public static void Main(string[] args)
		{
			try
			{
                ICharStream input = new ANTLRReaderStream(System.Console.In);
                ExprLexer lexer = new ExprLexer(input);
                CommonTokenStream tokens = new CommonTokenStream(lexer);
                ExprParser parser = new ExprParser(tokens);
                parser.prog();
			}
			catch (System.Exception e)
			{
				Console.Error.WriteLine("exception: " + e);
				Console.Error.WriteLine(e.StackTrace); 
			}
		}
	}
}
