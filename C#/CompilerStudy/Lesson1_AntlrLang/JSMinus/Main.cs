using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using Antlr.Runtime;

public class JSMinusMain {
    public static void Main(string[] args) {
        try {
            if (args.Length < 1) {
                System.Console.WriteLine("Usage : JSMinus file");
                return;
            }

            ICharStream input = new ANTLRFileStream(args[0]);
            JSMinusLexer lexer = new JSMinusLexer(input);
            CommonTokenStream tokens = new CommonTokenStream(lexer);
            JSMinusParser parser = new JSMinusParser(tokens);
            CodeEmitor emitor = new CodeEmitor("JSMinus", parser.program());
            emitor.save();
            emitor.run();

        } catch (System.Exception e) {
            Console.Error.WriteLine(e.StackTrace);
            Console.Error.WriteLine("Exception: " + e);
        }
    }
}
