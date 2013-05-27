using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using Antlr.Runtime;

public class JSMinusMain {
    public static void Main(string[] args) {
        try {

            ICharStream input = new ANTLRFileStream("../../test/test.js");
            JSMinusLexer lexer = new JSMinusLexer(input);
            CommonTokenStream tokens = new CommonTokenStream(lexer);
            JSMinusParser parser = new JSMinusParser(tokens);
            var funcs = parser.program();
            System.Console.WriteLine(funcs);

        } catch (System.Exception e) {
            Console.Error.WriteLine(e.StackTrace);
            Console.Error.WriteLine("Exception: " + e);
        }
    }
}
