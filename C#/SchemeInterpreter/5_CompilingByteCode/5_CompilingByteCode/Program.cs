using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace _5_CompilingByteCode {
    class Program {
        static void Main(string[] args) {
            ByteCodeInterpreter.Instance().SetMaxStackDepth(1000, 300);

            var tokens = Parser.Tokenize(new System.IO.StreamReader("../../test.rkt").ReadToEnd());
            while (true) {
                object s = Parser.Parse(tokens);
                if (s == null) break;
                object v = ByteCodeInterpreter.Instance().Interpret(s);
                // object v = ASTInterpreter.Instance().Interpret(s);
                if (v != null) {
                    ListProcess.PrintPairExp(v);
                }
            }
        }
    }
}
