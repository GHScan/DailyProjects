using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace _6_JIT {
    class Program {
        static void Main(string[] args) {
            var tokens = Parser.Tokenize(new System.IO.StreamReader("../../test.rkt").ReadToEnd());
            while (true) {
                object s = Parser.Parse(tokens);
                if (s == null) break;
                object v = JITInterpreter_DS2.Instance().Interpret(s);
                // object v = JITInterpreter_DS.Instance().Interpret(s);
                // object v = ASTInterpreter.Instance().Interpret(s);
                if (v != null) {
                    ListProcess.PrintPairExp(v);
                }
            }
        }
    }
}
