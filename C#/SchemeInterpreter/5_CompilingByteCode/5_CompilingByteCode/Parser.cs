using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using System.Text.RegularExpressions;

namespace _5_CompilingByteCode {

    class Parser {
        static public List<string> Tokenize(string s) {
            s = new Regex(@";[^\n]*\n").Replace(s, "");
            s = s.Replace("(", " ( ").Replace(")", " ) ").Replace("[", " ( ").Replace("]", " ) ");
            return s.Split(" \t\n\r".ToArray(), StringSplitOptions.RemoveEmptyEntries).ToList();
        }

        static public object Parse(List<string> tokens) {
            if (tokens.Count == 0) {
                return null;
            }

            string token = tokens[0];
            tokens.RemoveAt(0);

            INumber n;

            if (token == "(") {
                List<object> l = new List<object>();
                while (tokens[0] != ")") {
                    l.Add(Parse(tokens));
                }
                tokens.RemoveAt(0);
                return l;
            } else if (Number.TryParse(token, out n)) {
                return n;
            } else {
                return string.Intern(token);
            }
        }
    }
}
