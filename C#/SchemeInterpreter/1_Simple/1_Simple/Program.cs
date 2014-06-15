using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using System.Text.RegularExpressions;
using System.Numerics;
using System.Diagnostics;
using System.Runtime.InteropServices;

namespace _1_Simple
{
    class Program
    {
        //=================================================================

        class Pair {
            public object Car;
            public object Cdr;
        }

        static Pair ListToPair(List<object> l) {
            Pair r = null;
            for (int i = l.Count - 1; i >= 0; --i) {
                r = new Pair(){Car=l[i], Cdr=r};
            }
            return r;
        }

        static List<object> PairToList(Pair l) {
            List<object> r = new List<object>();
            while (l != null) {
                r.Add(l.Car);
                l = (Pair)l.Cdr;
            }
            return r;
        }

        static object PairExpToListExp(object exp) {
            if (exp is Pair) {
                return PairToList((Pair)exp);
            } else {
                return exp;
            }
        }

        static object ListExpToPairExp(object exp) {
            if (exp is List<object>) {
                return ListToPair((List<object>)exp);
            } else {
                return exp;
            }
        }

        static void PrintListExp(object exp) {
            if (exp is List<object>) {
                List<object> o = (List<object>)exp;
                Console.Write("(");
                for (int i = 0; i < o.Count; ++i) {
                    if (i > 0) Console.Write(" ");
                    PrintListExp(o[i]);
                }
                Console.Write(")");
            } else {
                Console.Write(exp);
            }
        }

        static void PrintPairExp(object exp) {
            PrintListExp(PairExpToListExp(exp));
            Console.WriteLine();
        }

        //=================================================================

        static List<string> Tokenize(string s) {
            s = new Regex(@";[^\n]*\n").Replace(s, "");
            s = s.Replace("(", " ( ").Replace(")", " ) ").Replace("[", " ( ").Replace("]", " ) ");
            return s.Split(" \r\n\t".ToArray(), StringSplitOptions.RemoveEmptyEntries).ToList();
        }        

        static object Parse(List<string> tokens) {
            if (tokens.Count == 0) {
                return null;
            }

            string token = tokens[0];
            tokens.RemoveAt(0);

            BigInteger bi; decimal f;

            if (token == "(") {
                List<object> l = new List<object>();
                while (tokens[0] != ")") {
                    l.Add(Parse(tokens));
                }
                tokens.RemoveAt(0);
                return l;
            } else if (BigInteger.TryParse(token, out bi)) {
                return bi;
            } else if (decimal.TryParse(token, out f)) {
                return f;
            } else {
                return string.Intern(token);
            }
        }

        //=================================================================

        delegate object Procedure(List<object> args);

        class Env {
            private Env mPreEnv;
            private Dictionary<string, object> mFrame = new Dictionary<string, object>();

            public Env(Env preEnv, List<string> formals, List<object> actuals) {
                mPreEnv = preEnv;

                if (actuals.Count != formals.Count) throw new Exception("Argument count mismatch!");

                for (int i = 0; i < formals.Count; ++i) {
                    mFrame.Add(formals[i], actuals[i]);
                }
            }

            public object Lookup(string name) {
                if (mFrame.ContainsKey(name)) {
                    return mFrame[name];
                } else {
                    return mPreEnv.Lookup(name);
                }
            }

            public void Define(string name, object value) {
                mFrame.Add(name, value);
            }

            public void DefineScriptProcedure(string name, Procedure p) {
                Define(name, (object)p);
            }

            public void Set(string name, object value) {
                if (mFrame.ContainsKey(name)) {
                    mFrame[name] = value;
                } else {
                    mPreEnv.Set(name, value);
                }
            }
        }

        [DllImport("Kernel32.dll")]
        static extern bool QueryPerformanceCounter(out long lpPerformanceCount);
        [DllImport("Kernel32.dll")]
        static extern bool QueryPerformanceFrequency(out long lpFrequency);

        static long sTimerFreq = 0;
        static long sTimerStart = 0;
        static Random sRandom = new Random();

        static decimal castToDecimal(object o) {
            if (o is BigInteger) return (decimal)(BigInteger)o;
            return (decimal)o;
        }

        static Env SetupGlobalEnv() {
            Env G = new Env(null, new List<string>(), new List<object>());

            G.Define("true", true);
            G.Define("false", false);
            G.Define("else", true);

            G.DefineScriptProcedure("not", args => !(bool)args[0]);
            G.DefineScriptProcedure("identity", args => args[0]);
            G.DefineScriptProcedure("sqr", args => {
                if (args[0] is BigInteger) {
                    var a = (BigInteger)args[0]; return a * a;
                } else {
                    var a = (decimal)args[0]; return a * a;
                }
            });

            G.DefineScriptProcedure("+", args => {
                if (args[0] is decimal || args[1] is decimal) return castToDecimal(args[0]) + castToDecimal(args[1]);
                return (BigInteger)args[0] + (BigInteger)args[1];
            });
            G.DefineScriptProcedure("-", args => {
                if (args[0] is decimal || args[1] is decimal) return castToDecimal(args[0]) - castToDecimal(args[1]);
                return (BigInteger)args[0] - (BigInteger)args[1];
            });
            G.DefineScriptProcedure("*", args => {
                if (args[0] is decimal || args[1] is decimal) return castToDecimal(args[0]) * castToDecimal(args[1]);
                return (BigInteger)args[0] * (BigInteger)args[1];
            });
            G.DefineScriptProcedure("/", args => {
                if (args[0] is decimal || args[1] is decimal) return castToDecimal(args[0]) / castToDecimal(args[1]);
                return (BigInteger)args[0] / (BigInteger)args[1];
            });
            G.DefineScriptProcedure("quotient", args => {
                if (args[0] is decimal || args[1] is decimal) return (BigInteger)(castToDecimal(args[0]) / castToDecimal(args[1]));
                return (BigInteger)args[0] / (BigInteger)args[1];
            });
            G.DefineScriptProcedure("remainder", args => {
                if (args[0] is decimal || args[1] is decimal) return castToDecimal(args[0]) % castToDecimal(args[1]);
                return (BigInteger)args[0] % (BigInteger)args[1];
            });
            G.DefineScriptProcedure("=", args => args[0].Equals(args[1]));
            G.DefineScriptProcedure("<", args => (args[0] as IComparable).CompareTo(args[1]) < 0);
            G.DefineScriptProcedure("<=", args => (args[0] as IComparable).CompareTo(args[1]) <= 0);
            G.DefineScriptProcedure(">", args => (args[0] as IComparable).CompareTo(args[1]) > 0);
            G.DefineScriptProcedure(">=", args => (args[0] as IComparable).CompareTo(args[1]) >= 0);
            G.DefineScriptProcedure("eq?", args => object.ReferenceEquals(args[0], args[1]));

            G.DefineScriptProcedure("cons", args => new Pair() {
                Car = args[0], Cdr = args[1]
            });
            G.DefineScriptProcedure("car", args => ((Pair)args[0]).Car);
            G.DefineScriptProcedure("cdr", args => ((Pair)args[0]).Cdr);
            G.DefineScriptProcedure("drop", args => {
                Pair l = (Pair)args[0]; int n = (int)(BigInteger)args[1];
                for (; n > 0; --n) {
                    l = (Pair)l.Cdr;
                }
                return l;
            });
            G.DefineScriptProcedure("length", args => {
                int n = 0;
                for (Pair l = (Pair)args[0]; l != null; ++n, l = (Pair)l.Cdr) ;
                return n;
            });
            G.DefineScriptProcedure("append", args => {
                var l = PairToList((Pair)args[0]);
                l.InsertRange(l.Count, PairToList((Pair)args[1]));
                return ListToPair(l);
            });
            G.Define("empty", null);
            G.DefineScriptProcedure("empty?", args => args[0] == null);

            G.DefineScriptProcedure("pretty-print", args => {
                PrintPairExp(args[0]);
                return null;
            });
            G.DefineScriptProcedure("current-inexact-milliseconds", args => {
                long now;
                QueryPerformanceCounter(out now);
                return (decimal)(now - sTimerStart) * 1000 / sTimerFreq;
            });
            G.DefineScriptProcedure("random", args => (BigInteger)sRandom.Next((int)(BigInteger)args[0]));
            G.DefineScriptProcedure("eval", args => Eval(G, PairExpToListExp(args[0])));

            return G;
        }

        //=================================================================

        static Tuple<Env, List<string>, List<object>> MakeScriptProcedure(Env env, List<object> formals, int formalIdx, List<object> exps, int expIdx) {
            return new Tuple<Env, List<string>, List<object>>(env, formals.Skip(formalIdx).Cast<string>().ToList(), exps.Skip(expIdx).ToList());
        }

        static object Eval(Env env, object exp) {
            while (true) {
                if (exp is string) {
                    return env.Lookup((string)exp);
                } else if (!(exp is List<object>)) {
                    return exp;
                }

                List<object> l = (List<object>)exp;
                switch (l[0] as string) {
                    case "quote":
                        return ListExpToPairExp(l[1]);
                    case "if":
                        if ((bool)Eval(env, l[1])) {
                            exp = l[2];
                        } else {
                            exp = l[3];
                        }
                        break;
                    case "lambda":
                        return MakeScriptProcedure(env, (List<object>)l[1], 0, l, 2);
                    case "begin":
                        for (int i = 1; i < l.Count - 1; ++i) Eval(env, l[i]);
                        exp = l[l.Count - 1];
                        break;
                    case "cond":
                        for (int i = 1; i < l.Count; ++i) {
                            List<object> caseExps = (List<object>)l[i];
                            if ((bool)Eval(env, caseExps[0])) {
                                for (int j = 1; j < caseExps.Count - 1; ++j) Eval(env, caseExps[j]);
                                exp = caseExps[caseExps.Count - 1];
                                break;
                            }
                        }
                        break;
                    case "define":
                        if (l[1] is string) {
                            env.Define((string)l[1], Eval(env, l[2]));
                        } else {
                            env.Define((string)((List<object>)l[1])[0], MakeScriptProcedure(env, (List<object>)l[1], 1, l, 2));
                        }
                        return null;
                    case "set!":
                        env.Set((string)l[1], Eval(env, l[2]));
                        return null;
                    case "let": {
                            List<object> nameValues = (List<object>)l[1];
                            var names = nameValues.Select(a => ((List<object>)a)[0]);
                            var values = nameValues.Select(a => ((List<object>)a)[1]);
                            List<object> lambda = new List<object> { "lambda", names.ToList() }.Concat(l.Skip(2)).ToList();
                            List<object> newExp = new List<object> { lambda }.Concat(values).ToList();
                            exp = newExp;
                        }
                        break;
                    default: {
                            object p = Eval(env, l[0]);
                            List<object> actuals = l.Skip(1).Select(e => Eval(env, e)).ToList();
                            if (p is Procedure) {
                                return ((Procedure)p)(actuals);
                            } else {
                                Tuple<Env, List<string>, List<object>> lambda = (Tuple<Env, List<string>, List<object>>)p;
                                env = new Env(lambda.Item1, lambda.Item2, actuals);
                                for (int i = 0; i < lambda.Item3.Count - 1; ++i) Eval(env, lambda.Item3[i]);
                                exp = lambda.Item3[lambda.Item3.Count - 1];
                            }
                        }
                        break;
                }
            }
        }

        //=================================================================

        static void Main(string[] args)
        {
            QueryPerformanceFrequency(out sTimerFreq);
            QueryPerformanceCounter(out sTimerStart);

            Env G = SetupGlobalEnv();
           
            var tokens = Tokenize(new System.IO.StreamReader("../../test.rkt").ReadToEnd());
            while (true) {
                object s = Parse(tokens);
                if (s == null) break;
                object v = Eval(G, s);
                if (v != null) {
                    PrintPairExp(v);
                }
            }
        }
    }
}
