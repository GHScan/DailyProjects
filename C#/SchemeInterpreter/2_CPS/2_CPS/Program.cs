using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using System.Text.RegularExpressions;
using System.Numerics;
using System.Diagnostics;
using System.Runtime.InteropServices;

namespace _2_CPS {
    class Program {
        //=================================================================

        class Pair {
            public object Car;
            public object Cdr;
        }

        static Pair ListToPair(List<object> l) {
            Pair r = null;
            for (int i = l.Count - 1; i >= 0; --i) {
                r = new Pair() {
                    Car = l[i], Cdr = r
                };
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

        delegate KeyValuePair<object, Continue> Continue(object v);

        delegate KeyValuePair<object, Continue> Procedure(List<object> args, Continue k);

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

            G.DefineScriptProcedure("not", (args, k) => new KeyValuePair<object, Continue>(!(bool)args[0], k));
            G.DefineScriptProcedure("identity", (args, k) => new KeyValuePair<object, Continue>(args[0], k));
            G.DefineScriptProcedure("sqr", (args, k) => {
                if (args[0] is BigInteger) {
                    var a = (BigInteger)args[0]; return new KeyValuePair<object, Continue>(a * a, k);
                } else {
                    var a = (decimal)args[0]; return new KeyValuePair<object, Continue>(a * a, k);
                }
            });

            G.DefineScriptProcedure("+", (args, k) => {
                if (args[0] is decimal || args[1] is decimal) return new KeyValuePair<object, Continue>(castToDecimal(args[0]) + castToDecimal(args[1]), k);
                return new KeyValuePair<object, Continue>((BigInteger)args[0] + (BigInteger)args[1], k);
            });
            G.DefineScriptProcedure("-", (args, k) => {
                if (args[0] is decimal || args[1] is decimal) return new KeyValuePair<object, Continue>(castToDecimal(args[0]) - castToDecimal(args[1]), k);
                return new KeyValuePair<object, Continue>((BigInteger)args[0] - (BigInteger)args[1], k);
            });
            G.DefineScriptProcedure("*", (args, k) => {
                if (args[0] is decimal || args[1] is decimal) return new KeyValuePair<object, Continue>(castToDecimal(args[0]) * castToDecimal(args[1]), k);
                return new KeyValuePair<object, Continue>((BigInteger)args[0] * (BigInteger)args[1], k);
            });
            G.DefineScriptProcedure("/", (args, k) => {
                if (args[0] is decimal || args[1] is decimal) return new KeyValuePair<object, Continue>(castToDecimal(args[0]) / castToDecimal(args[1]), k);
                return new KeyValuePair<object, Continue>((BigInteger)args[0] / (BigInteger)args[1], k);
            });
            G.DefineScriptProcedure("quotient", (args, k) => {
                if (args[0] is decimal || args[1] is decimal) return new KeyValuePair<object, Continue>((BigInteger)(castToDecimal(args[0]) / castToDecimal(args[1])), k);
                return new KeyValuePair<object, Continue>((BigInteger)args[0] / (BigInteger)args[1], k);
            });
            G.DefineScriptProcedure("remainder", (args, k) => {
                if (args[0] is decimal || args[1] is decimal) return new KeyValuePair<object, Continue>(castToDecimal(args[0]) % castToDecimal(args[1]), k);
                return new KeyValuePair<object, Continue>((BigInteger)args[0] % (BigInteger)args[1], k);
            });
            G.DefineScriptProcedure("=", (args, k) => new KeyValuePair<object, Continue>(args[0].Equals(args[1]), k));
            G.DefineScriptProcedure("<", (args, k) => new KeyValuePair<object, Continue>((args[0] as IComparable).CompareTo(args[1]) < 0, k));
            G.DefineScriptProcedure("<=", (args, k) => new KeyValuePair<object, Continue>((args[0] as IComparable).CompareTo(args[1]) <= 0, k));
            G.DefineScriptProcedure(">", (args, k) => new KeyValuePair<object, Continue>((args[0] as IComparable).CompareTo(args[1]) > 0, k));
            G.DefineScriptProcedure(">=", (args, k) => new KeyValuePair<object, Continue>((args[0] as IComparable).CompareTo(args[1]) >= 0, k));
            G.DefineScriptProcedure("eq?", (args, k) => new KeyValuePair<object, Continue>(object.ReferenceEquals(args[0], args[1]), k));

            G.DefineScriptProcedure("cons", (args, k) => new KeyValuePair<object, Continue>(new Pair() {
                Car = args[0], Cdr = args[1]
            }, k));
            G.DefineScriptProcedure("car", (args, k) => new KeyValuePair<object, Continue>(((Pair)args[0]).Car, k));
            G.DefineScriptProcedure("cdr", (args, k) => new KeyValuePair<object, Continue>(((Pair)args[0]).Cdr, k));
            G.DefineScriptProcedure("drop", (args, k) => {
                Pair l = (Pair)args[0]; int n = (int)(BigInteger)args[1];
                for (; n > 0; --n) {
                    l = (Pair)l.Cdr;
                }
                return new KeyValuePair<object, Continue>(l, k);
            });
            G.DefineScriptProcedure("length", (args, k) => {
                int n = 0;
                for (Pair l = (Pair)args[0]; l != null; ++n, l = (Pair)l.Cdr) ;
                return new KeyValuePair<object, Continue>(n, k);
            });
            G.DefineScriptProcedure("append", (args, k) => {
                var l = PairToList((Pair)args[0]);
                l.InsertRange(l.Count, PairToList((Pair)args[1]));
                return new KeyValuePair<object, Continue>(ListToPair(l), k);
            });
            G.Define("empty", null);
            G.DefineScriptProcedure("empty?", (args, k) => new KeyValuePair<object, Continue>(args[0] == null, k));

            G.DefineScriptProcedure("pretty-print", (args, k) => {
                PrintPairExp(args[0]);
                return new KeyValuePair<object, Continue>(null, k);
            });
            G.DefineScriptProcedure("display", (args, k) => {
                PrintListExp(PairExpToListExp(args[0]));
                return new KeyValuePair<object, Continue>(null, k);
            });
            G.DefineScriptProcedure("current-inexact-milliseconds", (args, k) => {
                long now;
                QueryPerformanceCounter(out now);
                return new KeyValuePair<object, Continue>((decimal)(now - sTimerStart) * 1000 / sTimerFreq, k);
            });
            G.DefineScriptProcedure("exit", (args, k) => {
                Environment.Exit(0);
                return new KeyValuePair<object, Continue>(null, k);
            });
            G.DefineScriptProcedure("random", (args, k) => new KeyValuePair<object, Continue>((BigInteger)sRandom.Next((int)(BigInteger)args[0]), k));
            G.DefineScriptProcedure("eval", (args, k) => new KeyValuePair<object, Continue>(ForceEval(G, PairExpToListExp(args[0])), k));
            G.DefineScriptProcedure("call/cc", (args, k) => {
                return ((Procedure)args[0])(new List<object>() {
                    (Procedure)((args2, k2)=> new KeyValuePair<object, Continue>(args2[0], k)),
                }, k);
            });

            return G;
        }

        //=================================================================

        static KeyValuePair<object, Continue> EvalSequentially(Env env, List<object> exps, int expIdx, Continue k) {
            if (expIdx == exps.Count - 1) {
                return Eval(env, exps[expIdx], k);
            } else {
                return Eval(env, exps[expIdx], (_ => EvalSequentially(env, exps, expIdx + 1, k)));
            }
        }

        static KeyValuePair<object, Continue> EvalList(Env env, List<object> exps, int expIdx, List<object> result, int ridx, Continue k) {
            if (expIdx == exps.Count) {
                return new KeyValuePair<object, Continue>(result, k);
            } else {
                return Eval(env, exps[expIdx], (v => {
                    result[ridx] = v;
                    return EvalList(env, exps, expIdx + 1, result, ridx + 1, k);
                }));
            }
        }

        static Procedure MakeScriptProcedure(Env env, List<object> _formals, int formalIdx, List<object> exps, int expIdx) {
            List<string> formals = _formals.Skip(formalIdx).Select(s => (string)s).ToList();
            return (actuals, k) => {
                Env newEnv = new Env(env, formals, actuals);
                return EvalSequentially(newEnv, exps, expIdx, k);
            };
        }

        static object ForceEval(Env env, object exp) {
            KeyValuePair<object, Continue> p = Eval(env, exp, null);
            while (p.Value != null) p = p.Value(p.Key);
            return p.Key;
        }

        static KeyValuePair<object, Continue> Eval(Env env, object exp, Continue k) {
            if (exp is string) {
                return new KeyValuePair<object,Continue>(env.Lookup((string)exp), k);
            } else if (!(exp is List<object>)) {
                return new KeyValuePair<object,Continue>(exp, k);
            }

            List<object> l = (List<object>)exp;
            switch (l[0] as string) {
                case "quote":
                    return new KeyValuePair<object,Continue>(ListExpToPairExp(l[1]), k);
                case "if":
                    return Eval(env, l[1], (b => {
                        if ((bool)b) return Eval(env, l[2], k);
                        else return Eval(env, l[3], k);
                    }));
                case "lambda":
                    return new KeyValuePair<object,Continue>(MakeScriptProcedure(env, (List<object>)l[1], 0, l, 2), k);
                case "begin":
                    return EvalSequentially(env, l, 1, k);
                case "cond":
                    if (l.Count == 2) {
                        return EvalSequentially(env, (List<object>)l[1], 0, k);
                    } else {
                        List<object> caseExps = (List<object>)l[1];
                        List<object> newExp = new List<object>() {
                            "if", caseExps[0], 
                            new List<object>(){"begin"}.Concat(caseExps.Skip(1)).ToList(), 
                            new List<object>(){"cond"}.Concat(l.Skip(2)).ToList(),
                        };
                        return Eval(env, newExp, k);
                    }
                case "define":
                    if (l[1] is string) {
                        return Eval(env, l[2], (v => {
                            env.Define((string)l[1], v);
                            return new KeyValuePair<object, Continue>(null, k);
                        }));
                    } else {
                        env.Define((string)((List<object>)l[1])[0], MakeScriptProcedure(env, (List<object>)l[1], 1, l, 2));
                        return new KeyValuePair<object, Continue>(null, k);
                    }
                case "set!":
                    return Eval(env, l[2], (v => {
                        env.Set((string)l[1], v);
                        return new KeyValuePair<object, Continue>(null, k);
                    }));
                case "let": {
                        List<object> nameValues = (List<object>)l[1];
                        var names = nameValues.Select(a => ((List<object>)a)[0]);
                        var values = nameValues.Select(a => ((List<object>)a)[1]);
                        List<object> lambda = new List<object> { "lambda", names.ToList() }.Concat(l.Skip(2)).ToList();
                        List<object> newExp = new List<object> { lambda }.Concat(values).ToList();
                        return Eval(env, newExp, k);
                    }
                default: {
                    return Eval(env, l[0], (p => {
                        return EvalList(env, l, 1, l.Skip(1).ToList(), 0, (actuals => {
                            return ((Procedure)p)((List<object>)actuals, k);
                        }));
                    }));
                    }
            }
        }

        //=================================================================

        static void Main(string[] args) {
            QueryPerformanceFrequency(out sTimerFreq);
            QueryPerformanceCounter(out sTimerStart);

            Env G = SetupGlobalEnv();

            var tokens = Tokenize(new System.IO.StreamReader("../../test.rkt").ReadToEnd());
            while (true) {
                object s = Parse(tokens);
                if (s == null) break;
                object v = ForceEval(G, s);
                if (v != null) {
                    PrintPairExp(v);
                }
            }
        }
    }
}
