using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using System.Text.RegularExpressions;
using System.Numerics;
using System.Diagnostics;
using System.Runtime.InteropServices;

namespace _4_ComplingAST {
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

        class Env {
            private object[] mVariables;

            public Env(Env preEnv, int varCount) {
                PrevEnv = preEnv;
                mVariables = new object[varCount];
            }

            public Env PrevEnv { get; private set; }

            public object this[int index] {
                get{ return mVariables[index];}
                set{ mVariables[index] = value;}
            }
        }

        class GlobalEnv {
            static private GlobalEnv sInstance = new GlobalEnv();

            static public GlobalEnv Instance() {
                return sInstance;
            }

            private GlobalEnv() {
            }

            private List<object> mVariables = new List<object>();

            public void ExtendTo(int newCount) {
                while (mVariables.Count < newCount) mVariables.Add(null);
            }

            public object this[int index] {
                get { return mVariables[index]; }
                set { mVariables[index] = value; }
            }
        }

        //=================================================================

        class SymbolTable {
            protected Dictionary<string, int> mVarLoc = new Dictionary<string, int>();
            protected int mNextLoc = 0;

            public SymbolTable PrevTalbe { get; private set;}

            public SymbolTable(SymbolTable prevTable) {
                PrevTalbe = prevTable;
            }

            public int Define(string name) {
                if (mVarLoc.ContainsKey(name)) throw new Exception("Variable has already defined:" + name);
                mVarLoc[name] = mNextLoc++;
                return mNextLoc - 1;
            }

            public int Lookup(string name) {
                if (mVarLoc.ContainsKey(name)) return mVarLoc[name];
                else return -1;
            }

            public int GetSymbolCount() {
                return mNextLoc;
            }
        }

        class GlobalSymbolTable {
            static private GlobalSymbolTable sInstance = new GlobalSymbolTable();
            static public GlobalSymbolTable Instance() {
                return sInstance;
            }

            private GlobalSymbolTable() {
            }

            private Dictionary<string, int> mVarLoc = new Dictionary<string,int>();
            private int mNextLoc = 0;          

            public int LookupOrDefine(string name) {
                if (mVarLoc.ContainsKey(name)) return mVarLoc[name];
                mVarLoc[name] = mNextLoc++;
                return mNextLoc - 1;
            }

            public int GetSymbolCount() {
                return mNextLoc;
            }
        }

        //=================================================================

        delegate KeyValuePair<object, Continue> Continue(object v);
        delegate KeyValuePair<object, Continue> Procedure(List<object> args, Continue k);

        //=================================================================
        interface IASTNode {
            KeyValuePair<object, Continue> Eval(Env e, Continue k);
        }

        static KeyValuePair<object, Continue> EvalSequentially(Env e, IASTNode[] nodes, int off, Continue k) {
            if (off == nodes.Length - 1) {
                return nodes[off].Eval(e, k);
            } else {
                return nodes[off].Eval(e, _ => {
                    return EvalSequentially(e, nodes, off + 1, k);
                });
            }
        }

        static KeyValuePair<object, Continue> EvalList(Env e, IASTNode[] nodes, int off, List<object> result, int rOff, Continue k) {
            if (off == nodes.Length) {
                return new KeyValuePair<object, Continue>(result, k);
            } else {
                return nodes[off].Eval(e, v => {
                    result[rOff] = v;
                    return EvalList(e, nodes, off + 1, result, rOff + 1, k);
                });
            }
        }

        class ASTNode_GetLocalVar: IASTNode {
            public int Index { get; set; }

            public KeyValuePair<object, Continue> Eval(Env e, Continue k) { 
                return k(e[Index]); 
            }
        }

        class ASTNode_SetLocalVar: IASTNode {
            public int Index{ get; set; }
            public IASTNode RightNode { get; set; }

            public KeyValuePair<object, Continue> Eval(Env e, Continue k) {
                return RightNode.Eval(e, v => {
                    e[Index] = v;
                    return k(null);
                });
            }
        }

        class ASTNode_GetFreeVar: IASTNode {
            public int EnvIndex { get; set; }
            public int Index { get; set; }

            public KeyValuePair<object, Continue> Eval(Env e, Continue k) { 
                for (int i = 0; i < EnvIndex; ++i) {
                    e = e.PrevEnv;
                }
                return k(e[Index]);
            }
        }

        class ASTNode_SetFreeVar: IASTNode {
            public int EnvIndex { get; set; }
            public int Index{ get; set; }
            public IASTNode RightNode { get; set; }

            public KeyValuePair<object, Continue> Eval(Env e, Continue k) {
                return RightNode.Eval(e, v => {
                    for (int i = 0; i < EnvIndex; ++i) {
                        e = e.PrevEnv;
                    }
                    e[Index] = v;
                    return k(null);
                });
            }
        }

        class ASTNode_GetGlobalVar: IASTNode {
            public int Index { get; set; }

            public KeyValuePair<object, Continue> Eval(Env e, Continue k) { 
                return k(GlobalEnv.Instance()[Index]); 
            }
        }

        class ASTNode_SetGlobalVar: IASTNode {
            public int Index { get; set; }
            public IASTNode RightNode { get; set; }

            public KeyValuePair<object, Continue> Eval(Env e, Continue k) {
                return RightNode.Eval(e, v=>{
                    GlobalEnv.Instance()[Index] = v;
                    return k(null);
                });
            }
        }

        class ASTNode_If: IASTNode {
            public IASTNode PredNode { get; set; }
            public IASTNode ThenNode { get; set; }
            public IASTNode ElseNode { get; set; }

            public KeyValuePair<object, Continue> Eval(Env e, Continue k) {
                return PredNode.Eval(e, b => {
                    if ((bool)b) return ThenNode.Eval(e, k);
                    else return ElseNode.Eval(e, k);
                });
            }
        }

        class ASTNode_Literal: IASTNode {
            public object Value { get; set; }

            public KeyValuePair<object, Continue> Eval(Env e, Continue k) {
                return k(Value);
            }
        }

        class ASTNode_Begin: IASTNode {
            public IASTNode[] Nodes { get; set; }

            public KeyValuePair<object, Continue> Eval(Env e, Continue k) {
                return EvalSequentially(e, Nodes, 0, k);
            }
        }

        class ASTNode_Lambda: IASTNode {
            public int LocalVarCount { get; set; }
            public IASTNode BodyNode { get; set; }

            public KeyValuePair<object, Continue> Eval(Env e, Continue k) {
                return k((Procedure)((actuals, k2)=>{
                    Env newEnv = new Env(e, LocalVarCount);
                    for (int i = 0; i < actuals.Count; ++i) newEnv[i] = actuals[i];
                    return BodyNode.Eval(newEnv, k2);
                }));
            }
        }

        class ASTNode_Application: IASTNode {
            public IASTNode ProcedureNode { get; set; }
            public IASTNode[] ActualNodes { get; set; }

            public KeyValuePair<object, Continue> Eval(Env e, Continue k) {
                return ProcedureNode.Eval(e, p => {
                    return EvalList(e, ActualNodes, 0, ActualNodes.ToList<object>(), 0, actuals=>{
                        return ((Procedure)p)((List<object>)actuals, k);
                    });
                });
            }
        }

        //=================================================================
        static object TransformLibraryForm(object exp) {
            List<object> l = exp as List<object>;
            if (l == null) return exp;

            switch (l[0] as string) {
                case "quote":
                    return exp;
                case "if":
                    return new List<object>(){"if", TransformLibraryForm(l[1]), TransformLibraryForm(l[2]), TransformLibraryForm(l[3])};
                case "lambda":
                    return new List<object>(){"lambda", l[1], TransformLibraryForm(new List<object>(){"begin"}.Concat(l.Skip(2)).ToList())};
                case "begin":
                    return new List<object>(){"begin"}.Concat(l.Skip(1).Select(TransformLibraryForm)).ToList();
                case "cond":
                    List<object> caseExps = (List<object>)l[1];
                   if (l.Count == 2) {
                       return TransformLibraryForm(new List<object>(){"begin"}.Concat(caseExps).ToList());
                   } else {
                       return TransformLibraryForm(new List<object>(){"if", caseExps[0], 
                            new List<object>(){"begin"}.Concat(caseExps.Skip(1)).ToList(),
                            new List<object>(){"cond"}.Concat(l.Skip(2)).ToList()});
                   }
                case "define":
                    if (l[1] is string) {
                        return new List<object>(){"define", l[1], TransformLibraryForm(l[2])};
                    }
                    else {
                        List<object> nameAndFormals = (List<object>)l[1];
                        return new List<object>(){"define", nameAndFormals[0], 
                            TransformLibraryForm(
                                new List<object>(){ "lambda", nameAndFormals.Skip(1).ToList(), }.Concat(l.Skip(2)).ToList()
                            )};
                    }
                case "set!":
                    return new List<object>(){"set!", l[1], TransformLibraryForm(l[2])};
                case "let":
                    List<object> nameValues = (List<object>)l[1];
                    var names = nameValues.Select(a => ((List<object>)a)[0]);
                    var values = nameValues.Select(a => ((List<object>)a)[1]);
                    List<object> lambda = new List<object> { "lambda", names.ToList() }.Concat(l.Skip(2)).ToList();
                    List<object> newExp = new List<object> { lambda }.Concat(values).ToList();
                    return TransformLibraryForm(newExp);
                default:
                    return l.Select(TransformLibraryForm).ToList();
            }
        }

        static void FindDefinition(List<string> defines, List<object> exps, int off) {
            for (int i = off; i < exps.Count; ++i) {
                List<object> l = exps[i] as List<object>;
                if (l == null) continue;
                switch (l[0] as string) {
                    case "define":
                        defines.Add((string)l[1]);
                        break;
                    case "begin":
                        FindDefinition(defines, l, 1);
                        break;
                    default:
                        break;
                }
            }
        }

        static IASTNode Compile(SymbolTable symTable, object exp) {
            if (exp is string) {
                string name = (string)exp;
                if (symTable != null && symTable.Lookup(name) != -1) {
                    return new ASTNode_GetLocalVar(){Index=symTable.Lookup(name)};
                } 

                int envIndex = 0;
                for (; symTable != null && symTable.Lookup(name) == -1; symTable = symTable.PrevTalbe, ++envIndex);

                if (symTable == null) {
                    return new ASTNode_GetGlobalVar(){Index=GlobalSymbolTable.Instance().LookupOrDefine(name)};
                } else {
                    return new ASTNode_GetFreeVar(){EnvIndex=envIndex, Index=symTable.Lookup(name)};
                }
            } else if (!(exp is List<object>)) {
                return new ASTNode_Literal(){Value=exp};
            }

            List<object> l = (List<object>)exp;
            switch (l[0] as string) {
                case "quote":
                    return new ASTNode_Literal() { Value = ListExpToPairExp(l[1]) };
                case "if":
                    return new ASTNode_If() { PredNode = Compile(symTable, l[1]), ThenNode = Compile(symTable, l[2]), ElseNode = Compile(symTable, l[3]) };
                case "lambda": {
                        SymbolTable newSymTable = new SymbolTable(symTable);
                        foreach (string name in ((List<object>)l[1])) {
                            newSymTable.Define(name);
                        }

                        List<string> defines = new List<string>();
                        FindDefinition(defines, (List<object>)l[2], 1);
                        foreach (string name in defines) {
                            newSymTable.Define(name);
                        }

                        return new ASTNode_Lambda() { LocalVarCount = newSymTable.GetSymbolCount(), BodyNode = Compile(newSymTable, l[2]) };
                    }
                case "begin":
                    return new ASTNode_Begin() { Nodes = l.Skip(1).Select(e => Compile(symTable, e)).ToArray() };
                case "define":
                    return Compile(symTable, new List<object>() { "set!", l[1], l[2] });
                case "set!": {
                        IASTNode right = Compile(symTable, l[2]);

                        string name = (string)l[1];
                        if (symTable != null && symTable.Lookup(name) != -1) {
                            return new ASTNode_SetLocalVar() { Index = symTable.Lookup(name), RightNode = right };
                        }

                        int envIndex = 0;
                        for (; symTable != null && symTable.Lookup(name) == -1; symTable = symTable.PrevTalbe, ++envIndex) ;

                        if (symTable == null) {
                            return new ASTNode_SetGlobalVar() { Index = GlobalSymbolTable.Instance().LookupOrDefine(name), RightNode = right };
                        } else {
                            return new ASTNode_SetFreeVar() { EnvIndex = envIndex, Index = symTable.Lookup(name), RightNode = right };
                        }
                    }
                default: {
                        return new ASTNode_Application() { ProcedureNode = Compile(symTable, l[0]), ActualNodes = l.Skip(1).Select(e => Compile(symTable, e)).ToArray() };
                    }
            }
        }

        //=================================================================
        [DllImport("Kernel32.dll")]
        static extern bool QueryPerformanceCounter(out long lpPerformanceCount);
        [DllImport("Kernel32.dll")]
        static extern bool QueryPerformanceFrequency(out long lpFrequency);

        static long sTimerFreq = 0;
        static long sTimerStart = 0;
        static Random sRandom = new Random();

        static decimal CastToDecimal(object o) {
            if (o is BigInteger) return (decimal)(BigInteger)o;
            return (decimal)o;
        }

        static void SetupGlobalEnv() {
            Dictionary<string, object> builtinVars = new Dictionary<string, object>() {
                {"true", true},
                {"false", false},
                {"else", true},
                {"null", null},
            };

            Dictionary<string, Procedure> builtinProcedures = new Dictionary<string, Procedure>() {
                {"not", (args, k) => k(!(bool)args[0])},
                    {"identity", (args, k) => k(args[0])},
                    {"sqr", (args, k) => {
                                             if (args[0] is BigInteger) {
                                                 var a = (BigInteger)args[0]; return k(a * a);
                                             } else {
                                                 var a = (decimal)args[0]; return k(a * a);
                                             }
                                         }},
                    {"+", (args, k) => {
                                           if (args[0] is decimal || args[1] is decimal) return k(CastToDecimal(args[0]) + CastToDecimal(args[1]));
                                           return k((BigInteger)args[0] + (BigInteger)args[1]);
                                       }},
                    {"-", (args, k) => {
                                           if (args[0] is decimal || args[1] is decimal) return k(CastToDecimal(args[0]) - CastToDecimal(args[1]));
                                           return k((BigInteger)args[0] - (BigInteger)args[1]);
                                       }},
                    {"*", (args, k) => {
                                           if (args[0] is decimal || args[1] is decimal) return k(CastToDecimal(args[0]) * CastToDecimal(args[1]));
                                           return k((BigInteger)args[0] * (BigInteger)args[1]);
                                       }},
                    {"/", (args, k) => {
                                           if (args[0] is decimal || args[1] is decimal) return k(CastToDecimal(args[0]) / CastToDecimal(args[1]));
                                           return k((BigInteger)args[0] / (BigInteger)args[1]);
                                       }},
                    {"quotient", (args, k) => {
                                                  if (args[0] is decimal || args[1] is decimal) return k((BigInteger)(CastToDecimal(args[0]) / CastToDecimal(args[1])));
                                                  return k((BigInteger)args[0] / (BigInteger)args[1]);
                                              }},
                    {"remainder", (args, k) => {
                                                   if (args[0] is decimal || args[1] is decimal) return k(CastToDecimal(args[0]) % CastToDecimal(args[1]));
                                                   return k((BigInteger)args[0] % (BigInteger)args[1]);
                                               }},
                    {"=", (args, k) => k(args[0].Equals(args[1]))},
                    {"<", (args, k) => k((args[0] as IComparable).CompareTo(args[1]) < 0)},
                    {"<=", (args, k) => k((args[0] as IComparable).CompareTo(args[1]) <= 0)},
                    {">", (args, k) => k((args[0] as IComparable).CompareTo(args[1]) > 0)},
                    {">=", (args, k) => k((args[0] as IComparable).CompareTo(args[1]) >= 0)},
                    {"eq?", (args, k) => k(object.ReferenceEquals(args[0], args[1]))},

                    {"cons", (args, k) => k(new Pair() {
                            Car = args[0], Cdr = args[1]
                            })},
                    {"car", (args, k) => k(((Pair)args[0]).Car)},
                    {"cdr", (args, k) => k(((Pair)args[0]).Cdr)},
                    {"drop", (args, k) => {
                                              Pair l = (Pair)args[0]; int n = (int)(BigInteger)args[1];
                                              for (; n > 0; --n) {
                                                  l = (Pair)l.Cdr;
                                              }
                                              return k(l);
                                          }},
                    {"length", (args, k) => {
                                                int n = 0;
                                                for (Pair l = (Pair)args[0]; l != null; ++n, l = (Pair)l.Cdr) ;
                                                return k(n);
                                            }},
                    {"append", (args, k) => {
                                                var l = PairToList((Pair)args[0]);
                                                l.InsertRange(l.Count, PairToList((Pair)args[1]));
                                                return k(ListToPair(l));
                                            }},
                    {"empty?", (args, k) => k(args[0] == null)},

                    {"pretty-print", (args, k) => {
                                                      PrintPairExp(args[0]);
                                                      return k(null);
                                                  }},
                    {"display", (args, k) => {
                                                 PrintListExp(PairExpToListExp(args[0]));
                                                 return k(null);
                                             }},
                    {"current-inexact-milliseconds", (args, k) => {
                                                                      long now;
                                                                      QueryPerformanceCounter(out now);
                                                                      return k((decimal)(now - sTimerStart) * 1000 / sTimerFreq);
                                                                  }},
                    {"exit", (args, k) => {
                                              Environment.Exit(0);
                                              return k(null);
                                          }},
                    {"random", (args, k) => k((BigInteger)sRandom.Next((int)(BigInteger)args[0]))},
                    {"eval", (args, k) => k(ForceEval(null, PairExpToListExp(args[0])))},
                    {"call/cc", (args, k) => {
                                                 return ((Procedure)args[0])(new List<object>() {
                                                         (Procedure)((args2, k2)=> new KeyValuePair<object, Continue>(args2[0], k)),
                                                         }, k);
                                             }},
            };

            GlobalEnv.Instance().ExtendTo(builtinVars.Count + builtinProcedures.Count);
            foreach (var nameValue in builtinVars) {
                int index = GlobalSymbolTable.Instance().LookupOrDefine(nameValue.Key);
                GlobalEnv.Instance()[index] = nameValue.Value;
            }
            foreach (var nameValue in builtinProcedures) {
                int index = GlobalSymbolTable.Instance().LookupOrDefine(nameValue.Key);
                GlobalEnv.Instance()[index] = nameValue.Value;
            }
        }

        //=================================================================

        static object ForceEval(Env env, object exp) {
            IASTNode node = Compile(null, TransformLibraryForm(exp));
            GlobalEnv.Instance().ExtendTo(GlobalSymbolTable.Instance().GetSymbolCount());

            KeyValuePair<object, Continue> p = node.Eval(env, (v => new KeyValuePair<object, Continue>(v, null))); ;
            while (p.Value != null) p = p.Value(p.Key);
            return p.Key;
        }

        //=================================================================

        static void Main(string[] args) {
            QueryPerformanceFrequency(out sTimerFreq);
            QueryPerformanceCounter(out sTimerStart);

            SetupGlobalEnv();

            var tokens = Tokenize(new System.IO.StreamReader("../../test.rkt").ReadToEnd());
            while (true) {
                object s = Parse(tokens);
                if (s == null) break;
                object v = ForceEval(null, s);
                if (v != null) {
                    PrintPairExp(v);
                }
            }
        }
    }
}
