using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace _6_JIT {
    class ASTInterpreter {
        delegate object Procedure(List<object> args);

        class Env {
            static private Env sGlobal = new Env(null, 0);
            private List<object> mVariables;
            private Env mPrevEnv;

            public Env(Env prevEnv, int localVarCount) {
                mPrevEnv = prevEnv;

                mVariables = new List<object>(localVarCount);
                for (int i = 0; i < localVarCount; ++i) mVariables.Add(null);
            }
            static public object GetVar(Env env, object address) {
                if (address is LocalAddress) return env.mVariables[((LocalAddress)address).index];

                if (address is GlobalAddress) return sGlobal.mVariables[((GlobalAddress)address).index];

                var free = (FreeAddress)address;
                for (int i = 0; i < free.envIndex; ++i) env = env.mPrevEnv;
                return env.mVariables[free.index];
            }
            static public void SetVar(Env env, object address, object value) {
                if (address is LocalAddress) {
                    env.mVariables[((LocalAddress)address).index] = value;
                    return;
                }

                if (address is GlobalAddress) {
                    sGlobal.mVariables[((GlobalAddress)address).index] = value;
                    return;
                }

                var free = (FreeAddress)address;
                for (int i = 0; i < free.envIndex; ++i) env = env.mPrevEnv;
                env.mVariables[free.index] = value;
            }

            static public void ReserveGlobalVariables(int count) {
                while (sGlobal.mVariables.Count < count) sGlobal.mVariables.Add(null);
            }
            static public void DefineBuiltin(GlobalAddress address, object value) {
                sGlobal.mVariables[address.index] = value;
            }
            static public void DefineBuiltin(GlobalAddress address, Procedure value) {
                DefineBuiltin(address, (object)value);
            }
        }

        class ASTNodeVisitor_Interpreter: IASTNodeVisitor {
            public object Value { get; private set; }
            private Env mEnv;
            public ASTNodeVisitor_Interpreter(Env env, IASTNode node) {
                mEnv = env;
                node.AcceptVisitor(this);
            }
            public void Visit(ASTNode_Literal node) {
                Value = node.value;
            }
            public void Visit(ASTNode_GetVar node) {
                Value = Env.GetVar(mEnv, node.address);
            }
            public void Visit(ASTNode_SetVar node) {
                node.rightNode.AcceptVisitor(this);
                Env.SetVar(mEnv, node.address, Value);
                Value = null;
            }
            public void Visit(ASTNode_If node) {
                node.predNode.AcceptVisitor(this);
                if ((bool)Value) node.thenNode.AcceptVisitor(this);
                else node.elseNode.AcceptVisitor(this);
            }
            public void Visit(ASTNode_Begin node) {
                foreach (var n in node.nodes) n.AcceptVisitor(this);
            }
            public void Visit(ASTNode_Lambda node) {
                Value = (Procedure)(actuals => {
                    Env newEnv = new Env(mEnv, node.locals.Count);
                    for (int i = 0; i < actuals.Count; ++i) {
                        Env.SetVar(newEnv, new LocalAddress { index = i }, actuals[i]);
                    }
                    return new ASTNodeVisitor_Interpreter(newEnv, node.bodyNode).Value;
                });
            }
            public void Visit(ASTNode_Application node) {
                node.procedureNode.AcceptVisitor(this);
                var p = (Procedure)Value;
                var actuals = new List<object>();
                foreach (var e in node.actualNodes) {
                    e.AcceptVisitor(this);
                    actuals.Add(Value);
                }
                Value = p(actuals);
            }
        }

        static long sTimerFreq = 0;
        static long sTimerStart = 0;
        static Random sRandom = new Random();

        private ASTInterpreter() {
            Utils.QueryPerformanceCounter(out sTimerStart);
            Utils.QueryPerformanceFrequency(out sTimerFreq);

            Dictionary<string, object> builtinVars = new Dictionary<string, object>() {
                {"true", true},
                {"false", false},
                {"else", true},
                {"null", null},
            };

            Dictionary<string, Procedure> builtinProcedures = new Dictionary<string, Procedure>() {
                {"not", (args) => !(bool)args[0]},
                {"identity", (args) => args[0]},
                {"sqr", (args) => {var a = args[0] as INumber; return a.Mul(a);} },
                {"+", (args) => ((INumber)args[0]).Add((INumber)args[1])},
                {"-", (args) => ((INumber)args[0]).Sub((INumber)args[1])},
                {"*", (args) => ((INumber)args[0]).Mul((INumber)args[1])},
                {"/", (args) => ((INumber)args[0]).Div((INumber)args[1])},
                {"quotient", (args) => ((INumber)args[0]).Div((INumber)args[1]).CastToInteger()},
                {"remainder", (args) => ((INumber)args[0]).Mod((INumber)args[1])},
                {"=", (args) => (args[0].Equals(args[1]))},
                {"<", (args) => (args[0] as IComparable).CompareTo(args[1]) < 0},
                {"<=", (args) => (args[0] as IComparable).CompareTo(args[1]) <= 0},
                {">", (args) => (args[0] as IComparable).CompareTo(args[1]) > 0},
                {">=", (args) => (args[0] as IComparable).CompareTo(args[1]) >= 0},
                {"eq?", (args) => object.ReferenceEquals(args[0], args[1])},

                {"cons", (args) => new Pair() { Car = args[0], Cdr = args[1] }},
                {"car", (args) => ((Pair)args[0]).Car},
                {"cdr", (args) => ((Pair)args[0]).Cdr},
                {"drop", (args) => {
                                       Pair l = (Pair)args[0]; int n = ((NumberInteger)args[1]).value;
                                       for (; n > 0; --n) {
                                           l = (Pair)l.Cdr;
                                       }
                                       return l;
                                   }},
                {"length", (args) => {
                                         int n = 0;
                                         for (Pair l = (Pair)args[0]; l != null; ++n, l = (Pair)l.Cdr) ;
                                         return Number.Create(n);
                                     }},
                {"append", (args) => {
                                         var l = ListProcess.PairToList((Pair)args[0]);
                                         l.InsertRange(l.Count, ListProcess.PairToList((Pair)args[1]));
                                         return ListProcess.ListToPair(l);
                                     }},
                {"empty?", (args) => args[0] == null},

                {"pretty-print", (args) => {
                                               ListProcess.PrintPairExp(args[0]);
                                               return null;
                                           }},
                {"display", (args) => {
                                          ListProcess.PrintListExp(ListProcess.PairExpToListExp(args[0]));
                                          return null;
                                      }},
                {"current-inexact-milliseconds", (args) => {
                                                               long now;
                                                               Utils.QueryPerformanceCounter(out now);
                                                               return Number.Create((decimal)(now - sTimerStart) * 1000 / sTimerFreq);
                                                           }},
                {"exit", (args) => {
                                       Environment.Exit(0);
                                       return null;
                                   }},
                {"random", (args) => Number.Create(sRandom.Next(((NumberInteger)args[0]).value))},
                {"eval", (args) => Instance().Interpret(ListProcess.PairExpToListExp(args[0]))},
            };

            Env.ReserveGlobalVariables(builtinVars.Count + builtinProcedures.Count);
            foreach (KeyValuePair<string, object> kv in builtinVars) {
                Env.DefineBuiltin(SymbolTable.DefineOrGetGlobalSymbol(kv.Key), kv.Value);
            }
            foreach (KeyValuePair<string, Procedure> kv in builtinProcedures) {
                Env.DefineBuiltin(SymbolTable.DefineOrGetGlobalSymbol(kv.Key), kv.Value);
            }
        }

        static private ASTInterpreter sInstance = new ASTInterpreter();
        static public ASTInterpreter Instance() {
            return sInstance;
        }

        public object Interpret(object exp) {
            IASTNode node = ASTCompiler.Compile(null, ListProcess.TransformLibraryForms(exp));
            Env.ReserveGlobalVariables(SymbolTable.GetGlobalSymbolCount());

            return new ASTNodeVisitor_Interpreter(null, node).Value;
        }
    }
}
