using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace _5_CompilingByteCode {
    class ByteCodeInterpreter {

        //=================================================================
        delegate void HostProcedure(List<object> stack, int retOff);

        class ScriptProcedureMeta {
            public int[] byteCodes;
            public int localVarCount;
            public object[] literals;
            public FreeAddress[] freeAdresses;
        }

        struct ByteCodeEnum {
            public const int PUSH_LITERAL = 0;
            public const int PUSH_LOCAL = 1;
            public const int POP_LOCAL = 2;
            public const int PUSH_GLOBAL = 3;
            public const int POP_GLOBAL = 4;
            public const int PUSH_FREE = 5;
            public const int POP_FREE = 6;
            public const int POP1 = 7;
            public const int CJMP = 8;
            public const int JMP = 9;
            public const int PUSH_SCRIPT_PROCEDURE = 10;
            public const int CALL = 11;
        }

        class ASTNodeVisitor_ByteCodeGenerator: IASTNodeVisitor {
            public List<int> ByteCodes { get; private set; }
            public List<object> Literals { get; private set; }
            private List<FreeAddress> mFreeAddresses;
            private int GetLiteralIndex(object value) {
                int index = Literals.IndexOf(value);
                if (index == -1) {
                    index = Literals.Count;
                    Literals.Add(value);
                }
                return index;
            }
            public FreeAddress GetTranslatedFreeAddress(FreeAddress address) {
                int index = mFreeAddresses.IndexOf(address);
                return new FreeAddress { envIndex=index, index=address.index};
            }
            public ASTNodeVisitor_ByteCodeGenerator(ASTNode_Lambda node) {
                ByteCodes = new List<int>();
                Literals = new List<object>();
                mFreeAddresses = node.freeAddresses;
                node.bodyNode.AcceptVisitor(this);
            }
            public void Visit(ASTNode_Literal node) {
                ByteCodes.Add(ByteCodeEnum.PUSH_LITERAL);
                ByteCodes.Add(GetLiteralIndex(node.value));
            }
            public void Visit(ASTNode_GetVar node) {
                if (node.address is LocalAddress) {
                    ByteCodes.Add(ByteCodeEnum.PUSH_LOCAL);
                    ByteCodes.Add(((LocalAddress)node.address).index);
                } else if (node.address is GlobalAddress) {
                    ByteCodes.Add(ByteCodeEnum.PUSH_GLOBAL);
                    ByteCodes.Add(((GlobalAddress)node.address).index);
                } else {
                    var address = GetTranslatedFreeAddress((FreeAddress)node.address);
                    ByteCodes.Add(ByteCodeEnum.PUSH_FREE);
                    ByteCodes.Add(address.envIndex);
                    ByteCodes.Add(address.index);
                }
            }
            public void Visit(ASTNode_SetVar node) {
                node.rightNode.AcceptVisitor(this);

                if (node.address is LocalAddress) {
                    ByteCodes.Add(ByteCodeEnum.POP_LOCAL);
                    ByteCodes.Add(((LocalAddress)node.address).index);
                } else if (node.address is GlobalAddress) {
                    ByteCodes.Add(ByteCodeEnum.POP_GLOBAL);
                    ByteCodes.Add(((GlobalAddress)node.address).index);
                } else {
                    var address = GetTranslatedFreeAddress((FreeAddress)node.address);
                    ByteCodes.Add(ByteCodeEnum.POP_FREE);
                    ByteCodes.Add(address.envIndex);
                    ByteCodes.Add(address.index);
                }

                ByteCodes.Add(ByteCodeEnum.PUSH_LITERAL);
                ByteCodes.Add(GetLiteralIndex(null));
            }
            public void Visit(ASTNode_If node) {
                node.predNode.AcceptVisitor(this);

                ByteCodes.Add(ByteCodeEnum.CJMP);
                int cjmpOff = ByteCodes.Count; ByteCodes.Add(0);

                node.elseNode.AcceptVisitor(this);

                ByteCodes.Add(ByteCodeEnum.JMP);
                int jmpOff = ByteCodes.Count; ByteCodes.Add(0);

                ByteCodes[cjmpOff] = ByteCodes.Count;
                node.thenNode.AcceptVisitor(this);

                ByteCodes[jmpOff] = ByteCodes.Count;
            }
            public void Visit(ASTNode_Begin node) {
                for (int i = 0; i < node.nodes.Count - 1; ++i) {
                    node.nodes[i].AcceptVisitor(this);
                    ByteCodes.Add(ByteCodeEnum.POP1);
                }
                node.nodes.Last().AcceptVisitor(this);
            }
            public void Visit(ASTNode_Lambda node) {
                ByteCodes.Add(ByteCodeEnum.PUSH_SCRIPT_PROCEDURE);
                ByteCodes.Add(GetLiteralIndex(CompileToByteCode(node)));
            }
            public void Visit(ASTNode_Application node) {
                node.procedureNode.AcceptVisitor(this);
                foreach (var n in node.actualNodes) {
                    n.AcceptVisitor(this);
                }
                ByteCodes.Add(ByteCodeEnum.CALL);
                ByteCodes.Add(node.actualNodes.Count);
            }
        }

        static ScriptProcedureMeta CompileToByteCode(ASTNode_Lambda node) {
            var generator = new ASTNodeVisitor_ByteCodeGenerator(node);
            return new ScriptProcedureMeta { 
                byteCodes=generator.ByteCodes.ToArray(), 
                freeAdresses=node.freeAddresses.ToArray(), 
                literals=generator.Literals.ToArray(), 
                localVarCount=node.localVarCount,
            };
        }

        //=================================================================
        class Env {
            public Env prevEnv;
            public object[] variables;
        }

        class GlobalEnv {
            public List<object> variables = new List<object>();
            private GlobalEnv() { }
            static private GlobalEnv sInstance = new GlobalEnv();
            static public GlobalEnv Instance() {
                return sInstance;
            }
            public void ReserveVariables(int count) {
                while (variables.Count < count) variables.Add(null);
            }
        }

        class ScriptProcedure {
            public ScriptProcedureMeta meta;
            public Env freeEnv;
            public object[][] freeVariables;
            public ScriptProcedure(Env env, ScriptProcedureMeta _meta) {
                meta = _meta;
                freeEnv = env;

                freeVariables = new object[meta.freeAdresses.Length][];
                for (int i = 0; i < freeVariables.Length; ++i) {
                    var address = meta.freeAdresses[i];
                    Env e = env;
                    for (int j = 1; j < address.envIndex; ++j) e = e.prevEnv;
                    freeVariables[i] = e.variables;
                }
            }
        }

        class StackFrame {
            public ScriptProcedure procedure;
            public Env localEnv;
            public int pc;
        }

        class Stack {
            private List<object> evalStack = new List<object>();
            private Stack<StackFrame> frames = new Stack<StackFrame>();

            public int MaxStackFrameCount { get; set; }
            public int MaxEvalStackDepth { get; set; }

            public Stack() {
                MaxStackFrameCount = 1024 * 1024;
                MaxEvalStackDepth = 1024 * 1024;
            }

            public object CallScriptProcedure(ScriptProcedure procedure, List<object> actuals) {
                int startFrame = frames.Count;
                int startEvalStack = evalStack.Count;

                {
                    Env localEnv = new Env { prevEnv=null, variables=new object[procedure.meta.localVarCount],};
                    for (int i = 0; i < actuals.Count; ++i) localEnv.variables[i] = actuals[i];
                    frames.Push(new StackFrame { procedure=procedure, localEnv=localEnv, pc=0, });
                }

                Label_PeekFrame:
                while (frames.Count > startFrame) {
                    if (frames.Count > MaxStackFrameCount) throw new Exception("Stack frame overflow: " + MaxStackFrameCount);
                    if (evalStack.Count > MaxEvalStackDepth) throw new Exception("Eval stack overflow: " + MaxEvalStackDepth);

                    StackFrame frame = frames.Peek();
                    object[] localVaraibles = frame.localEnv.variables;
                    object[][] freeVariables = frame.procedure.freeVariables;
                    List<object> globalVariables = GlobalEnv.Instance().variables;
                    object[] literals = frame.procedure.meta.literals;
                    int[] byteCodes = frame.procedure.meta.byteCodes;

                    int pc = frame.pc;
                    while (pc < byteCodes.Length) {
                        switch (byteCodes[pc]) {
                            case ByteCodeEnum.PUSH_LITERAL: {
                                    evalStack.Add(literals[byteCodes[pc + 1]]);
                                    pc += 2;
                                }
                                break;
                            case ByteCodeEnum.PUSH_LOCAL: {
                                    evalStack.Add(localVaraibles[byteCodes[pc + 1]]);
                                    pc += 2;
                                }
                                break;
                            case ByteCodeEnum.POP_LOCAL: {
                                    localVaraibles[byteCodes[pc + 1]] = evalStack.Last();
                                    evalStack.RemoveAt(evalStack.Count - 1);
                                    pc += 2;
                                }
                                break;
                            case ByteCodeEnum.PUSH_GLOBAL: {
                                    evalStack.Add(globalVariables[byteCodes[pc + 1]]);
                                    pc += 2;
                                }
                                break;
                            case ByteCodeEnum.POP_GLOBAL: {
                                    globalVariables[byteCodes[pc + 1]] = evalStack.Last();
                                    evalStack.RemoveAt(evalStack.Count - 1);
                                    pc += 2;
                                }
                                break;
                            case ByteCodeEnum.PUSH_FREE: {
                                    evalStack.Add(freeVariables[byteCodes[pc + 1]][byteCodes[pc + 2]]);
                                    pc += 3;
                                }
                                break;
                            case ByteCodeEnum.POP_FREE: {
                                    freeVariables[byteCodes[pc + 1]][byteCodes[pc + 2]] = evalStack.Last();
                                    evalStack.RemoveAt(evalStack.Count - 1);
                                    pc += 3;
                                }
                                break;
                            case ByteCodeEnum.POP1: {
                                    evalStack.RemoveAt(evalStack.Count - 1);
                                    pc += 1;
                                }
                                break;
                            case ByteCodeEnum.CJMP: {
                                    var b = (bool)evalStack.Last();
                                    evalStack.RemoveAt(evalStack.Count - 1);
                                    if (b) pc = byteCodes[pc + 1];
                                    else pc += 2;
                                }
                                break;
                            case ByteCodeEnum.JMP: {
                                    pc = byteCodes[pc + 1];
                                }
                                break;
                            case ByteCodeEnum.PUSH_SCRIPT_PROCEDURE: {
                                evalStack.Add(new ScriptProcedure(frame.localEnv, (ScriptProcedureMeta)literals[byteCodes[pc + 1]]));
                                pc += 2;
                            }
                                break;
                            case ByteCodeEnum.CALL: { 
                                int actualCount = byteCodes[pc + 1];
                                object p = evalStack[evalStack.Count - actualCount - 1];
                                var hostProcuedure = p as HostProcedure;
                                if (hostProcuedure != null) {
                                    hostProcuedure(evalStack, evalStack.Count - actualCount - 1);
                                    evalStack.RemoveRange(evalStack.Count - actualCount, actualCount);
                                    pc += 2;
                                } else {
                                    frame.pc = pc + 2;
                                    if (frame.pc == byteCodes.Length) frames.Pop();

                                    var scriptProcedure = (ScriptProcedure)p;
                                    Env localEnv = new Env { prevEnv=scriptProcedure.freeEnv, variables=new object[scriptProcedure.meta.localVarCount]};
                                    for (int i = 0; i < actualCount; ++i) { 
                                        localEnv.variables[i] = evalStack[evalStack.Count - actualCount + i];
                                    }
                                    evalStack.RemoveRange(evalStack.Count - actualCount - 1, actualCount + 1);

                                    frames.Push(new StackFrame { procedure = scriptProcedure, localEnv = localEnv, pc = 0 });
                                    goto Label_PeekFrame;
                                }
                            }
                                break;
                            default:
                                throw new Exception("Unkown bytecode :" + byteCodes[pc]);
                        }                        
                    }

                    frames.Pop();
                }

                object r = evalStack[startEvalStack];
                evalStack.RemoveAt(startEvalStack);
                return r;
            }
        }

        //=================================================================
        private Stack mStack = new Stack();
        private long mTimerFreq = 0;
        private long mTimerStart = 0;
        private Random mRandom = new Random();

        private ByteCodeInterpreter() {
            Utils.QueryPerformanceCounter(out mTimerStart);
            Utils.QueryPerformanceFrequency(out mTimerFreq);

            Dictionary<string, object> builtinVars = new Dictionary<string, object>() {
                {"true", true},
                {"false", false},
                {"else", true},
                {"null", null},
            };

            Dictionary<string, HostProcedure> builtinProcedures = new Dictionary<string, HostProcedure>() {
                {"not", (stack, ret) => stack[ret]=!(bool)stack[ret+1]},
                {"identity", (stack, ret) => stack[ret]=stack[ret+1]},
                {"sqr", (stack, ret) => {var a = (INumber)stack[ret+1]; stack[ret]=a.Mul(a);} },
                {"+", (stack, ret) => stack[ret]=((INumber)stack[ret+1]).Add((INumber)stack[ret+2])},
                {"-", (stack, ret) => stack[ret]=((INumber)stack[ret+1]).Sub((INumber)stack[ret+2])},
                {"*", (stack, ret) => stack[ret]=((INumber)stack[ret+1]).Mul((INumber)stack[ret+2])},
                {"/", (stack, ret) => stack[ret]=((INumber)stack[ret+1]).Div((INumber)stack[ret+2])},
                {"quotient", (stack, ret) => stack[ret]=((INumber)stack[ret+1]).Div((INumber)stack[ret+2]).CastToInteger()},
                {"remainder", (stack, ret) => stack[ret]=((INumber)stack[ret+1]).Mod((INumber)stack[ret+2])},
                {"=", (stack, ret) => stack[ret]=stack[ret+1].Equals(stack[ret+2])},
                {"<", (stack, ret) => stack[ret]=(stack[ret+1] as IComparable).CompareTo(stack[ret+2]) < 0},
                {"<=", (stack, ret) => stack[ret]=(stack[ret+1] as IComparable).CompareTo(stack[ret+2]) <= 0},
                {">", (stack, ret) => stack[ret]=(stack[ret+1] as IComparable).CompareTo(stack[ret+2]) > 0},
                {">=", (stack, ret) => stack[ret]=(stack[ret+1] as IComparable).CompareTo(stack[ret+2]) >= 0},
                {"eq?", (stack, ret) => stack[ret]=object.ReferenceEquals(stack[ret+1], stack[ret+2])},

                {"cons", (stack, ret) => stack[ret]=new Pair() { Car = stack[ret+1], Cdr = stack[ret+2] }},
                {"car", (stack, ret) => stack[ret]=((Pair)stack[ret+1]).Car},
                {"cdr", (stack, ret) => stack[ret]=((Pair)stack[ret+1]).Cdr},
                {"drop", (stack, ret) => {
                                       Pair l = (Pair)stack[ret+1]; int n = ((NumberInteger)stack[ret+2]).value;
                                       for (; n > 0; --n) {
                                           l = (Pair)l.Cdr;
                                       }
                                       stack[ret] = l;
                                   }},
                {"length", (stack, ret) => {
                                         int n = 0;
                                         for (Pair l = (Pair)stack[ret+1]; l != null; ++n, l = (Pair)l.Cdr) ;
                                         stack[ret] = Number.Create(n);
                                     }},
                {"append", (stack, ret) => {
                                         var l = ListProcess.PairToList((Pair)stack[ret+1]);
                                         l.InsertRange(l.Count, ListProcess.PairToList((Pair)stack[ret+2]));
                                         stack[ret] = ListProcess.ListToPair(l);
                                     }},
                {"empty?", (stack, ret) => stack[ret]=stack[ret+1] == null},

                {"pretty-print", (stack, ret) => {
                                               ListProcess.PrintPairExp(stack[ret+1]);
                                               stack[ret] = null;
                                           }},
                {"display", (stack, ret) => {
                                          ListProcess.PrintListExp(ListProcess.PairExpToListExp(stack[ret+1]));
                                          stack[ret] = null;
                                      }},
                {"current-inexact-milliseconds", (stack, ret) => {
                                                               long now;
                                                               Utils.QueryPerformanceCounter(out now);
                                                               stack[ret] = Number.Create((decimal)(now - mTimerStart) * 1000 / mTimerFreq);
                                                           }},
                {"exit", (stack, ret) => {
                                       Environment.Exit(0);
                                       stack[ret] = null;
                                   }},
                {"random", (stack, ret) => stack[ret] = Number.Create(mRandom.Next(((NumberInteger)stack[ret+1]).value))},
                {"eval", (stack, ret) => stack[ret] = Interpret(ListProcess.PairExpToListExp(stack[ret+1]))},
            };

            GlobalEnv.Instance().ReserveVariables(builtinVars.Count + builtinProcedures.Count);
            foreach (KeyValuePair<string, object> kv in builtinVars) {
                GlobalEnv.Instance().variables[SymbolTable.DefineOrGetGlobalSymbol(kv.Key).index] = kv.Value;
            }
            foreach (KeyValuePair<string, HostProcedure> kv in builtinProcedures) {
                GlobalEnv.Instance().variables[SymbolTable.DefineOrGetGlobalSymbol(kv.Key).index] = kv.Value;
            }
        }

        static private ByteCodeInterpreter sInstance = new ByteCodeInterpreter();
        static public ByteCodeInterpreter Instance() {
            return sInstance;
        }

        public object Interpret(object exp) {
            IASTNode node = ASTCompiler.Compile(null, ListProcess.TransformLibraryForms(exp));
            GlobalEnv.Instance().ReserveVariables(SymbolTable.GetGlobalSymbolCount());

            var lambda = new ASTNode_Lambda { bodyNode = node, localVarCount =0, freeAddresses = new List<FreeAddress>(),};
            return mStack.CallScriptProcedure(new ScriptProcedure(null, CompileToByteCode(lambda)), new List<object> { });
        }

        public void SetMaxStackDepth(int evalStackDepth, int stackFrameCount) {
            mStack.MaxEvalStackDepth = evalStackDepth;
            mStack.MaxStackFrameCount = stackFrameCount;
        }
    }
}
