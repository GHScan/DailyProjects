using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using System.Reflection;
using System.Reflection.Emit;

namespace _6_JIT {
    public class JITInterpreter_DS {

        class ASTNodeVisitor_JITCompiler: IASTNodeVisitor {
            public MethodBuilder MethodBuilder { get; private set; }
            private TypeBuilder mEnvTypeBuilder;
            private ASTNode_Lambda mLambdaNode;
            private TypeBuilder mHeapEnvTypeBuilder;
            private ILGenerator mILGenerator;
            private List<ParameterBuilder> mArgBuilders = new List<ParameterBuilder>();
            private List<LocalBuilder> mLocalBuilders = new List<LocalBuilder>();
            private Dictionary<string, FieldBuilder> mHeapEnvFieldBuilders = new Dictionary<string, FieldBuilder>();
            private LocalBuilder mHeapEnvBuilder;
            private ASTNodeVisitor_JITCompiler mParent;
            private Stack<bool> mTailCallFlags = new Stack<bool>();

            static private string HEAP_ENV_LOCAL_NAME = "____heapEnv";
            static private string PREV_ENV_FIELD_NAME = "____prevEnv";

            private bool HasThisArgument() {
                return mLambdaNode.HasFreeVaraible();
            }
            
            private void DeclareArguments() {
                int argIndex = 1;
                for (int i = 0; i < mLambdaNode.formalCount; ++i) {
                    mArgBuilders.Add(MethodBuilder.DefineParameter(argIndex++, ParameterAttributes.In, mLambdaNode.locals[i]));
                }
            }
            private void DeclareLocals() {
                for (var i = mLambdaNode.formalCount; i < mLambdaNode.locals.Count; ++i) {
                    var localBuilder = mILGenerator.DeclareLocal(typeof(object));
                    if (JITInterpreter_DS.Instance().HasSymbolInfo()) {
                        localBuilder.SetLocalSymInfo(mLambdaNode.locals[i]);
                    }
                    mLocalBuilders.Add(localBuilder);
                }
            }

            private bool HasHeapEnv() {
                return mLambdaNode.childrenFreeAddresses.Count > 0;
            }
            private void EmitInitHeapEnv() {
                if (!HasHeapEnv()) return;

                mHeapEnvBuilder = mILGenerator.DeclareLocal(mHeapEnvTypeBuilder);
                if (JITInterpreter_DS.Instance().HasSymbolInfo()) {
                    mHeapEnvBuilder.SetLocalSymInfo(HEAP_ENV_LOCAL_NAME);
                }

                var constructor = mHeapEnvTypeBuilder.DefineDefaultConstructor(MethodAttributes.Public);
                mILGenerator.Emit(OpCodes.Newobj, constructor);
                EmitStoreLocal(mILGenerator, mHeapEnvBuilder);

                if (mLambdaNode.childrenFreeAddresses.Where(v => v.envIndex > 0).GetEnumerator().MoveNext()) {
                    var fieldBuilder = mHeapEnvTypeBuilder.DefineField(PREV_ENV_FIELD_NAME, mEnvTypeBuilder, FieldAttributes.Public);
                    mHeapEnvFieldBuilders[PREV_ENV_FIELD_NAME] = fieldBuilder;
                    JITInterpreter_DS.EmitLoadLocal(mILGenerator, mHeapEnvBuilder);
                    JITInterpreter_DS.EmitLoadThis(mILGenerator);
                    mILGenerator.Emit(OpCodes.Stfld, fieldBuilder);
                }

                var heapLocals = mLambdaNode.childrenFreeAddresses.Where(a=>a.envIndex == 0).Select(a=>a.index).OrderBy(a=>a).ToList();
                foreach (var index in heapLocals) {
                    var fieldBuilder = mHeapEnvTypeBuilder.DefineField(mLambdaNode.locals[index], typeof(object), FieldAttributes.Public);
                    mHeapEnvFieldBuilders[mLambdaNode.locals[index]] = fieldBuilder;

                    if (index < mLambdaNode.formalCount) {
                        JITInterpreter_DS.EmitLoadLocal(mILGenerator, mHeapEnvBuilder);
                        JITInterpreter_DS.EmitLoadArg(mILGenerator, HasThisArgument(), mArgBuilders[index]);
                        mILGenerator.Emit(OpCodes.Stfld, fieldBuilder);
                    }
                }
            }


            private void EmitLoadLocal(LocalAddress address) {
                string localName = mLambdaNode.locals[address.index];
                if (mHeapEnvFieldBuilders.ContainsKey(localName)) {
                    JITInterpreter_DS.EmitLoadLocal(mILGenerator, mHeapEnvBuilder);
                    mILGenerator.Emit(OpCodes.Ldfld, mHeapEnvFieldBuilders[localName]);
                } else if (address.index < mLambdaNode.formalCount) {
                    JITInterpreter_DS.EmitLoadArg(mILGenerator, HasThisArgument(), mArgBuilders[address.index]);
                } else {
                    JITInterpreter_DS.EmitLoadLocal(mILGenerator, mLocalBuilders[address.index - mLambdaNode.formalCount]);
                }
            }
            private void BeginEmitPopLocal(LocalAddress address) {
                string localName = mLambdaNode.locals[address.index];
                if (mHeapEnvFieldBuilders.ContainsKey(localName)) {
                    JITInterpreter_DS.EmitLoadLocal(mILGenerator, mHeapEnvBuilder);
                } else if (address.index < mLambdaNode.formalCount) {
                    //
                } else {
                    //
                }
            }
            private void EndEmitPopLocal(LocalAddress address) {
                string localName = mLambdaNode.locals[address.index];
                if (mHeapEnvFieldBuilders.ContainsKey(localName)) {
                    mILGenerator.Emit(OpCodes.Stfld, mHeapEnvFieldBuilders[localName]);
                } else if (address.index < mLambdaNode.formalCount) {
                    JITInterpreter_DS.EmitStoreArg(mILGenerator, HasThisArgument(), mArgBuilders[address.index]);
                } else {
                    JITInterpreter_DS.EmitStoreLocal(mILGenerator, mLocalBuilders[address.index - mLambdaNode.formalCount]);
                }
            }

            private FieldBuilder EmitLoadFreeEnv(FreeAddress address) {
                JITInterpreter_DS.EmitLoadThis(mILGenerator);
                ASTNodeVisitor_JITCompiler visitor = mParent;
                for (int i = 1; i < address.envIndex; ++i) {
                    mILGenerator.Emit(OpCodes.Ldfld, visitor.mHeapEnvFieldBuilders[PREV_ENV_FIELD_NAME]);
                    visitor = visitor.mParent;
                }
                return visitor.mHeapEnvFieldBuilders[visitor.mLambdaNode.locals[address.index]];
            }
            private void EmitLoadFree(FreeAddress address) {
                var field = EmitLoadFreeEnv(address);
                mILGenerator.Emit(OpCodes.Ldfld, field);
            }
            private FieldBuilder BeginEmitPopFree(FreeAddress address) {
                return EmitLoadFreeEnv(address);
            }
            private void EndEmitPopFree(FreeAddress address, FieldBuilder field) {
                mILGenerator.Emit(OpCodes.Stfld, field);
            }

            public ASTNodeVisitor_JITCompiler(ASTNodeVisitor_JITCompiler parent, TypeBuilder envTypeBuilder, ASTNode_Lambda node) {
                mParent = parent;
                mEnvTypeBuilder = envTypeBuilder;
                mLambdaNode = node;

                mHeapEnvTypeBuilder = envTypeBuilder.DefineNestedType(JITInterpreter_DS.Instance().GenernateUniqueString("nested_class"));

                if (HasThisArgument()) {
                    MethodBuilder = envTypeBuilder.DefineMethod(
                        JITInterpreter_DS.Instance().GenernateUniqueString("method"),
                        MethodAttributes.Public,
                        CallingConventions.HasThis,
                        typeof(object),
                        Enumerable.Repeat(typeof(object), mLambdaNode.formalCount).ToArray());
                } else {
                    MethodBuilder = envTypeBuilder.DefineMethod(
                        JITInterpreter_DS.Instance().GenernateUniqueString("static_method"),
                        MethodAttributes.Static | MethodAttributes.Public,
                        CallingConventions.Standard,
                        typeof(object),
                        Enumerable.Repeat(typeof(object), mLambdaNode.formalCount).ToArray());
                }


                mILGenerator = MethodBuilder.GetILGenerator();

                DeclareArguments();
                DeclareLocals();
                EmitInitHeapEnv();

                mTailCallFlags.Push(true);
                mLambdaNode.bodyNode.AcceptVisitor(this);
                mTailCallFlags.Pop();

                mILGenerator.Emit(OpCodes.Ret);
                mHeapEnvTypeBuilder.CreateType();
            }
            public void Visit(ASTNode_Literal node) {
                JITInterpreter_DS.Instance().EmitLoadLiteral(mILGenerator, node.value);
            }
            public void Visit(ASTNode_GetVar node) {
                if (node.address is GlobalAddress) {
                    JITInterpreter_DS.Instance().EmitLoadGlobal(mILGenerator, (GlobalAddress)node.address);
                } else if (node.address is LocalAddress) {
                    EmitLoadLocal((LocalAddress)node.address);
                } else {
                    EmitLoadFree((FreeAddress)node.address);
                }
            }
            public void Visit(ASTNode_SetVar node) {
                if (node.address is GlobalAddress) {
                    JITInterpreter_DS.Instance().BeginEmitPopGlobal(mILGenerator, (GlobalAddress)node.address);

                    mTailCallFlags.Push(false);
                    node.rightNode.AcceptVisitor(this);
                    mTailCallFlags.Pop();

                    JITInterpreter_DS.Instance().EndEmitPopGlobal(mILGenerator, (GlobalAddress)node.address);
                } else if (node.address is LocalAddress) {
                    BeginEmitPopLocal((LocalAddress)node.address);

                    mTailCallFlags.Push(false);
                    node.rightNode.AcceptVisitor(this);
                    mTailCallFlags.Pop();

                    EndEmitPopLocal((LocalAddress)node.address);
                } else {
                    var context = BeginEmitPopFree((FreeAddress)node.address);

                    mTailCallFlags.Push(false);
                    node.rightNode.AcceptVisitor(this);
                    mTailCallFlags.Pop();

                    EndEmitPopFree((FreeAddress)node.address, context);
                }
                mILGenerator.Emit(OpCodes.Ldnull);
            }
            public void Visit(ASTNode_If node) {
                Label thenLabel = mILGenerator.DefineLabel();
                Label endLabel = mILGenerator.DefineLabel();

                mTailCallFlags.Push(false);
                node.predNode.AcceptVisitor(this);
                mTailCallFlags.Pop();
                mILGenerator.Emit(OpCodes.Unbox_Any, typeof(bool));
                mILGenerator.Emit(OpCodes.Brtrue, thenLabel);

                mTailCallFlags.Push(true);
                node.elseNode.AcceptVisitor(this);
                mTailCallFlags.Pop();
                mILGenerator.Emit(OpCodes.Br, endLabel);

                mILGenerator.MarkLabel(thenLabel);
                mTailCallFlags.Push(true);
                node.thenNode.AcceptVisitor(this);
                mTailCallFlags.Pop();

                mILGenerator.MarkLabel(endLabel);
            }
            public void Visit(ASTNode_Begin node) {
                foreach (var n in node.nodes.Take(node.nodes.Count - 1)) {
                    mTailCallFlags.Push(false);
                    n.AcceptVisitor(this);
                    mTailCallFlags.Pop();
                    mILGenerator.Emit(OpCodes.Pop);
                }

                mTailCallFlags.Push(true);
                node.nodes.Last().AcceptVisitor(this);
                mTailCallFlags.Pop();
            }
            public void Visit(ASTNode_Lambda node) {
                var methodBuilder = new ASTNodeVisitor_JITCompiler(this, mHeapEnvTypeBuilder, node).MethodBuilder;
                if (methodBuilder.IsStatic) {
                    mILGenerator.Emit(OpCodes.Ldnull);
                    mILGenerator.Emit(OpCodes.Ldftn, methodBuilder);
                    mILGenerator.Emit(OpCodes.Newobj, 
                        JITInterpreter_DS.Instance().GetDelegateType(node.formalCount).
                        GetConstructor(new Type[] { typeof(object), typeof(IntPtr) }));
                } else {
                    JITInterpreter_DS.EmitLoadLocal(mILGenerator, mHeapEnvBuilder);
                    mILGenerator.Emit(OpCodes.Ldftn, methodBuilder);
                    mILGenerator.Emit(OpCodes.Newobj, 
                        JITInterpreter_DS.Instance().GetDelegateType(node.formalCount)
                        .GetConstructor(new Type[] { typeof(object), typeof(IntPtr) }));
                }
            }
            public void Visit(ASTNode_Application node) {
                var delegateType = JITInterpreter_DS.Instance().GetDelegateType(node.actualNodes.Count);

                mTailCallFlags.Push(false);
                node.procedureNode.AcceptVisitor(this);
                mTailCallFlags.Pop();

                mILGenerator.Emit(OpCodes.Castclass, delegateType);
                foreach (var n in node.actualNodes) {
                    mTailCallFlags.Push(false);
                    n.AcceptVisitor(this);
                    mTailCallFlags.Pop();
                }

                if (mTailCallFlags.All(b => b)) {
                    mILGenerator.Emit(OpCodes.Tailcall);
                    mILGenerator.Emit(OpCodes.Callvirt, delegateType.GetMethod("Invoke"));
                    mILGenerator.Emit(OpCodes.Ret);
                } else {
                    mILGenerator.Emit(OpCodes.Callvirt, delegateType.GetMethod("Invoke"));
                }
            }
        }

        private static JITInterpreter_DS sInstance;
        public static JITInterpreter_DS Instance() {
            if (sInstance == null) sInstance = new JITInterpreter_DS();
            return sInstance;
        }

        private long mTimerFreq = 0;
        private long mTimerStart = 0;
        private Random mRandom = new Random();
        private JITInterpreter_DS() {
            Utils.QueryPerformanceCounter(out mTimerStart);
            Utils.QueryPerformanceFrequency(out mTimerFreq);

            Dictionary<string, object> builtinVars = new Dictionary<string, object>() {
                {"true", true},
                {"false", false},
                {"else", true},
                {"null", null},
            };
            Dictionary<string, object> builtinProcedures = new Dictionary<string, object>() {
                {"not", (Func<object, object>)(a=>!(bool)a)},
                {"identity", (Func<object, object>)(a=>a)},
                {"sqr", (Func<object, object>)(a=>{ var v = (INumber)a; return v.Mul(v); }) },
                {"+", (Func<object, object, object>)((a,b)=>((INumber)a).Add((INumber)b))},
                {"-", (Func<object, object, object>)((a,b)=>((INumber)a).Sub((INumber)b))},
                {"*", (Func<object, object, object>)((a,b)=>((INumber)a).Mul((INumber)b))},
                {"/", (Func<object, object, object>)((a,b)=>((INumber)a).Div((INumber)b))},
                {"quotient", (Func<object, object, object>)((a,b)=>((INumber)a).Div((INumber)b).CastToInteger())},
                {"remainder", (Func<object, object, object>)((a,b)=>((INumber)a).Mod((INumber)b))},
                {"=", (Func<object, object, object>)((a,b)=>((INumber)a).CompareTo((INumber)b)==0)},
                {"<", (Func<object, object, object>)((a,b)=>((INumber)a).CompareTo((INumber)b)<0)},
                {"<=", (Func<object, object, object>)((a,b)=>((INumber)a).CompareTo((INumber)b)<=0)},
                {">", (Func<object, object, object>)((a,b)=>((INumber)a).CompareTo((INumber)b)>0)},
                {">=", (Func<object, object, object>)((a,b)=>((INumber)a).CompareTo((INumber)b)>=0)},
                {"eq?", (Func<object, object, object>)((a,b)=>object.ReferenceEquals(a,b))},
                {"equal?", (Func<object, object, object>)((a,b)=>object.Equals(a,b))},

                {"cons", (Func<object, object, object>)((a, b) => new Pair() { Car = a, Cdr = b })},
                {"car", (Func<object, object>)(a=>((Pair)a).Car)},
                {"cdr", (Func<object, object>)(a=>((Pair)a).Cdr)},
                {"drop", (Func<object, object, object>)((a, b) => {
                        Pair l = (Pair)a; int n = ((NumberInteger)b).value;
                        for (; n > 0; --n) {
                        l = (Pair)l.Cdr;
                        }
                        return l;
                        })},
                {"length", (Func<object, object>)(a=>{
                        int n = 0;
                        for (Pair l = (Pair)a; l != null; ++n, l = (Pair)l.Cdr) ;
                        return Number.Create(n);
                        })},
                {"append", (Func<object, object, object>)((a, b) => {
                        var l = ListProcess.PairToList((Pair)a);
                        l.AddRange(ListProcess.PairToList((Pair)b));
                        return ListProcess.ListToPair(l);
                        })},
                {"empty?", (Func<object, object>)(a=>a==null)},
                {"void", (Func<object>)(()=>null)},

                {"pretty-print", (Func<object, object>)(a=>{
                        ListProcess.PrintPairExp(a);
                        return null;
                        })},
                {"display", (Func<object, object>)(a=>{
                        ListProcess.PrintListExp(ListProcess.PairExpToListExp(a));
                        return null;
                        })},
                {"current-inexact-milliseconds", (Func<object>)(()=>{
                        long now;
                        Utils.QueryPerformanceCounter(out now);
                        return Number.Create((decimal)(now - mTimerStart) * 1000 / mTimerFreq);
                        })},
                {"exit", (Func<object>)(()=>{
                        Environment.Exit(0);
                        return null;
                        })},
                {"random", (Func<object, object>)(a=>Number.Create(mRandom.Next(((NumberInteger)a).value)))},
                {"eval", (Func<object, object>)(a=>{ return Instance().Interpret(ListProcess.PairExpToListExp(a)); })},
            };

            ReserveGlobalVaraible(builtinVars.Count + builtinProcedures.Count);
            foreach (KeyValuePair<string, object> kv in builtinVars) {
                mGlobalVariables[SymbolTable.DefineOrGetGlobalSymbol(kv.Key).index] = kv.Value;
            }
            foreach (KeyValuePair<string, object> kv in builtinProcedures) {
                mGlobalVariables[SymbolTable.DefineOrGetGlobalSymbol(kv.Key).index] = kv.Value;
            }
        }


        private int mNextUniqueID = 0;
        public string GenernateUniqueString(string prefix) {
            return string.Format("{0}_{1}", prefix, mNextUniqueID++);
        }

        private static List<Type> LookupDelegateTypes() {
            List<Type> templates = new List<Type>();
            foreach (var assembly in AppDomain.CurrentDomain.GetAssemblies()) {
                foreach (var type in assembly.GetTypes()) {
                    if (type.ToString().Contains("System.Func")) {
                        templates.Add(type);
                    }
                }
                if (templates.Count > 0) break;
            }
            return templates.OrderBy(d => d.GetGenericArguments().Length)
                .Select(d=>d.MakeGenericType(Enumerable.Repeat(typeof(object), d.GetGenericArguments().Length).ToArray()))
                .ToList();
        }
        private List<Type> mDelegateTypes = LookupDelegateTypes();
        
        public Type GetDelegateType(int formalCount) {
            return mDelegateTypes[formalCount];
        }
       
        public static void EmitLoadInt(ILGenerator generator, int i) {
            if (i == 0) generator.Emit(OpCodes.Ldc_I4_0);
            else if (i == 1) generator.Emit(OpCodes.Ldc_I4_1);
            else if (i == -1) generator.Emit(OpCodes.Ldc_I4_M1);
            else if (i <= SByte.MaxValue && i >= SByte.MinValue) generator.Emit(OpCodes.Ldc_I4_S, (SByte)i);
            else generator.Emit(OpCodes.Ldc_I4, i);
        }
        public static void EmitLoadLocal(ILGenerator generator, LocalBuilder builder) {
            if (builder.LocalIndex == 0) generator.Emit(OpCodes.Ldloc_0);
            else if (builder.LocalIndex == 1) generator.Emit(OpCodes.Ldloc_1);
            else if (builder.LocalIndex == 2) generator.Emit(OpCodes.Ldloc_2);
            else if (builder.LocalIndex == 3) generator.Emit(OpCodes.Ldloc_3);
            else if (builder.LocalIndex <= SByte.MaxValue) generator.Emit(OpCodes.Ldloc_S, (SByte)builder.LocalIndex);
            else generator.Emit(OpCodes.Ldloc, builder.LocalIndex);
        }
        public static void EmitStoreLocal(ILGenerator generator, LocalBuilder builder) {
            if (builder.LocalIndex == 0) generator.Emit(OpCodes.Stloc_0);
            else if (builder.LocalIndex == 1) generator.Emit(OpCodes.Stloc_1);
            else if (builder.LocalIndex == 2) generator.Emit(OpCodes.Stloc_2);
            else if (builder.LocalIndex == 3) generator.Emit(OpCodes.Stloc_3);
            else if (builder.LocalIndex <= SByte.MaxValue) generator.Emit(OpCodes.Stloc_S, (SByte)builder.LocalIndex);
            else generator.Emit(OpCodes.Stloc, builder.LocalIndex);
        }
        public static void EmitLoadThis(ILGenerator generator) {
            generator.Emit(OpCodes.Ldarg_0);
        }
        public static void EmitLoadArg(ILGenerator generator, bool hasThis, ParameterBuilder builder) {
            int index = hasThis ? builder.Position : builder.Position - 1;
            if (index == 0) generator.Emit(OpCodes.Ldarg_0);
            else if (index == 1) generator.Emit(OpCodes.Ldarg_1);
            else if (index == 2) generator.Emit(OpCodes.Ldarg_2);
            else if (index == 3) generator.Emit(OpCodes.Ldarg_3);
            else if (index <= SByte.MaxValue) generator.Emit(OpCodes.Ldarg_S, (SByte)index);
            else generator.Emit(OpCodes.Ldarg, index);
        }
        public static void EmitStoreArg(ILGenerator generator, bool hasThis, ParameterBuilder builder) {
            int index = hasThis ? builder.Position : builder.Position - 1;
            if (index <= SByte.MaxValue) generator.Emit(OpCodes.Starg_S, (SByte)index);
            else generator.Emit(OpCodes.Starg, index);
        }


        public static List<object> mLiterals = new List<object>();
        private static Dictionary<object, int> mLiteral2Index = new Dictionary<object, int>();
        private static int mNextLiteralIndedx = 0;
        private static int GetLiteralIndedx(object literal) {
            if (mLiteral2Index.ContainsKey(literal)) return mLiteral2Index[literal];
            int index = mNextLiteralIndedx++;
            mLiteral2Index[literal] = index;
            mLiterals.Add(literal);
            return index;
        }
        public void EmitLoadLiteral(ILGenerator generator, object literal) {
            if (literal == null) {
                generator.Emit(OpCodes.Ldnull);
            } else if (literal is bool) {
                if ((bool)literal) generator.Emit(OpCodes.Ldc_I4_1);
                else generator.Emit(OpCodes.Ldc_I4_0);
                generator.Emit(OpCodes.Box, typeof(bool));
            } else if (literal is NumberInteger) {
                EmitLoadInt(generator, ((NumberInteger)literal).value);
                generator.Emit(OpCodes.Call, typeof(Number).GetMethod("Create", new Type[]{typeof(int)}));
            } else {
                int index = GetLiteralIndedx(literal);
                generator.Emit(OpCodes.Ldsfld, typeof(JITInterpreter_DS).GetField("mLiterals"));
                EmitLoadInt(generator, index);
                generator.Emit(OpCodes.Callvirt, typeof(List<object>).GetMethod("get_Item"));
            }
        }

        public static List<object> mGlobalVariables = new List<object>();
        private void EmitLoadGlobal(ILGenerator generator, GlobalAddress address) {
            generator.Emit(OpCodes.Ldsfld, typeof(JITInterpreter_DS).GetField("mGlobalVariables"));
            EmitLoadInt(generator, address.index);
            generator.Emit(OpCodes.Callvirt, typeof(List<object>).GetMethod("get_Item"));
        }
        private void BeginEmitPopGlobal(ILGenerator generator, GlobalAddress address) {
            generator.Emit(OpCodes.Ldsfld, typeof(JITInterpreter_DS).GetField("mGlobalVariables"));
            EmitLoadInt(generator, address.index);
        }
        private void EndEmitPopGlobal(ILGenerator generator, GlobalAddress address) {
            generator.Emit(OpCodes.Callvirt, typeof(List<object>).GetMethod("set_Item"));
        }


        private void ReserveGlobalVaraible(int count) {
            while (mGlobalVariables.Count < count) mGlobalVariables.Add(null);
        }

        public bool HasSymbolInfo() {
#if DEBUG
            return true;
#else
            return false;
#endif
        }

        public object Interpret(object exp) {
            IASTNode node = ASTCompiler.Compile(null, ListProcess.TransformLibraryForms(exp));
            ReserveGlobalVaraible(SymbolTable.GetGlobalSymbolCount());
            ASTNode_Lambda lambdaNode = new ASTNode_Lambda { 
                bodyNode = node, bodyFreeAddresses = new List<FreeAddress>(), childrenFreeAddresses = new List<FreeAddress>(),
                formalCount=0, locals=new List<string>(),
            };

            var fileName = GenernateUniqueString("testFile.exe");
            var assemblyBuilder = AppDomain.CurrentDomain.DefineDynamicAssembly(
                new AssemblyName(GenernateUniqueString("assemly")), AssemblyBuilderAccess.RunAndSave);
            var moduleBuilder = assemblyBuilder.DefineDynamicModule(GenernateUniqueString("module"), fileName, HasSymbolInfo());
            var envTypeBuilder = moduleBuilder.DefineType(GenernateUniqueString("class"));

            var methodBuilder = new ASTNodeVisitor_JITCompiler(null, envTypeBuilder, lambdaNode).MethodBuilder;

            var type = envTypeBuilder.CreateType();
            assemblyBuilder.SetEntryPoint(methodBuilder, PEFileKinds.ConsoleApplication);
            //assemblyBuilder.Save(fileName);

            return ((Func<object>)type.GetMethod(methodBuilder.Name).CreateDelegate(typeof(Func<object>)))();
        }
    }
}
