using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using System.Reflection;
using System.Reflection.Emit;

namespace _6_JIT {
    public class JITInterpreter_DS2 {

        public class SharedValue {
            public object value;
            public SharedValue(object _value) {
                value = _value;
            }
        }
        
        class ASTNodeVisitor_JITCompiler: IASTNodeVisitor {
            public Dictionary<FreeAddress, FieldBuilder> FieldBuilders { get; private set; }
            public TypeBuilder TypeBuilder { get; private set; }
            public MethodBuilder MethodBuilder { get; private set; }
            public ConstructorBuilder ConstructorBuilder { get; private set; }
            private ASTNode_Lambda mLambdaNode;
            private ILGenerator mILGenerator;
            private List<object> mLocalBuilders = new List<object>();
            private Stack<bool> mTaillCallFlags = new Stack<bool>();


            public bool HasThisArgument() {
                return mLambdaNode.HasFreeVaraible();
            }
            
            private void DeclareLocals() {
                var formalBuilders = new List<ParameterBuilder>();
                for (int i = 0; i < mLambdaNode.formalCount; ++i) {
                    formalBuilders.Add(MethodBuilder.DefineParameter(i + 1, ParameterAttributes.In, mLambdaNode.locals[i]));
                }

                var heapLocals = mLambdaNode.childrenFreeAddresses.Where(a=>a.envIndex==0).Select(a=>a.index).OrderBy(a=>a).ToList();

                for (int i = 0; i < mLambdaNode.formalCount; ++i) {
                    if (heapLocals.IndexOf(i) == -1) {
                        mLocalBuilders.Add(formalBuilders[i]);
                    } else {
                        var localBuilder = mILGenerator.DeclareLocal(typeof(SharedValue));
                        if (JITInterpreter_DS2.Instance().HasSymbolInfo()) {
                            localBuilder.SetLocalSymInfo("_" + mLambdaNode.locals[i]);
                        }
                        mLocalBuilders.Add(localBuilder);

                        JITInterpreter_DS2.EmitLoadArg(mILGenerator, HasThisArgument(), formalBuilders[i]);
                        mILGenerator.Emit(OpCodes.Newobj, typeof(SharedValue).GetConstructor(new Type[]{typeof(object)}));
                        JITInterpreter_DS2.EmitStoreLocal(mILGenerator, localBuilder);
                    }
                }

                for (int i = mLambdaNode.formalCount; i < mLambdaNode.locals.Count; ++i) {
                    if (heapLocals.IndexOf(i) == -1) {
                        var localBuilder = mILGenerator.DeclareLocal(typeof(object));
                        if (JITInterpreter_DS2.Instance().HasSymbolInfo()) {
                            localBuilder.SetLocalSymInfo("_" + mLambdaNode.locals[i]);
                        }
                        mLocalBuilders.Add(localBuilder);
                    } else {
                        var localBuilder = mILGenerator.DeclareLocal(typeof(SharedValue));
                        if (JITInterpreter_DS2.Instance().HasSymbolInfo()) {
                            localBuilder.SetLocalSymInfo("_" + mLambdaNode.locals[i]);
                        }
                        mLocalBuilders.Add(localBuilder);

                        mILGenerator.Emit(OpCodes.Ldnull);
                        mILGenerator.Emit(OpCodes.Newobj, typeof(SharedValue).GetConstructor(new Type[] { typeof(object) }));
                        JITInterpreter_DS2.EmitStoreLocal(mILGenerator, localBuilder);
                    }
                }
            }


            private void EmitLoadLocal(LocalAddress address) {
               var builder = mLocalBuilders[address.index];
               if (builder is ParameterBuilder) {
                   JITInterpreter_DS2.EmitLoadArg(mILGenerator, HasThisArgument(), builder as ParameterBuilder);
               } else {
                   LocalBuilder localBuilder = builder as LocalBuilder;
                   if (localBuilder.LocalType == typeof(object)) {
                       JITInterpreter_DS2.EmitLoadLocal(mILGenerator, localBuilder);
                   } else {
                       JITInterpreter_DS2.EmitLoadLocal(mILGenerator, localBuilder);
                       mILGenerator.Emit(OpCodes.Ldfld, typeof(SharedValue).GetField("value"));
                   }
               }
            }
            private void BeginEmitPopLocal(LocalAddress address) {
                var builder = mLocalBuilders[address.index];
                if (builder is ParameterBuilder) {
                    //
                } else {
                    LocalBuilder localBuilder = builder as LocalBuilder;
                    if (localBuilder.LocalType == typeof(object)) {
                        //
                    } else {
                        JITInterpreter_DS2.EmitLoadLocal(mILGenerator, localBuilder);
                    }
                }
            }
            private void EndEmitPopLocal(LocalAddress address) {
                var builder = mLocalBuilders[address.index];
                if (builder is ParameterBuilder) {
                    JITInterpreter_DS2.EmitStoreArg(mILGenerator, HasThisArgument(), builder as ParameterBuilder);
                } else {
                    LocalBuilder localBuilder = builder as LocalBuilder;
                    if (localBuilder.LocalType == typeof(object)) {
                        JITInterpreter_DS2.EmitStoreLocal(mILGenerator, localBuilder);
                    } else {
                        mILGenerator.Emit(OpCodes.Stfld, typeof(SharedValue).GetField("value"));
                    }
                }
            }

            private void EmitLoadFree(FreeAddress address) {
                JITInterpreter_DS2.EmitLoadThis(mILGenerator);
                mILGenerator.Emit(OpCodes.Ldfld, FieldBuilders[address]);
                mILGenerator.Emit(OpCodes.Ldfld, typeof(SharedValue).GetField("value"));
            }
            private void BeginEmitPopFree(FreeAddress address) {
                JITInterpreter_DS2.EmitLoadThis(mILGenerator);
                mILGenerator.Emit(OpCodes.Ldfld, FieldBuilders[address]);
            }
            private void EndEmitPopFree(FreeAddress address) {
                mILGenerator.Emit(OpCodes.Stfld, typeof(SharedValue).GetField("value"));
            }

            public ASTNodeVisitor_JITCompiler(TypeBuilder parentTypeBuilder, ASTNode_Lambda node) {
                mLambdaNode = node;
                TypeBuilder = parentTypeBuilder.DefineNestedType(JITInterpreter_DS2.Instance().GenernateUniqueString("closure"), TypeAttributes.NestedPublic);

                ConstructorBuilder = TypeBuilder.DefineDefaultConstructor(MethodAttributes.Public);

                FieldBuilders = new Dictionary<FreeAddress, FieldBuilder>();
                foreach (var address in mLambdaNode.GetFreeAddresses()) {
                    FieldBuilders[address] = TypeBuilder.DefineField(address.ToString(), typeof(SharedValue), FieldAttributes.Public);
                }

                if (HasThisArgument()) {
                    MethodBuilder = TypeBuilder.DefineMethod(
                        "Invoke",
                        MethodAttributes.Public,
                        CallingConventions.HasThis,
                        typeof(object),
                        Enumerable.Repeat(typeof(object), mLambdaNode.formalCount).ToArray());
                } else {
                    MethodBuilder = TypeBuilder.DefineMethod(
                        "Invoke",
                        MethodAttributes.Static | MethodAttributes.Public,
                        CallingConventions.Standard,
                        typeof(object),
                        Enumerable.Repeat(typeof(object), mLambdaNode.formalCount).ToArray());
                }


                mILGenerator = MethodBuilder.GetILGenerator();

                DeclareLocals();

                mTaillCallFlags.Push(true);
                mLambdaNode.bodyNode.AcceptVisitor(this);
                mTaillCallFlags.Pop();

                mILGenerator.Emit(OpCodes.Ret);

                TypeBuilder.CreateType();
            }
            public void Visit(ASTNode_Literal node) {
                JITInterpreter_DS2.Instance().EmitLoadLiteral(mILGenerator, node.value);
            }
            public void Visit(ASTNode_GetVar node) {
                if (node.address is GlobalAddress) {
                    JITInterpreter_DS2.Instance().EmitLoadGlobal(mILGenerator, (GlobalAddress)node.address);
                } else if (node.address is LocalAddress) {
                    EmitLoadLocal((LocalAddress)node.address);
                } else {
                    EmitLoadFree((FreeAddress)node.address);
                }
            }
            public void Visit(ASTNode_SetVar node) {
                if (node.address is GlobalAddress) {
                    JITInterpreter_DS2.Instance().BeginEmitPopGlobal(mILGenerator, (GlobalAddress)node.address);

                    mTaillCallFlags.Push(false);
                    node.rightNode.AcceptVisitor(this);
                    mTaillCallFlags.Pop();

                    JITInterpreter_DS2.Instance().EndEmitPopGlobal(mILGenerator, (GlobalAddress)node.address);
                } else if (node.address is LocalAddress) {
                    BeginEmitPopLocal((LocalAddress)node.address);

                    mTaillCallFlags.Push(false);
                    node.rightNode.AcceptVisitor(this);
                    mTaillCallFlags.Pop();

                    EndEmitPopLocal((LocalAddress)node.address);
                } else {
                    BeginEmitPopFree((FreeAddress)node.address);

                    mTaillCallFlags.Push(false);
                    node.rightNode.AcceptVisitor(this);
                    mTaillCallFlags.Pop();

                    EndEmitPopFree((FreeAddress)node.address);
                }
                mILGenerator.Emit(OpCodes.Ldnull);
            }
            public void Visit(ASTNode_If node) {
                Label thenLabel = mILGenerator.DefineLabel();
                Label endLabel = mILGenerator.DefineLabel();

                mTaillCallFlags.Push(false);
                node.predNode.AcceptVisitor(this);
                mTaillCallFlags.Pop();

                mILGenerator.Emit(OpCodes.Unbox_Any, typeof(bool));
                mILGenerator.Emit(OpCodes.Brtrue, thenLabel);

                mTaillCallFlags.Push(true);
                node.elseNode.AcceptVisitor(this);
                mTaillCallFlags.Pop();

                mILGenerator.Emit(OpCodes.Br, endLabel);

                mILGenerator.MarkLabel(thenLabel);

                mTaillCallFlags.Push(true);
                node.thenNode.AcceptVisitor(this);
                mTaillCallFlags.Pop();

                mILGenerator.MarkLabel(endLabel);
            }
            public void Visit(ASTNode_Begin node) {
                foreach (var n in node.nodes.Take(node.nodes.Count - 1)) {
                    mTaillCallFlags.Push(false);
                    n.AcceptVisitor(this);
                    mTaillCallFlags.Pop();

                    mILGenerator.Emit(OpCodes.Pop);
                }

                mTaillCallFlags.Push(true);
                node.nodes.Last().AcceptVisitor(this);
                mTaillCallFlags.Pop();
            }
            public void Visit(ASTNode_Lambda node) {
                var childCompiler = new ASTNodeVisitor_JITCompiler(TypeBuilder, node);
                if (childCompiler.MethodBuilder.IsStatic) {
                    mILGenerator.Emit(OpCodes.Ldnull);
                    mILGenerator.Emit(OpCodes.Ldftn, childCompiler.MethodBuilder);
                    mILGenerator.Emit(OpCodes.Newobj, 
                        JITInterpreter_DS2.Instance().GetDelegateType(node.formalCount).
                        GetConstructor(new Type[] { typeof(object), typeof(IntPtr) }));
                } else {
                    mILGenerator.Emit(OpCodes.Newobj, childCompiler.ConstructorBuilder);

                    foreach (var address in node.GetFreeAddresses()) {
                        mILGenerator.Emit(OpCodes.Dup);
                        if (address.envIndex == 1) {
                            JITInterpreter_DS2.EmitLoadLocal(mILGenerator, (LocalBuilder)mLocalBuilders[address.index]);
                        } else {
                            JITInterpreter_DS2.EmitLoadThis(mILGenerator);
                            mILGenerator.Emit(OpCodes.Ldfld, FieldBuilders[address.GetOuterAddress()]);
                        }
                        mILGenerator.Emit(OpCodes.Stfld, childCompiler.FieldBuilders[address]);
                    }

                    mILGenerator.Emit(OpCodes.Ldftn, childCompiler.MethodBuilder);
                    mILGenerator.Emit(OpCodes.Newobj, 
                        JITInterpreter_DS2.Instance().GetDelegateType(node.formalCount)
                        .GetConstructor(new Type[] { typeof(object), typeof(IntPtr) }));
                }
            }
            public void Visit(ASTNode_Application node) {
                var delegateType = JITInterpreter_DS2.Instance().GetDelegateType(node.actualNodes.Count);

                mTaillCallFlags.Push(false);
                node.procedureNode.AcceptVisitor(this);
                mTaillCallFlags.Pop();

                mILGenerator.Emit(OpCodes.Castclass, delegateType);
                foreach (var n in node.actualNodes) {
                    mTaillCallFlags.Push(false);
                    n.AcceptVisitor(this);
                    mTaillCallFlags.Pop();
                }

                if (mTaillCallFlags.All(b => b)) {
                    mILGenerator.Emit(OpCodes.Tailcall);
                    mILGenerator.Emit(OpCodes.Callvirt, delegateType.GetMethod("Invoke"));
                    mILGenerator.Emit(OpCodes.Ret);
                } else {
                    mILGenerator.Emit(OpCodes.Callvirt, delegateType.GetMethod("Invoke"));
                }
            }
        }

        private static JITInterpreter_DS2 sInstance;
        public static JITInterpreter_DS2 Instance() {
            if (sInstance == null) sInstance = new JITInterpreter_DS2();
            return sInstance;
        }

        private long mTimerFreq = 0;
        private long mTimerStart = 0;
        private Random mRandom = new Random();
        private JITInterpreter_DS2() {
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
                        return ListProcess.ConcatListAndPairToNewPair(l, (Pair)b);
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
                generator.Emit(OpCodes.Ldsfld, typeof(JITInterpreter_DS2).GetField("mLiterals"));
                EmitLoadInt(generator, index);
                generator.Emit(OpCodes.Callvirt, typeof(List<object>).GetMethod("get_Item"));
            }
        }

        public static List<object> mGlobalVariables = new List<object>();
        private void EmitLoadGlobal(ILGenerator generator, GlobalAddress address) {
            generator.Emit(OpCodes.Ldsfld, typeof(JITInterpreter_DS2).GetField("mGlobalVariables"));
            EmitLoadInt(generator, address.index);
            generator.Emit(OpCodes.Callvirt, typeof(List<object>).GetMethod("get_Item"));
        }
        private void BeginEmitPopGlobal(ILGenerator generator, GlobalAddress address) {
            generator.Emit(OpCodes.Ldsfld, typeof(JITInterpreter_DS2).GetField("mGlobalVariables"));
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

            var fileName = "____test.exe";
            var assemblyBuilder = AppDomain.CurrentDomain.DefineDynamicAssembly(
                new AssemblyName(GenernateUniqueString("assemly")), AssemblyBuilderAccess.RunAndSave);
            var moduleBuilder = assemblyBuilder.DefineDynamicModule(GenernateUniqueString("module"), fileName, HasSymbolInfo());

            var mainTypeBuilder = moduleBuilder.DefineType(GenernateUniqueString("class"));
            var compiler = new ASTNodeVisitor_JITCompiler(mainTypeBuilder, lambdaNode);
            var type = mainTypeBuilder.CreateType();

            assemblyBuilder.SetEntryPoint(compiler.MethodBuilder, PEFileKinds.ConsoleApplication);
            // assemblyBuilder.Save(fileName);

            return ((Func<object>)type.GetNestedType(compiler.TypeBuilder.Name).GetMethod(compiler.MethodBuilder.Name)
                .CreateDelegate(typeof(Func<object>)))();
        }
    }
}
