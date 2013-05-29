using System;
using System.Collections.Generic;
using System.Linq;

using System.Diagnostics;
using System.Reflection;
using System.Reflection.Emit;

public class BuildinCodeEmitor {
    static public void emitBuildins(CodeEmitor emitor) {
        emit_print(emitor);
        emit_println(emitor);
        emit_clock(emitor);
        emit_format(emitor);
        emit_push(emitor);
        emit_random(emitor);
    }
    static private void emit_print(CodeEmitor emitor) {
        var funcName = "print";
        var methodBuilder = emitor.TypeBuilder.DefineMethod(funcName, MethodAttributes.Private | MethodAttributes.Static, CallingConventions.Standard, typeof(object), new Type[] { typeof(List<object>) });
        emitor.FuncName2MethodInfo[funcName] = methodBuilder;
        var ilGenerator = methodBuilder.GetILGenerator();
        var loc_i = ilGenerator.DeclareLocal(typeof(int));
        var loc_len = ilGenerator.DeclareLocal(typeof(int));
        // i = 0; len = args.Count;
        ilGenerator.Emit(OpCodes.Ldc_I4_0);
        ilGenerator.Emit(OpCodes.Stloc, loc_i);
        ilGenerator.Emit(OpCodes.Ldarg_0);
        ilGenerator.Emit(OpCodes.Callvirt, typeof(List<object>).GetMethod("get_Count"));
        ilGenerator.Emit(OpCodes.Stloc, loc_len);
        // label_loop:
        var label_loop = ilGenerator.DefineLabel();
        ilGenerator.MarkLabel(label_loop);
        // if (i >= len) goto label_break;
        var label_break = ilGenerator.DefineLabel();
        ilGenerator.Emit(OpCodes.Ldloc, loc_i);
        ilGenerator.Emit(OpCodes.Ldloc, loc_len);
        ilGenerator.Emit(OpCodes.Clt);
        ilGenerator.Emit(OpCodes.Ldc_I4_0);
        ilGenerator.Emit(OpCodes.Ceq);
        ilGenerator.Emit(OpCodes.Brtrue, label_break);
        // System.Console.Write(args[i]); System.Console.Write("\t");
        ilGenerator.Emit(OpCodes.Ldarg_0);
        ilGenerator.Emit(OpCodes.Ldloc, loc_i);
        ilGenerator.Emit(OpCodes.Callvirt, typeof(List<object>).GetMethod("get_Item"));
        ilGenerator.Emit(OpCodes.Call, typeof(System.Console).GetMethod("Write", new Type[]{typeof(object)}));
        ilGenerator.Emit(OpCodes.Ldstr, "\t");
        ilGenerator.Emit(OpCodes.Call, typeof(System.Console).GetMethod("Write",new Type[]{typeof(string)}));
        // ++i;
        ilGenerator.Emit(OpCodes.Ldloc, loc_i);
        ilGenerator.Emit(OpCodes.Ldc_I4_1);
        ilGenerator.Emit(OpCodes.Add);
        ilGenerator.Emit(OpCodes.Stloc, loc_i);
        // goto label_loop;
        ilGenerator.Emit(OpCodes.Br, label_loop);
        // label_break:
        ilGenerator.MarkLabel(label_break);
        // return null;
        ilGenerator.Emit(OpCodes.Ldnull);
    }
    static private void emit_println(CodeEmitor emitor) {
        var funcName = "println";
        var methodBuilder = emitor.TypeBuilder.DefineMethod(funcName, MethodAttributes.Private | MethodAttributes.Static, CallingConventions.Standard, typeof(object), new Type[] { typeof(List<object>) });
        emitor.FuncName2MethodInfo[funcName] = methodBuilder;
        var ilGenerator = methodBuilder.GetILGenerator();
        ilGenerator.Emit(OpCodes.Ldarg_0);
        ilGenerator.Emit(OpCodes.Call, emitor.FuncName2MethodInfo["print"]);
        ilGenerator.Emit(OpCodes.Ldstr, "\n");
        ilGenerator.Emit(OpCodes.Call, typeof(System.Console).GetMethod("Write", new Type[]{typeof(string)}));
        ilGenerator.Emit(OpCodes.Ret);
    }
    static private void emit_clock(CodeEmitor emitor) {
        var funcName = "clock";
        var methodBuilder = emitor.TypeBuilder.DefineMethod(funcName, MethodAttributes.Private | MethodAttributes.Static, CallingConventions.Standard, typeof(object), new Type[] { });
        emitor.FuncName2MethodInfo[funcName] = methodBuilder;
        var ilGenerator = methodBuilder.GetILGenerator();
        var loc_datetime = ilGenerator.DeclareLocal(typeof(DateTime));
        ilGenerator.Emit(OpCodes.Call, typeof(DateTime).GetMethod("get_Now"));
        ilGenerator.Emit(OpCodes.Stloc, loc_datetime);
        ilGenerator.Emit(OpCodes.Ldloca_S, loc_datetime.LocalIndex);
        ilGenerator.Emit(OpCodes.Call, typeof(DateTime).GetMethod("get_Ticks"));
        ilGenerator.Emit(OpCodes.Conv_R8);
        ilGenerator.Emit(OpCodes.Ldc_R8, 10000000.0);
        ilGenerator.Emit(OpCodes.Div);
        ilGenerator.Emit(OpCodes.Box, typeof(double));
        ilGenerator.Emit(OpCodes.Ret);
    }
    static private void emit_format(CodeEmitor emitor) {
        var funcName = "format";
        var methodBuilder = emitor.TypeBuilder.DefineMethod(funcName, MethodAttributes.Private | MethodAttributes.Static, CallingConventions.Standard, typeof(object), new Type[] { typeof(string), typeof(List<object>) });
        emitor.FuncName2MethodInfo[funcName] = methodBuilder;
        var ilGenerator = methodBuilder.GetILGenerator();
        ilGenerator.Emit(OpCodes.Ldarg_0);
        ilGenerator.Emit(OpCodes.Ldarg_1);
        ilGenerator.Emit(OpCodes.Callvirt, typeof(List<object>).GetMethod("ToArray"));
        ilGenerator.Emit(OpCodes.Call, typeof(string).GetMethod("Format", new Type[]{typeof(string), typeof(object[])}));
        ilGenerator.Emit(OpCodes.Ret);
    }
    static private void emit_push(CodeEmitor emitor) {
        var funcName = "push";
        var methodBuilder = emitor.TypeBuilder.DefineMethod(funcName, MethodAttributes.Private | MethodAttributes.Static, CallingConventions.Standard, typeof(object), new Type[] { typeof(List<object>), typeof(object) });
        emitor.FuncName2MethodInfo[funcName] = methodBuilder;
        var ilGenerator = methodBuilder.GetILGenerator();
        ilGenerator.Emit(OpCodes.Ldarg_0);
        ilGenerator.Emit(OpCodes.Ldarg_1);
        ilGenerator.Emit(OpCodes.Callvirt, typeof(List<object>).GetMethod("Add"));
        ilGenerator.Emit(OpCodes.Ldarg_0);
        ilGenerator.Emit(OpCodes.Ret);
    }
    static private void emit_random(CodeEmitor emitor) {
        var funcName = "random";
        var methodBuilder = emitor.TypeBuilder.DefineMethod(funcName, MethodAttributes.Private | MethodAttributes.Static, CallingConventions.Standard, typeof(object), new Type[] { });
        emitor.FuncName2MethodInfo[funcName] = methodBuilder;
        var ilGenerator = methodBuilder.GetILGenerator();
        ilGenerator.Emit(OpCodes.Ldsfld, emitor.GlobalField);
        ilGenerator.Emit(OpCodes.Ldstr, "__s_random");
        ilGenerator.Emit(OpCodes.Callvirt, typeof(Dictionary<string, object>).GetMethod("get_Item"));
        ilGenerator.Emit(OpCodes.Callvirt, typeof(Random).GetMethod("Next", new Type[]{}));
        ilGenerator.Emit(OpCodes.Conv_R8);
        ilGenerator.Emit(OpCodes.Box, typeof(double));
        ilGenerator.Emit(OpCodes.Ret);
    }
}
