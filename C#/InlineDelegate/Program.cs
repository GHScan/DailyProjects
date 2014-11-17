using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;
using System.Diagnostics;

using System.Linq.Expressions;
using System.Reflection;

class Program
{
    public static T Fold<T>(T[] values, Func<T, T, T> op)
    {
        T r = default(T);
        foreach (var v in values) r = op(r, v);
        return r;
    }
    public interface IFoldOperator<T>
    {
        T Op(T a, T b);
    }
    public sealed class IFoldOperator_AddInt : IFoldOperator<int>
    {
        public int Op(int a, int b)
        {
            return a + b;
        }
    }
    public abstract class FoldOperator<T>
    {
        public abstract T Op(T a, T b);
    }
    public sealed class FoldOperator_AddInt : FoldOperator<int>
    {
        public sealed override int Op(int a, int b)
        {
            return a + b;
        }
    }
    public static T Fold2<T>(T[] values, IFoldOperator<T> op)
    {
        T r = default(T);
        foreach (var v in values) r = op.Op(r, v);
        return r;
    }
    public static T Fold3<T, TOp>(T[] values, TOp op) where TOp : IFoldOperator<T>
    {
        T r = default(T);
        foreach (var v in values) r = op.Op(r, v);
        return r;
    }
    public static T Fold4<T>(T[] values, FoldOperator<T> op)
    {
        T r = default(T);
        foreach (var v in values) r = op.Op(r, v);
        return r;
    }
    public static T Fold5<T, TOp>(T[] values, TOp op) where TOp : FoldOperator<T>
    {
        T r = default(T);
        foreach (var v in values) r = op.Op(r, v);
        return r;
    }
    public static Func<T[], T> BuildFold6<T>(Expression<Func<T, T, T>> op)
    {
        var values = Expression.Parameter(typeof(T[]));
        var result = Expression.Parameter(typeof(T));
        var i = Expression.Parameter(typeof(int));
        var breakTarget = Expression.Label(typeof(T));
        var block = Expression.Block(
            new[] { result, i },
            Expression.Loop(
            Expression.IfThenElse(
            Expression.LessThan(i, Expression.ArrayLength(values)),
            Expression.Block(
            op.Parameters,
            Expression.Assign(op.Parameters[0], result),
            Expression.Assign(op.Parameters[1], Expression.ArrayIndex(values, Expression.PostIncrementAssign(i))),
            Expression.Assign(result, op.Body)),
            Expression.Break(breakTarget, result)),
            breakTarget));
        var lambda = Expression.Lambda<Func<T[], T>>(block, values);
        return lambda.Compile();
    }

    public static TDelegate BuildFold7<TDelegate>(string funcSource, string name) where TDelegate : class
    {
        var compiler = new Microsoft.CSharp.CSharpCodeProvider();
        var parms = new System.CodeDom.Compiler.CompilerParameters
        {
            GenerateExecutable = false,
            GenerateInMemory = true
        };
        parms.ReferencedAssemblies.Add("System.dll");

        var result = compiler.CompileAssemblyFromSource(parms, new[]{
            string.Format(
            @"
using System; 
public class MyClass
{{
    {0}
}}
            ", funcSource),});

        if (result.Errors.Count > 0)
        {
            throw new Exception(string.Join("\n", result.Errors));
        }

        return result.CompiledAssembly.GetType("MyClass").GetMethod(name).CreateDelegate(typeof(TDelegate)) as TDelegate;
    }
    public static int SumInt(int[] values)
    {
        int r = 0;
        foreach (var v in values) r += v;
        return r;
    }

    static void Main(string[] args)
    {
        Func<int[], int> Fold6 = BuildFold6<int>((result, i) => result + i);

        var Fold7 = BuildFold7<Func<int[], int>>(@"
            public static int SumInt(int[] values)
            {
                int r = 0;
                foreach (var v in values) r += v;
                return r;
            }
            ", "SumInt");


        var ints = Enumerable.Range(0, 128 * 1024).ToArray();
        const int TIME = 10;
        const int LOOP = 50;


        Utils.Timeit(TIME, () =>
        {
            for (var i = 0; i < LOOP; ++i) Fold(ints, (a, b) => a + b);
        });
        Utils.Timeit(TIME, () =>
        {
            for (var i = 0; i < LOOP; ++i) Fold2(ints, new IFoldOperator_AddInt());
        });
        Utils.Timeit(TIME, () =>
        {
            for (var i = 0; i < LOOP; ++i) Fold3(ints, new IFoldOperator_AddInt());
        });
        Utils.Timeit(TIME, () =>
        {
            for (var i = 0; i < LOOP; ++i) Fold4(ints, new FoldOperator_AddInt());
        });
        Utils.Timeit(TIME, () =>
        {
            for (var i = 0; i < LOOP; ++i) Fold5(ints, new FoldOperator_AddInt());
        });
        Utils.Timeit(TIME, () =>
        {
            for (var i = 0; i < LOOP; ++i) Fold6(ints);
        });
        Utils.Timeit(TIME, () =>
        {
            for (var i = 0; i < LOOP; ++i) Fold7(ints);
        });
        Utils.Timeit(TIME, () =>
        {
            for (var i = 0; i < LOOP; ++i) SumInt(ints);
        });
    }
}
