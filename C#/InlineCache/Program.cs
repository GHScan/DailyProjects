using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;
using System.Diagnostics;
using System.Linq.Expressions;

public interface IInlineCache<T>
{
    string MethodName { get; }
    T Get(object target);
    bool TryGet(object target, out T result);
    IInlineCache<T> Promote(Type newTargetType);
}

public class DefaultCache<T> : IInlineCache<T>
{
    public string MethodName { get; private set; }
    public DefaultCache(string methodName)
    {
        MethodName = methodName;
    }
    public bool TryGet(object target, out T result)
    {
        result = default(T);
        return false;
    }
    public T Get(object target)
    {
        throw new Exception("Not implemention");
    }
    public IInlineCache<T> Promote(Type newTargetType)
    {
        return new MonomorphicCache<T>(MethodName, newTargetType);
    }
}
public class MonomorphicCache<T> : IInlineCache<T>
{
    delegate bool TryGetT(object target, out T result);

    private Type mCachedType;
    private TryGetT mCachedTryGet;
    public string MethodName { get; private set; }
    public MonomorphicCache(string methodName, Type targetType)
    {
        MethodName = methodName;
        mCachedType = targetType;
        mCachedTryGet = BuildCachedTryGet(MethodName, mCachedType);
    }
    public bool TryGet(object target, out T result)
    {
        return mCachedTryGet(target, out result);
    }
    public T Get(object target)
    {
        T value;
        while (!mCachedTryGet(target, out value))
        {
            mCachedType = target.GetType();
            mCachedTryGet = BuildCachedTryGet(MethodName, mCachedType);
        }
        return value;
    }
    public IInlineCache<T> Promote(Type newTargetType)
    {
        return new PolymorphicCache<T>(MethodName, new[] { mCachedType, newTargetType });
    }
    private static TryGetT BuildCachedTryGet(string methodName, Type cachedType)
    {
        var target = Expression.Parameter(typeof(object));
        var result = Expression.Parameter(typeof(T).MakeByRefType());
        var retLabel = Expression.Label(typeof(bool));
        var body = Expression.Block(
            Expression.IfThen(
            Expression.TypeIs(target, cachedType),
            Expression.Block(
            Expression.Assign(result, Expression.Call(Expression.Convert(target, cachedType), cachedType.GetMethod(methodName))),
            Expression.Return(retLabel, Expression.Constant(true)))),
            Expression.Label(retLabel, Expression.Constant(false)));
        return Expression.Lambda<TryGetT>(body, target, result).Compile();
    }
}
public class PolymorphicCache<T> : IInlineCache<T>
{
    delegate bool TryGetT(object target, out T result);
    const int kMaxCachedTypes = 4;

    public string MethodName { get; private set; }
    private List<Type> mCachedTypes = new List<Type>(kMaxCachedTypes);
    private TryGetT mCachedTryGet;
    public PolymorphicCache(string methodName, IEnumerable<Type> targetTypes)
    {
        MethodName = methodName;
        mCachedTypes.AddRange(targetTypes);
        mCachedTryGet = BuildCachedTryGet(MethodName, mCachedTypes);
    }
    public bool TryGet(object target, out T result)
    {
        while (true)
        {
            if (mCachedTryGet(target, out result)) return true;
            if (mCachedTypes.Count >= kMaxCachedTypes) return false;
            mCachedTypes.Add(target.GetType());
            mCachedTryGet = BuildCachedTryGet(MethodName, mCachedTypes);
        }
    }
    public T Get(object target)
    {
        T value;
        while (!TryGet(target, out value))
        {
            mCachedTypes.RemoveAt(mCachedTypes.Count - 1);
            mCachedTypes.Insert(0, target.GetType());
            mCachedTryGet = BuildCachedTryGet(MethodName, mCachedTypes);
        }
        return value;
    }
    public IInlineCache<T> Promote(Type newTargetType)
    {
        return new MegamorphicCache<T>(MethodName, mCachedTypes.Concat(new[] { newTargetType }));
    }
    private static TryGetT BuildCachedTryGet(string methodName, List<Type> cachedTypes)
    {
        var target = Expression.Parameter(typeof(object));
        var result = Expression.Parameter(typeof(T).MakeByRefType());
        var retLabel = Expression.Label(typeof(bool));
        var body = Expression.Block(
            cachedTypes.Select(cachedType =>
                Expression.IfThen(
            Expression.TypeIs(target, cachedType),
            Expression.Block(
            Expression.Assign(result, Expression.Call(Expression.Convert(target, cachedType), cachedType.GetMethod(methodName))),
            Expression.Return(retLabel, Expression.Constant(true))))
            ).Cast<Expression>().Concat(new[] { Expression.Label(retLabel, Expression.Constant(false)) }));
        return Expression.Lambda<TryGetT>(body, target, result).Compile();
    }
}
public class MegamorphicCache<T> : IInlineCache<T>
{
    public string MethodName { get; private set; }
    private Dictionary<Type, Func<object, T>> mCachedGets = new Dictionary<Type, Func<object, T>>(8);
    public MegamorphicCache(string methodName, IEnumerable<Type> targetTypes)
    {
        MethodName = methodName;
        foreach (var targetType in targetTypes)
        {
            mCachedGets.Add(targetType, BuildCachedGet(MethodName, targetType));
        }
    }
    public bool TryGet(object target, out T result)
    {
        result = Get(target);
        return true;
    }
    public T Get(object target)
    {
        var targetType = target.GetType();
        Func<object, T> cachedGet;
        if (!mCachedGets.TryGetValue(targetType, out cachedGet))
        {
            cachedGet = BuildCachedGet(MethodName, targetType);
            mCachedGets.Add(targetType, cachedGet);
        }
        return cachedGet(target);
    }
    public IInlineCache<T> Promote(Type newTargetType)
    {
        throw new Exception("Not implemention");
    }
    private static Func<object, T> BuildCachedGet(string methodName, Type cachedType)
    {
        var target = Expression.Parameter(typeof(object));
        var body = Expression.Call(Expression.Convert(target, cachedType), cachedType.GetMethod(methodName));
        return Expression.Lambda<Func<object, T>>(body, target).Compile();
    }
}
public class CallSite<T>
{
    private IInlineCache<T> mCache;
    public CallSite(string methodName)
    {
        mCache = new DefaultCache<T>(methodName);
    }
    public T Get(object target)
    {
        T value;
        while (!mCache.TryGet(target, out value))
        {
            mCache = mCache.Promote(target.GetType());
        }
        return value;
    }
}

class Program
{
    static string MethodName_Count = "get_Count";
    static string MethodName_Capacity = "get_Capacity";
    static MonomorphicCache<int> MonoCache_Count = new MonomorphicCache<int>(MethodName_Count, typeof(List<int>));
    static MonomorphicCache<int> MonoCache_Capacity = new MonomorphicCache<int>(MethodName_Capacity, typeof(List<int>));
    static PolymorphicCache<int> PolyCache_Count = new PolymorphicCache<int>(MethodName_Count, new Type[0]);
    static PolymorphicCache<int> PolyCache_Capacity = new PolymorphicCache<int>(MethodName_Capacity, new Type[0]);
    static MegamorphicCache<int> MegaCache_Count = new MegamorphicCache<int>(MethodName_Count, new Type[0]);
    static MegamorphicCache<int> MegaCache_Capacity = new MegamorphicCache<int>(MethodName_Capacity, new Type[0]);
    static CallSite<int> CallSite_Count = new CallSite<int>(MethodName_Count);
    static CallSite<int> CallSite_Capacity = new CallSite<int>(MethodName_Capacity);
    public static void UnitTest()
    {
        var objs = new object[] 
        { 
            new List<char>(),
            new List<short> {1,2,5},
            new List<int> {1,2,3,4},
        };

        foreach (var obj in objs)
        {
            int count = (int)obj.GetType().GetMethod(MethodName_Count).Invoke(obj, null);
            Debug.Assert(count == MonoCache_Count.Get(obj));
            Debug.Assert(count == PolyCache_Count.Get(obj));
            Debug.Assert(count == MegaCache_Count.Get(obj));
            Debug.Assert(count == CallSite_Count.Get(obj));

            int capacity = (int)obj.GetType().GetMethod(MethodName_Capacity).Invoke(obj, null);
            Debug.Assert(capacity == MonoCache_Capacity.Get(obj));
            Debug.Assert(capacity == PolyCache_Capacity.Get(obj));
            Debug.Assert(capacity == MegaCache_Capacity.Get(obj));
            Debug.Assert(capacity == CallSite_Capacity.Get(obj));
        }
    }
    public static void Benchmark(Tuple<string, Action<int, object[], int>>[] funcs)
    {
        var objs = new object[]
        {
            new List<bool>(),
            new List<char>(),
            new List<short> {1,2,5},
            new List<int> {1,2,3,4},
            new List<long> {1,2,3,4,5},
            new List<float> {1,2,},
            new List<double> {1,2,3,4,5,6},
            new List<string>(),
            new List<object>(),
            new List<Action>(),
            new List<Func<bool>>(),
            new List<Func<char>>(),
            new List<Func<short>>(),
            new List<Func<int>>(),
            new List<Func<long>>(),
        };

        var typeNs = new[] { 1, 4, objs.Length };
        foreach (var func in funcs)
        {
            foreach (var typeN in typeNs)
            {
                Console.Write(string.Format("{0,20}({1,-2}): ", func.Item1, typeN));
                try
                {
                    Utils.Timeit(10, () =>
                    {
                        func.Item2(100000, objs, typeN);
                    });
                }
                catch (Exception e)
                {
                    var color = Console.ForegroundColor;
                    Console.ForegroundColor = ConsoleColor.Red;
                    Console.Write("failed({0})\n", e.Message);
                    Console.ForegroundColor = color;
                }
            }
        }
    }

    public static void Main(string[] args)
    {
#if  DEBUG
        UnitTest();
#endif
        Benchmark(new[]
            {
                new Tuple<string, Action<int, object[], int>>(
                    "dynamic ",
                    (loop, objs, typeN)=>
                    {
                        for (var i = 0; i < loop; ++i)
                        {
                            dynamic o = objs[i % typeN];
                            var v = o.Count + o.Count * o.Capacity;
                        }
                    }
                ),
                new Tuple<string, Action<int, object[], int>>(
                    "reflection ",
                    (loop, objs, typeN)=>
                    {
                        for (var i = 0; i < loop; ++i)
                        {
                            var o = objs[i % typeN];
                            var mcount = o.GetType().GetMethod(MethodName_Count);
                            var mcapacity = o.GetType().GetMethod(MethodName_Capacity);
                            var v = (int)mcount.Invoke(o, null) + (int)mcount.Invoke(o, null) * (int)mcapacity.Invoke(o, null);
                        }
                    }
                ),
                new Tuple<string, Action<int, object[], int>>(
                    "reflection optmize",
                    (loop, objs, typeN)=>
                    {
                        if (typeN == 1)
                        {
                            var o = objs[0];
                            var mcount = o.GetType().GetMethod(MethodName_Count);
                            var mcapacity = o.GetType().GetMethod(MethodName_Capacity);
                            for (var i = 0; i < loop; ++i)
                            {
                                var v = (int)mcount.Invoke(o, null) + (int)mcount.Invoke(o, null) * (int)mcapacity.Invoke(o, null);
                            }
                        }
                        else
                        {
                            for (var i = 0; i < loop; ++i)
                            {
                                var o = objs[i % typeN];
                                var mcount = o.GetType().GetMethod(MethodName_Count);
                                var mcapacity = o.GetType().GetMethod(MethodName_Capacity);
                                var v = (int)mcount.Invoke(o, null) + (int)mcount.Invoke(o, null) * (int)mcapacity.Invoke(o, null);
                            }
                        }
                    }
                ),
                new Tuple<string, Action<int, object[], int>>(
                    "monomorphic",
                    (loop, objs, typeN)=>
                    {
                        if (typeN > 1) throw new Exception("Out of time");
                        for (var i = 0; i < loop; ++i)
                        {
                            var o = objs[i % typeN];
                            var v = MonoCache_Count.Get(o) + MonoCache_Count.Get(o) * MonoCache_Capacity.Get(o);
                        }
                    }
                ),
                new Tuple<string, Action<int, object[], int>>(
                    "polymorphic",
                    (loop, objs, typeN)=>
                    {
                        if (typeN > 4) throw new Exception("Out of time");
                        for (var i = 0; i < loop; ++i)
                        {
                            var o = objs[i % typeN];
                            var v = PolyCache_Count.Get(o) + PolyCache_Count.Get(o) * PolyCache_Capacity.Get(o);
                        }
                    }
                ),
                new Tuple<string, Action<int, object[], int>>(
                    "megamorphic",
                    (loop, objs, typeN)=>
                    {
                        for (var i = 0; i < loop; ++i)
                        {
                            var o = objs[i % typeN];
                            var v = MegaCache_Count.Get(o) + MegaCache_Count.Get(o) * MegaCache_Capacity.Get(o);
                        }
                    }
                ),
                new Tuple<string, Action<int, object[], int>>(
                    "callsite",
                    (loop, objs, typeN)=>
                    {
                        for (var i = 0; i < loop; ++i)
                        {
                            var o = objs[i % typeN];
                            var v = CallSite_Count.Get(o) + CallSite_Count.Get(o) * CallSite_Capacity.Get(o);
                        }
                    }
                ),
            });
    }
}
