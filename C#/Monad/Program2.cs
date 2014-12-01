using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;
using System.Net;
using System.Threading.Tasks;
using System.Threading;

//-------------------------------------------------------------------------
public class FPList<T> {
    public readonly T Value;
    public readonly FPList<T> Next;
    public FPList(T value, FPList<T> next) {
        Value = value;
        Next = next;
    }
}
public static class FPListHelper {
    public static FPList<T> FromArray<T>(T[] a, int off = 0) {
        return off == a.Length ? null : new FPList<T>(a[off], FromArray(a, off + 1));
    }
    public static FPList<T> Create<T>(params T[] args) {
        return FromArray(args);
    }
    public static FPList<int> Range(int end) {
        return Range(0, end, 1);
    }
    public static FPList<int> Range(int begin, int end, int step = 1) {
        if (step > 0) {
            return begin >= end ? null : new FPList<int>(begin, Range(begin + step, end, step));
        } else {
            return begin <= end ? null : new FPList<int>(begin, Range(begin + step, end, step));
        }
    }
    public static T[] ToArray<T>(this FPList<T> head) {
        var a = new T[head.Length()];
        for (var i = 0; i < a.Length; ++i, head = head.Next) {
            a[i] = head.Value;
        }
        return a;
    }
    public static int Length<T>(this FPList<T> head) {
        return head == null ? 0 : 1 + head.Next.Length();
    }
    public static FPList<T> Push<T>(this FPList<T> head, T value) {
        return new FPList<T>(value, head);
    }
    public static FPList<T> Pop<T>(this FPList<T> head) {
        return head.Next;
    }
    public static bool Contain<T>(this FPList<T> head, Func<T, bool> f) {
        return head == null ? false : (f(head.Value) ? true : head.Next.Contain(f));
    }
    public static FPList<T2> Select<T, T2>(this FPList<T> head, Func<T, T2> f) {
        return head == null ? null : new FPList<T2>(f(head.Value), head.Next.Select(f));
    }
    public static FPList<T> Where<T>(this FPList<T> head, Func<T, bool> f) {
        return head == null ? null : (f(head.Value) ? new FPList<T>(head.Value, head.Next.Where(f)) : head.Next.Where(f));
    }
    public static T2 Aggregate<T, T2>(this FPList<T> head, T2 init, Func<T2, T, T2> f) {
        return head == null ? init : head.Next.Aggregate(f(init, head.Value), f);
    }
}
//-------------------------------------------------------------------------
public static partial class Monad {
    public class Maybe<T> {
        public readonly T Value;
        public Maybe(T value) {
            Value = value;
        }
        public override string ToString() {
            return string.Format("Just({0})", Value);
        }
    }
    public static Maybe<T> ToMaybe<T>(this T value) {
        return new Maybe<T>(value);
    }
    public static Maybe<T3> SelectMany<T, T2, T3>(this Maybe<T> source, Func<T, Maybe<T2>> f, Func<T, T2, T3> combine) {
        if (source != null) {
            var source2 = f(source.Value);
            if (source2 != null) {
                return combine(source.Value, source2.Value).ToMaybe();
            }
        }
        return null;
    }
    public static Maybe<T2> Select<T, T2>(this Maybe<T> source, Func<T, T2> f) {
        return source != null ? f(source.Value).ToMaybe() : null;
    }
    public static Maybe<T> Where<T>(this Maybe<T> source, Func<T, bool> f) {
        return source != null && f(source.Value) ? source : null;
    }
}
//-------------------------------------------------------------------------
public static partial class Monad {
    public static FPList<T3> SelectMany<T, T2, T3>(this FPList<T> source, Func<T, FPList<T2>> f, Func<T, T2, T3> combine) {
        FPList<T3> r = null;
        for (; source != null; source = source.Next) {
            for (var source2 = f(source.Value); source2 != null; source2 = source2.Next) {
                r = r.Push(combine(source.Value, source2.Value));
            }
        }
        return r;
    }
}
//-------------------------------------------------------------------------
public static partial class Monad {
    public delegate Tuple<TState, TResult> StateAccess<TState, TResult>(TState state);
    public static StateAccess<TState, T3> SelectMany<TState, T, T2, T3>(this StateAccess<TState, T> source, Func<T, StateAccess<TState, T2>> f, Func<T, T2, T3> combine) {
        return state => {
            var r1 = source(state);
            var r2 = f(r1.Item2)(r1.Item1);
            return new Tuple<TState, T3>(r2.Item1, combine(r1.Item2, r2.Item2));
        };
    }
    public static StateAccess<TState, T2> Select<TState, T, T2>(this StateAccess<TState, T> source, Func<T, T2> f) {
        return state => {
            var r = source(state);
            return new Tuple<TState, T2>(r.Item1, f(r.Item2));
        };
    }
    public static StateAccess<TState, T3> Then<TState, T, T2, T3>(this StateAccess<TState, T> source, StateAccess<TState, T2> source2, Func<T, T2, T3> combine) {
        return source.SelectMany(_ => source2, combine);
    }
}
//-------------------------------------------------------------------------
public static partial class Monad {
    public delegate void CPS<T>(Action<T> k);
    public static CPS<T3> SelectMany<T, T2, T3>(this CPS<T> source, Func<T, CPS<T2>> f, Func<T, T2, T3> combine) {
        return k => source(v => f(v)(v2 => k(combine(v, v2))));
    }
    public static CPS<T2> Select<T, T2>(this CPS<T> source, Func<T, T2> f) {
        return k => source(v => k(f(v)));
    }
    public static CPS<T> ToCPS<T>(this Task<T> task) {
        return k => {
            task.Wait();
            k(task.Result);
        };
    }
}
//-------------------------------------------------------------------------
public class Program {
    static void TestMaybeMonad() {
        var r = from i in 3.ToMaybe()
                from j in 4.ToMaybe()
                where i < j
                from k in ((Func<int>)(() => { Console.WriteLine("TestMaybe"); return 5; }))().ToMaybe()
                select i + j;
        Utils.Print(r);
    }
    static void TestListMonad() {
        var r = from i in FPListHelper.Range(1, 5)
                from j in FPListHelper.Create(i + 1, 5)
                from k in FPListHelper.Create(j + 1, 5)
                where i * i + j * j == k * k
                select new { I = i, J = j, K = k };
        Utils.Print(r.ToArray());
    }
    static void TestStateMonad() {
        Func<string, Monad.StateAccess<FPList<Tuple<string, int>>, int>> read = k => {
            return state => {
                return new Tuple<FPList<Tuple<string, int>>, int>(state, state.Where(n => n.Item1 == k).Value.Item2);
            };
        };
        Func<string, int, Monad.StateAccess<FPList<Tuple<string, int>>, object>> write = (k, v) => {
            return state => {
                var state2 = state.Contain(n => n.Item1 == k)
                    ? state.Select(n => n.Item1 == k ? new Tuple<string, int>(k, v) : n)
                    : state.Push(new Tuple<string, int>(k, v));
                return new Tuple<FPList<Tuple<string, int>>, object>(state2, null);
            };
        };

        var r = from k in write("x", 1)
                    .Then(write("y", 10), (a, b) => a)
                    .Then(read("x"), (a, x) => x)
                    .Then(read("y"), (x, y) => x + y)
                from k2 in write("x", 2)
                    .Then(read("x"), (a, x) => x)
                    .Then(read("y"), (x, y) => x + y)
                select new { K = k, K2 = k2 };
        Utils.Print(r(null).Item2);
    }
    static void TestCPSMonad() {
        var r = from resp1 in WebRequest.Create("http://www.baidu.com").GetResponseAsync().ToCPS()
                from str1 in new StreamReader(resp1.GetResponseStream()).ReadToEndAsync().ToCPS()
                from resp2 in WebRequest.Create("http://www.qq.com").GetResponseAsync().ToCPS()
                from str2 in new StreamReader(resp2.GetResponseStream()).ReadToEndAsync().ToCPS()
                select str1.Length + str2.Length;
        r(v => Utils.Print(v));
    }
    public static void Main(string[] args) {
        TestMaybeMonad();
        TestListMonad();
        TestStateMonad();
        TestCPSMonad();
    }
}
