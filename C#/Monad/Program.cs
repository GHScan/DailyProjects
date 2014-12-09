using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;
using System.Net;
using System.Threading.Tasks;

public static partial class Monad {
    public class Maybe<T> {
        public T Value { get; private set; }
        public Maybe(T value) { Value = value; }
        public override string ToString() { return string.Format("Maybe({0})", Value); }
    }
    public static Maybe<T3> SelectMany<T1, T2, T3>(this Maybe<T1> source, Func<T1, Maybe<T2>> f, Func<T1, T2, T3> result) {
        if (source == null) return null;
        var source2 = f(source.Value);
        if (source2 == null) return null;
        return new Maybe<T3>(result(source.Value, source2.Value));
    }
    public static Maybe<T2> Select<T1, T2>(this Maybe<T1> source, Func<T1, T2> f) {
        return source.SelectMany(v=> new Maybe<T2>(f(v)), (a,b)=>b);
    }
    public static Maybe<T> Where<T>(this Maybe<T> source, Func<T, bool> f) {
        if (source == null) return null;
        return f(source.Value) ? source : null;
    }
}

public static partial class Monad {
    public delegate Action Awaiter<T>(Action<T> k);
    public static Awaiter<T> Await<T>(this Task<T> task) {
        return k => {
            return task.ContinueWith(t => {
                k(t.Result);
            }).Wait;
        };
    }
    public static Awaiter<T3> SelectMany<T1, T2, T3>(this Awaiter<T1> source, Func<T1, Awaiter<T2>> f, Func<T1, T2, T3> result) {
        return k => {
            Action wait1, wait2 = null;

            wait1 = source(v1 => {
                wait2 = f(v1)(v2 => {
                    k(result(v1, v2));
                });
            });

            return () => {
                wait1();
                wait2();
            };
        };
    }
    public static Awaiter<T2> Select<T1, T2>(this Awaiter<T1> source, Func<T1, T2> f) {
        return source.SelectMany(v => new Awaiter<T2>(k => { k(f(v)); return () => { }; }), (a, b) => b);
    }
}

public class Program {
    static void TestMaybeMonad() {
        var l = from i in new Monad.Maybe<int>(3)
                from j in new Monad.Maybe<int>(4)
                where i < j
                let k = ((Func<int>)(() => { Console.WriteLine("hello world"); return i + j; }))()
                select k;
        Console.WriteLine(l);
    }
    static void TestAwaiterMonad() {
        var l = from resp1 in WebRequest.Create("http://www.baidu.com").GetResponseAsync().Await()
                from str1 in new StreamReader(resp1.GetResponseStream()).ReadToEndAsync().Await()
                from resp2 in WebRequest.Create("http://www.qq.com").GetResponseAsync().Await()
                from str2 in new StreamReader(resp2.GetResponseStream()).ReadToEndAsync().Await()
                select str1.Length + str2.Length;
        Task.Run(l((v) => { Console.WriteLine(v); })).Wait();
    }

    public static void Main(string[] args) {
        TestMaybeMonad();
        TestAwaiterMonad();
    }
}
