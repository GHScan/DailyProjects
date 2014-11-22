using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;

using System.Linq.Expressions;

namespace CSharp13 {
    public class Program {
        public static int Filter<T>(T[] result, T[] values, Func<T, bool> f) {
            int len = 0;
            foreach (var v in values) {
                if (f(v)) result[len++] = v;
            }
            return len;
        }
        public static void Map<T, T2>(T2[] result, T[] values, Func<T, T2> f) {
            var len = 0;
            foreach (var v in values) result[len++] = f(v);
        }
        public static T2 Reduce<T, T2>(T[] values, Func<T, T2, T2> f, T2 init = default(T2)) {
            foreach (var v in values) init = f(v, init);
            return init;
        }

        private class Filter2Cache<T> {
            private static Dictionary<string, Func<T[], T[], int>> Cache = new Dictionary<string, Func<T[], T[], int>>();
            public static Func<T[], T[], int> Lookup(Expression<Func<T, bool>> exp) {
                string expStr = exp.ToString();
                Func<T[], T[], int> f;
                if (Cache.TryGetValue(expStr, out f)) return f;
                f = Build(exp);
                Cache.Add(expStr, f);
                return f;
            }
            private static Func<T[], T[], int> Build(Expression<Func<T, bool>> exp) {
                var result = Expression.Parameter(typeof(T[]));
                var values = Expression.Parameter(typeof(T[]));
                var len = Expression.Parameter(typeof(int));
                var i = Expression.Parameter(typeof(int));
                var breakLabel = Expression.Label(len.Type);
                var body = Expression.Block(
                    new[] { len, i, },
                    Expression.Loop(
                    Expression.IfThenElse(
                    Expression.LessThan(i, Expression.ArrayLength(values)),
                    Expression.Block(
                    new[] { exp.Parameters[0] },
                    Expression.Assign(exp.Parameters[0], Expression.ArrayIndex(values, Expression.PostIncrementAssign(i))),
                    Expression.IfThen(
                    exp.Body,
                    Expression.Block(Expression.Assign(Expression.ArrayAccess(result, Expression.PostIncrementAssign(len)), exp.Parameters[0])))
                    ),
                    Expression.Break(breakLabel, len)),
                    breakLabel));
                return Expression.Lambda<Func<T[], T[], int>>(body, result, values).Compile();
            }
        }
        public static int Filter2<T>(T[] result, T[] values, Expression<Func<T, bool>> exp) {
            var f = Filter2Cache<T>.Lookup(exp);
            return f(result, values);
        }
        private class Map2Cache<T, T2> {
            private static Dictionary<string, Action<T[], T[]>> Cache = new Dictionary<string, Action<T[], T[]>>();
            public static Action<T[], T[]> Lookup(Expression<Func<T, T2>> exp) {
                string expStr = exp.ToString();
                Action<T[], T[]> f;
                if (Cache.TryGetValue(expStr, out f)) return f;
                f = Build(exp);
                Cache.Add(expStr, f);
                return f;
            }
            private static Action<T[], T[]> Build(Expression<Func<T, T2>> exp) {
                var result = Expression.Parameter(typeof(T[]));
                var values = Expression.Parameter(typeof(T[]));
                var i = Expression.Parameter(typeof(int));
                var breakLabel = Expression.Label();
                var body = Expression.Block(
                    new[] { i, },
                    Expression.Loop(
                    Expression.IfThenElse(
                    Expression.LessThan(i, Expression.ArrayLength(values)),
                    Expression.Block(
                    new[] { exp.Parameters[0] },
                    Expression.Assign(exp.Parameters[0], Expression.ArrayIndex(values, i)),
                    Expression.Assign(Expression.ArrayAccess(result, Expression.PostIncrementAssign(i)), exp.Body)),
                    Expression.Break(breakLabel)),
                    breakLabel));
                return Expression.Lambda<Action<T[], T[]>>(body, result, values).Compile();
            }
        }
        public static void Map2<T, T2>(T[] result, T[] values, Expression<Func<T, T2>> exp) {
            var f = Map2Cache<T, T2>.Lookup(exp);
            f(result, values);
        }
        private class Reduce2Cache<T, T2> {
            private static Dictionary<string, Func<T[], T2, T2>> Cache = new Dictionary<string, Func<T[], T2, T2>>();
            public static Func<T[], T2, T2> Lookup(Expression<Func<T, T2, T2>> exp) {
                string expStr = exp.ToString();
                Func<T[], T2, T2> f;
                if (Cache.TryGetValue(expStr, out f)) return f;
                f = Build(exp);
                Cache.Add(expStr, f);
                return f;
            }
            private static Func<T[], T2, T2> Build(Expression<Func<T, T2, T2>> exp) {
                var values = Expression.Parameter(typeof(T[]));
                var init = Expression.Parameter(typeof(T2));
                var i = Expression.Parameter(typeof(int));
                var breakLabel = Expression.Label(init.Type);
                var body = Expression.Block(
                    new[] { i, },
                    Expression.Loop(
                    Expression.IfThenElse(
                    Expression.LessThan(i, Expression.ArrayLength(values)),
                    Expression.Block(
                    new[] { exp.Parameters[0], exp.Parameters[1] },
                    Expression.Assign(exp.Parameters[0], Expression.ArrayIndex(values, Expression.PostIncrementAssign(i))),
                    Expression.Assign(exp.Parameters[1], init),
                    Expression.Assign(init, exp.Body)),
                    Expression.Break(breakLabel, init)),
                    breakLabel));
                return Expression.Lambda<Func<T[], T2, T2>>(body, values, init).Compile();
            }
        }
        public static T2 Reduce2<T, T2>(T[] values, Expression<Func<T, T2, T2>> exp, T2 init = default(T2)) {
            var f = Reduce2Cache<T, T2>.Lookup(exp);
            return f(values, init);
        }
        public static int FilterInt(int[] result, int[] values) {
            int len = 0;
            foreach (var v in values) {
                if ((v & 1) == 1) result[len++] = v;
            }
            return len;
        }
        public static void MapInt(int[] result, int[] values) {
            var len = 0;
            foreach (var v in values) result[len++] = v + v;
        }
        public static int ReduceInt(int[] values) {
            var init = 0;
            foreach (var v in values) init += v;
            return init;
        }

        public static void Main(string[] args) {
            int[] a = Enumerable.Range(0, 1024 * 32).ToArray();
            int[] b = new int[a.Length];

            Utils.Print("##############");
            Utils.Timeit(10, () => {
                a.Where(v => (v & 1) == 0).ToArray();
            });
            Utils.Timeit(10, () => {
                a.Select(v => v + v).ToArray();
            });
            Utils.Timeit(10, () => {
                a.Aggregate((v, v2) => v + v2);
            });
            Utils.Print("##############");
            Utils.Timeit(10, () => {
                Filter(b, a, v => (v & 1) == 0);
            });
            Utils.Timeit(10, () => {
                Map(b, a, v => v + v);
            });
            Utils.Timeit(10, () => {
                Reduce(a, (v, v2) => v + v2, 0);
            });
            Utils.Print("##############");
            Utils.Timeit(10, () => {
                Filter2(b, a, v => (v & 1) == 0);
            });
            Utils.Timeit(10, () => {
                Map2(b, a, v => v + v);
            });
            Utils.Timeit(10, () => {
                Reduce2(a, (v, v2) => v + v2, 0);
            });
            Utils.Print("##############");
            Utils.Timeit(10, () => {
                FilterInt(b, a);
            });
            Utils.Timeit(10, () => {
                MapInt(b, a);
            });
            Utils.Timeit(10, () => {
                ReduceInt(a);
            });
        }
    }
}
