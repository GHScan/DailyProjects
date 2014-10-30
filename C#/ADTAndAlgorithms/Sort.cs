using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;

namespace CSharp13
{
    public class Program
    {
        public static IEnumerable<T> QSort<T>(IEnumerable<T> a) where T : IComparable<T>
        {
            return !a.Any() ? a : QSort(a.Where(i => i.CompareTo(a.First()) < 0)).Concat(a.Where(i => i.CompareTo(a.First()) == 0)).Concat(QSort(a.Where(i => i.CompareTo(a.First()) > 0)));
        }
        public static Func<T, T> Y<T>(Func<Func<T, T>, Func<T, T>> f)
        {
            return a => f(Y(f))(a);
        }

        public static void TestSortFuncs(Func<IEnumerable<int>, IEnumerable<int>>[] sortFuncs)
        {
            var datas = new int[][]{ 
                new int[0],
                new []{3, 2},
                new []{3, 1, 2},
                new []{3, 1, 2, 4, },
                new []{5, 3, 1, 2, 4, },
            };
            foreach (var f in sortFuncs)
            {
                foreach (var data in datas)
                {
                    Debug.Assert(f(data).SequenceEqual(data.OrderBy(i=>i)));
                }
            }
        }

        public static void Main(string[] args)
        {
            TestSortFuncs(new Func<IEnumerable<int>, IEnumerable<int>>[]
                {
                    // Traditional quick sort
                    QSort,
                    // Y combinator
                    Y<IEnumerable<int>>(f => (a =>
                    {
                        return !a.Any() ? a : f(a.Where(i => i < a.First())).Concat(a.Where(i => i == a.First())).Concat(f(a.Where(i => i > a.First())));
                    })),
                });
        }
    }
}
