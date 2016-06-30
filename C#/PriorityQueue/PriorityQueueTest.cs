
using System;
using System.Collections.Generic;
using NUnit.Framework;

namespace CSharp2013
{
    [TestFixture]
    public sealed class PriorityQueueTest
    {
        [Test]
        public void TestBasicFunctionality()
        {
            var q = new PriorityQueue<int>();
            Assert.AreEqual(0, q.Count);
            Assert.AreEqual(0, q.Capacity);
            Assert.Throws<InvalidOperationException>(() => q.Peek());
            Assert.Throws<InvalidOperationException>(() => q.Dequeue());

            q.Enqueue(5);
            q.Enqueue(2);
            q.Enqueue(4);

            Assert.AreEqual(3, q.Count);
            Assert.IsTrue(q.Capacity >= 3);

            Assert.IsTrue(q.Contains(2));
            Assert.IsFalse(q.Contains(3));

            Assert.AreEqual(3, q.ToArray().Length);
            CollectionAssert.AreEqual(q.ToArray(), q);

            Assert.AreEqual(2, q.Peek());

            Assert.AreEqual(2, q.Dequeue());
            Assert.AreEqual(4, q.Dequeue());
            Assert.AreEqual(5, q.Dequeue());
        }

        [Test]
        public void TestConstructWithCapacity()
        {
            var q = new PriorityQueue<int>(10);
            Assert.AreEqual(0, q.Count);
            Assert.AreEqual(10, q.Capacity);

            q.Enqueue(5);
            q.Enqueue(4);

            q.TrimExcess();

            Assert.AreEqual(2, q.Count);
            Assert.AreEqual(2, q.Capacity);
        }

        [Test]
        public void TestConstructWithComparer()
        {
            var q = new PriorityQueue<int>(Comparer<int>.Create((a, b) => -a.CompareTo(b)));

            q.Enqueue(5);
            q.Enqueue(2);
            q.Enqueue(4);

            Assert.AreEqual(5, q.Dequeue());
            Assert.AreEqual(4, q.Dequeue());
            Assert.AreEqual(2, q.Dequeue());
        }

        [Test]
        public void TestEnumerator()
        {
            var q = new PriorityQueue<int>();
            var e = q.GetEnumerator();

            Assert.Throws<InvalidOperationException>(() =>
            {
                var v = e.Current;
            });

            Assert.IsFalse(e.MoveNext());

            Assert.Throws<InvalidOperationException>(() =>
            {
                var v = e.Current;
            });

            Assert.IsFalse(e.MoveNext());
        }

        [Test]
        public void TestEnumeratorWhileModify()
        {
            var q = new PriorityQueue<int>();
            q.Enqueue(3);

            var e = q.GetEnumerator();
            Assert.IsTrue(e.MoveNext());

            q.Dequeue();

            Assert.Throws<InvalidOperationException>(() => e.MoveNext());
        }

        [Test]
        public void TestWithRandomData()
        {
            var random = new Random();
            for (var iteration = 0; iteration < 100; ++iteration)
            {
                var cmp = Comparer<int>.Create((a,b)=>-a.CompareTo(b));
                var q = new PriorityQueue<int>(cmp);
                var sl = new SortedList<int, int>(cmp);

                var len = random.Next(30);

                for (var i = 0; i < len; ++i)
                {
                    if (random.Next(3) == 0)
                    {
                        var v = random.Next(100);
                        if (!sl.ContainsKey(v))
                        {
                            q.Enqueue(v);
                            sl.Add(v, v);
                        }
                    }
                    else
                    {
                        if (sl.Count > 0)
                        {
                            q.Dequeue();
                            sl.RemoveAt(0);
                        }
                    }
                }

                for (var i = 0; i < sl.Count; ++i)
                {
                    Assert.AreEqual(sl.Keys[i], q.Dequeue());
                }
            }
        }
    }
}
