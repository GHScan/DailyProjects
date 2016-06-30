using System.Diagnostics;
using System.Linq;
using System.Runtime.InteropServices;

namespace System.Collections.Generic
{
    internal sealed class PriorityQueueDebugView<T>
    {
        private readonly PriorityQueue<T> _queue;

        public PriorityQueueDebugView(PriorityQueue<T> queue)
        {
            if (queue == null) throw new ArgumentNullException("queue");

            _queue = queue;
        }

        [DebuggerBrowsable(DebuggerBrowsableState.RootHidden)]
        public T[] Items
        {
            get { return _queue.ToArray(); }
        }
    }

    /// <summary>
    /// Represents a collection of objects that are removed in a sorted order.
    /// </summary>
    /// <typeparam name="T">Specifies the type of elements in the queue.</typeparam>    
    [DebuggerTypeProxy(typeof(PriorityQueueDebugView<>))]
    [DebuggerDisplay("Count = {Count}")]
    [ComVisible(true)]
    public class PriorityQueue<T> : ICollection, IReadOnlyCollection<T>
    {
        private static readonly T[] _emptyArray = new T[1];

        private object _syncRoot;

        private readonly IComparer<T> _comparer;
        private int _version;
        private T[] _array;

        private static void Swap(T[] array, int i, int j)
        {
            var v = array[i];
            array[i] = array[j];
            array[j] = v;
        }

        private static void BuildHeap(T[] array, int first, int last, IComparer<T> comparer)
        {
            if (first >= last) return;

            for (var i = last / 2;
                i >= first;
                --i)
            {
                ShiftDown(array, i, last, comparer);
            }
        }

        private static void ShiftUp(T[] array, int first, int last, IComparer<T> comparer)
        {
            if (first >= last) return;

            for (var parent = last / 2;
                parent >= first
                && comparer.Compare(array[last], array[parent]) < 0;
                last = parent, parent = parent / 2)
            {
                Swap(array, last, parent);
            }
        }

        private static void ShiftDown(T[] array, int first, int last, IComparer<T> comparer)
        {
            if (first >= last) return;

            while (first * 2 <= last)
            {
                var leftChild = first * 2;
                var rightChild = leftChild + 1;

                var minChild =
                    rightChild <= last && comparer.Compare(array[rightChild], array[leftChild]) < 0
                        ? rightChild
                        : leftChild;

                if (comparer.Compare(array[minChild], array[first]) < 0)
                {
                    Swap(array, minChild, first);
                    first = minChild;
                }
                else
                {
                    break;
                }
            }
        }

        private void EnsureCapacity(int newCapacity)
        {
            if (Capacity < newCapacity)
                Capacity = Math.Max(newCapacity, Capacity * 3 / 2);
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="PriorityQueue{T}"/> class 
        /// that uses a default comparer.
        /// </summary>
        public PriorityQueue() :
            this(0)
        {
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="PriorityQueue{T}"/> class 
        /// that has the specified initial capacity.
        /// </summary>
        /// <param name="capacity">The initial number of elements that the <see cref="PriorityQueue{T}"/> can contain.</param>
        /// <exception cref="T:System.ArgumentOutOfRangeException"><paramref name="capacity"/> is less than zero.</exception>
        public PriorityQueue(int capacity) :
            this(capacity, Comparer<T>.Default)
        {
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="PriorityQueue{T}"/> class 
        /// that uses a specified comparer.
        /// </summary>
        /// <param name="comparer">The <see cref="T:System.Collections.Generic.IComparer{T}"/> to use when comparing elements.</param>
        /// <exception cref="T:System.ArgumentNullException"><paramref name="comparer"/> is null.</exception>
        public PriorityQueue(IComparer<T> comparer) :
            this(0, comparer)
        {
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="PriorityQueue{T}"/> class 
        /// that contains elements copied from the specified collection and uses a default comparer.
        /// </summary>
        /// <param name="collection">The collection whose elements are copied to the new <see cref="PriorityQueue{T}"/>.</param>
        /// <exception cref="T:System.ArgumentNullException"><paramref name="collection"/> is null.</exception>
        public PriorityQueue(IEnumerable<T> collection) :
            this(collection, Comparer<T>.Default)
        {
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="PriorityQueue{T}"/> class 
        /// that contains elements copied from the specified collection and uses a specified comparer.
        /// </summary>
        /// <param name="collection">The collection whose elements are copied to the new <see cref="PriorityQueue{T}"/>.</param>
        /// <param name="comparer">The <see cref="T:System.Collections.Generic.IComparer{T}"/> to use when comparing elements.</param>
        /// <exception cref="T:System.ArgumentNullException">
        /// <paramref name="collection"/> is null. -or-
        /// <paramref name="comparer"/> is null.
        /// </exception>
        public PriorityQueue(IEnumerable<T> collection, IComparer<T> comparer)
        {
            if (collection == null) throw new ArgumentNullException("collection");
            if (comparer == null) throw new ArgumentNullException("comparer");

            _comparer = comparer;
            _version = int.MinValue;
            _array = _emptyArray.Concat(collection).ToArray();
            Count = Capacity;

            BuildHeap(_array, 1, Count, _comparer);
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="PriorityQueue{T}"/> class that is empty,
        /// has the specified initial capacity, and uses a specified comparer.
        /// </summary>
        /// <param name="capacity">The initial number of elements that the <see cref="PriorityQueue{T}"/> can contain.</param>
        /// <param name="comparer">The <see cref="T:System.Collections.Generic.IComparer{T}"/> to use when comparing elements.</param>
        /// <exception cref="T:System.ArgumentOutOfRangeException"><paramref name="capacity"/> is less than zero.</exception>
        /// <exception cref="T:System.ArgumentNullException"><paramref name="comparer"/> is null.</exception>
        public PriorityQueue(int capacity, IComparer<T> comparer)
        {
            if (capacity < 0) throw new ArgumentOutOfRangeException("capacity");
            if (comparer == null) throw new ArgumentNullException("comparer");

            _comparer = comparer;
            _version = int.MinValue;
            _array = _emptyArray;
            Count = 0;

            Capacity = capacity;
        }

        /// <summary>
        /// Gets the <see cref="IComparer{T}"/> for the <see cref="PriorityQueue{T}"/>. 
        /// </summary>
        /// <value>
        /// The <see cref="T:System.Collections.Generic.IComparer{T}"/> that is used when
        /// comparing elements in the <see cref="PriorityQueue{T}"/>. 
        /// </value>
        public IComparer<T> Comparer
        {
            get { return _comparer; }
        }

        /// <summary>
        /// Gets the number of elements contained in the <see cref="PriorityQueue{T}"/>.
        /// </summary>
        /// <value>The number of elements contained in the <see cref="PriorityQueue{T}"/>.</value>
        public int Count { get; private set; }

        /// <summary>
        /// Gets or sets the total number of elements the internal data structure can hold without resizing.
        /// </summary>
        /// 
        /// <returns>
        /// The number of elements that the <see cref="PriorityQueue{T}"/> can contain before resizing is required.
        /// </returns>
        /// <exception cref="T:System.ArgumentOutOfRangeException"><see cref="PriorityQueue{T}.Capacity"/> is set to a value that is less than <see cref="PriorityQueue{T}.Count"/>. </exception><exception cref="T:System.OutOfMemoryException">There is not enough memory available on the system.</exception>
        public int Capacity
        {
            get { return _array.Length - 1; }
            set
            {
                if (value < Count) throw new ArgumentOutOfRangeException();
                if (value + 1 == _array.Length) return;

                var array = new T[value + 1];
                Array.Copy(_array, 1, array, 1, Count);
                _array = array;
            }
        }

        /// <summary>
        /// Adds an object to the end of the <see cref="PriorityQueue{T}"/>.
        /// </summary>
        /// <param name="item">
        /// The object to add to the end of the <see cref="PriorityQueue{T}"/>. 
        /// The value can be null for reference types.
        /// </param>
        public void Enqueue(T item)
        {
            EnsureCapacity(Count + 1);

            ++Count;
            _array[Count] = item;
            ShiftUp(_array, 1, Count, _comparer);

            ++_version;
        }

        /// <summary>
        /// Removes and returns the object with the lowest priority in the <see cref="PriorityQueue{T}"/>.
        /// </summary>
        /// <returns>The object with the lowest priority that is removed from the <see cref="PriorityQueue{T}"/>.</returns>
        /// <exception cref="InvalidOperationException">The <see cref="PriorityQueue{T}"/> is empty.</exception>
        public T Dequeue()
        {
            if (Count == 0) throw new InvalidOperationException();

            Swap(_array, 1, Count);
            ShiftDown(_array, 1, Count - 1, _comparer);

            --Count;
            ++_version;

            return _array[Count + 1];
        }

        /// <summary>
        /// Returns the object with the lowest priority in the <see cref="PriorityQueue{T}"/>.
        /// </summary>
        /// <exception cref="InvalidOperationException">The <see cref="PriorityQueue{T}"/> is empty.</exception>
        public T Peek()
        {
            if (Count == 0) throw new InvalidOperationException();

            return _array[1];
        }

        /// <summary>
        /// Removes all elements from the <see cref="PriorityQueue{T}"/>.
        /// </summary>
        public void Clear()
        {
            Array.Clear(_array, 1, Count);
            Count = 0;
            ++_version;
        }

        /// <summary>
        /// Determines whether an element is in the <see cref="PriorityQueue{T}"/>.
        /// </summary>
        /// <param name="item">
        /// The object to add to the end of the <see cref="PriorityQueue{T}"/>. 
        /// The value can be null for reference types.
        /// </param>
        /// <returns>
        /// true if item is found in the <see cref="PriorityQueue{T}"/>;  otherwise, false.
        /// </returns>
        public bool Contains(T item)
        {
            for (var i = 1; i <= Count; ++i)
                if (_comparer.Compare(item, _array[i]) == 0)
                    return true;
            return false;
        }

        /// <summary>
        /// Copies the elements of the <see cref="PriorityQueue{T}"/> to an  <see cref="T:System.Array"/>, 
        /// starting at a particular <see cref="T:System.Array"/> index.
        /// </summary>
        /// <param name="array">
        /// The one-dimensional <see cref="T:System.Array">Array</see> that is the
        /// destination of the elements copied from the <see cref="PriorityQueue{T}"/>. 
        /// The <see cref="T:System.Array">Array</see> must have zero-based indexing.
        /// </param>
        /// <param name="arrayIndex">The zero-based index in <paramref name="array"/> at which copying begins.</param>
        /// <exception cref="T:System.ArgumentNullException"><paramref name="array"/> is null.</exception>
        /// <exception cref="T:System.ArgumentOutOfRangeException">
        /// <paramref name="arrayIndex"/> is less than zero. -or- 
        /// <paramref name="arrayIndex"/> is equal to or greater than the length of the <paramref name="array"/>
        /// </exception>
        /// <exception cref="ArgumentException">
        /// The number of elements in the source <see cref="T:System.Collections.ICollection"/> is
        /// greater than the available space from <paramref name="index"/> to the end of the destination
        /// <paramref name="array"/>.
        /// </exception>
        public void CopyTo(T[] array, int arrayIndex)
        {
            if (array == null) throw new ArgumentNullException("array");
            if (arrayIndex < 0 || arrayIndex >= array.Length) throw new ArgumentOutOfRangeException("arrayIndex");
            if (array.Length - arrayIndex < Count) throw new ArgumentException("array");

            Array.Copy(_array, 1, array, arrayIndex, Count);
        }

        /// <summary>
        /// Copies the elements of the <see cref="T:System.Collections.ICollection"/> to an 
        /// <see cref="T:System.Array"/>, starting at a particular <see cref="T:System.Array"/> index.
        /// </summary>
        /// <param name="array">
        /// The one-dimensional <see cref="T:System.Array">Array</see> that is the
        /// destination of the elements copied from the <see cref="PriorityQueue{T}"/>. 
        /// The <see cref="T:System.Array">Array</see> must have zero-based indexing.
        /// </param>
        /// <param name="index">The zero-based index in <paramref name="array"/> at which copying begins.</param>
        /// <exception cref="T:System.ArgumentNullException"><paramref name="array"/> is null.</exception>
        /// <exception cref="T:System.ArgumentOutOfRangeException"><paramref name="index"/> is less than zero.</exception>
        /// <exception cref="ArgumentException">
        /// <paramref name="array"/> is multidimensional. -or-
        /// <paramref name="array"/> does not have zero-based indexing. -or-
        /// <paramref name="index"/> is equal to or greater than the length of the <paramref name="array"/> -or- 
        /// The number of elements in the source <see cref="T:System.Collections.ICollection"/> is
        /// greater than the available space from <paramref name="index"/> to the end of the destination
        /// <paramref name="array"/>. -or- 
        /// The type of the source <see cref="T:System.Collections.ICollection"/> cannot be cast automatically 
        /// to the type of the destination <paramref name="array"/>.
        /// </exception>
        void ICollection.CopyTo(Array array, int index)
        {
            if (array == null) throw new ArgumentNullException("array");
            if (index < 0 || index >= array.Length) throw new ArgumentOutOfRangeException("arrayIndex");
            if (array.Length - index < Count) throw new ArgumentException("array");

            Array.Copy(_array, 1, array, index, Count);
        }

        /// <summary>
        /// Copies the elements stored in the <see cref="PriorityQueue{T}"/> to a new array.
        /// </summary>
        /// <returns>
        /// A new array containing a snapshot of elements copied from the <see cref="PriorityQueue{T}"/>.
        /// </returns>
        public T[] ToArray()
        {
            var array = new T[Count];
            CopyTo(array, 0);
            return array;
        }

        /// <summary>
        /// Returns an enumerator that iterates through the <see cref="PriorityQueue{T}"/>
        /// </summary>
        /// <returns>An enumerator for the contents of the <see cref="PriorityQueue{T}"/>.</returns>
        public Enumerator GetEnumerator()
        {
            return new Enumerator(this);
        }

        /// <summary>
        /// Returns an enumerator that iterates through the <see cref="PriorityQueue{T}"/>
        /// </summary>
        /// <returns>An enumerator for the contents of the <see cref="PriorityQueue{T}"/>.</returns>
        IEnumerator<T> IEnumerable<T>.GetEnumerator()
        {
            return GetEnumerator();
        }

        /// <summary>
        /// Returns an enumerator that iterates through the <see cref="PriorityQueue{T}"/>.
        /// </summary>
        /// <returns>An <see cref="T:System.Collections.IEnumerator"/> that can be used to iterate through the collection.</returns>
        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }

        /// <summary>
        /// Sets the capacity to the actual number of elements in the <see cref="PriorityQueue{T}"/>, 
        /// if that number is less than than a threshold value.
        /// </summary>
        public void TrimExcess()
        {
            Capacity = Count;
        }

        /// <summary>
        /// Gets a value that indicates whether access to the <see cref="ICollection"/> is 
        /// synchronized with the SyncRoot.
        /// </summary>
        /// <value>true if access to the <see cref="T:System.Collections.ICollection"/> is synchronized
        /// with the SyncRoot; otherwise, false. For <see cref="PriorityQueue{T}"/>, this property always
        /// returns false.</value>
        bool ICollection.IsSynchronized
        {
            get { return false; }
        }

        /// <summary>
        /// Gets an object that can be used to synchronize access to the 
        /// <see cref="T:System.Collections.ICollection"/>.
        /// </summary>
        /// <value>
        /// An object that can be used to synchronize access to the 
        /// <see cref="T:System.Collections.ICollection"/>.
        /// </value>
        object ICollection.SyncRoot
        {
            get
            {
                if (_syncRoot == null)
                {
                    Threading.Interlocked.CompareExchange(ref _syncRoot, new object(), null);
                }
                return _syncRoot;
            }
        }

        public struct Enumerator : IEnumerator<T>
        {
            private readonly PriorityQueue<T> _queue;
            private readonly int _version;
            private int _index;

            internal Enumerator(PriorityQueue<T> queue)
            {
                if (queue == null) throw new ArgumentException("queue");

                _queue = queue;
                _version = queue._version;
                _index = 0;
            }

            public T Current
            {
                get
                {
                    if (_index < 1 || _index > _queue.Count) throw new InvalidOperationException();

                    return _queue._array[_index];
                }
            }

            object IEnumerator.Current
            {
                get { return Current; }
            }

            public bool MoveNext()
            {
                if (_queue._version != _version) throw new InvalidOperationException();
                if (_index >= _queue.Count) return false;

                ++_index;
                return _index <= _queue.Count;
            }

            public void Reset()
            {
                _index = 0;
            }

            public void Dispose()
            {
            }
        }
    }
}