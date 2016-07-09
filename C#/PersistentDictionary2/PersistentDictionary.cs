using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Runtime.Serialization;
using System.Text;

namespace CSharp2013
{
    [Serializable]
    public sealed class WriteAheadLogCorruptionException : Exception
    {
        public WriteAheadLogCorruptionException(string message)
            : base(message)
        {
        }

        public WriteAheadLogCorruptionException(string message, Exception innerException)
            : base(message, innerException)
        {
        }

        public WriteAheadLogCorruptionException(SerializationInfo info, StreamingContext context)
            : base(info, context)
        {
        }
    }

    public sealed class PersistentDictionary : IDictionary<string, byte[]>, IDisposable
    {
        public string SnapshotPath { get; private set; }
        private readonly Dictionary<string, byte[]> mDictionary;
        private WriteAheadLogWriter mWriteAheadLogWriter;

        public PersistentDictionary(string snapshotPath)
        {
            SnapshotPath = snapshotPath;

            if (File.Exists(TempSnapshotPath) && !File.Exists(SnapshotPath))
                File.Move(TempSnapshotPath, SnapshotPath);

            mDictionary = LoadSnapshot(SnapshotPath);
            mWriteAheadLogWriter = new WriteAheadLogWriter(SnapshotPath);

            if (mWriteAheadLogWriter.Length > 0)
            {
                CheckPoint();
            }
        }

        private enum WriteAheadLogType
        {
            Add,
            Update,
            Remove
        }

        private static Dictionary<string, byte[]> LoadSnapshot(string snapshotPath)
        {
            var dict = new Dictionary<string, byte[]>();

            if (!File.Exists(snapshotPath))
                return dict;

            var reader = new WriteAheadLogReader(snapshotPath);
            foreach (var tuple in reader)
            {
                var key = Encoding.UTF8.GetString(tuple.Item2);
                var value = tuple.Item3;

                switch ((WriteAheadLogType)tuple.Item1)
                {
                    case WriteAheadLogType.Add:
                        dict.Add(key, value);
                        break;
                    case WriteAheadLogType.Update:
                        dict[key] = value;
                        break;
                    case WriteAheadLogType.Remove:
                        dict.Remove(key);
                        break;
                    default:
                        throw new NotSupportedException();
                }
            }

            return dict;
        }

        private string TempSnapshotPath
        {
            get { return SnapshotPath + "_temp"; }
        }

        public void CheckPoint()
        {
            if (File.Exists(TempSnapshotPath))
                File.Delete(TempSnapshotPath);

            CreateSnapshot(TempSnapshotPath);


            mWriteAheadLogWriter.Dispose();

            File.Delete(SnapshotPath);
            File.Move(TempSnapshotPath, SnapshotPath);

            mWriteAheadLogWriter = new WriteAheadLogWriter(SnapshotPath);
        }

        public void CreateSnapshot(string snapshotPath)
        {
            if (File.Exists(snapshotPath))
                throw new ArgumentException(string.Format("Snapshot file already exists: {0}", snapshotPath));

            using (var writer = new WriteAheadLogWriter(snapshotPath))
            {
                foreach (var kv in mDictionary)
                {
                    writer.Append((byte)WriteAheadLogType.Add, Encoding.UTF8.GetBytes(kv.Key), kv.Value);
                }
            }
        }

        public void Add(string key, byte[] value)
        {
            if (mDictionary.ContainsKey(key))
                throw new ArgumentException(string.Format("Duplicate key: {0}", key));

            mWriteAheadLogWriter.Append((byte)WriteAheadLogType.Add, Encoding.UTF8.GetBytes(key), value);
            mDictionary.Add(key, value);
        }

        public bool Remove(string key)
        {
            if (!mDictionary.ContainsKey(key))
                return false;

            mWriteAheadLogWriter.Append((byte)WriteAheadLogType.Remove, Encoding.UTF8.GetBytes(key), new byte[0]);
            return mDictionary.Remove(key);
        }

        public byte[] this[string key]
        {
            get { return mDictionary[key]; }
            set
            {
                mWriteAheadLogWriter.Append((byte)WriteAheadLogType.Update, Encoding.UTF8.GetBytes(key), value);
                mDictionary[key] = value;
            }
        }

        public void Dispose()
        {
            if (mWriteAheadLogWriter != null)
            {
                mWriteAheadLogWriter.Dispose();
                mWriteAheadLogWriter = null;
            }
        }

        public int Count
        {
            get { return mDictionary.Count; }
        }

        public bool IsReadOnly
        {
            get { return false; }
        }

        public bool ContainsKey(string key)
        {
            return mDictionary.ContainsKey(key);
        }

        public bool TryGetValue(string key, out byte[] value)
        {
            return mDictionary.TryGetValue(key, out value);
        }

        public ICollection<string> Keys
        {
            get { return mDictionary.Keys; }
        }

        public ICollection<byte[]> Values
        {
            get { return mDictionary.Values; }
        }

        public IEnumerator<KeyValuePair<string, byte[]>> GetEnumerator()
        {
            return mDictionary.GetEnumerator();
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }

        void ICollection<KeyValuePair<string, byte[]>>.Clear()
        {
            throw new NotSupportedException();
        }

        void ICollection<KeyValuePair<string, byte[]>>.Add(KeyValuePair<string, byte[]> item)
        {
            Add(item.Key, item.Value);
        }

        bool ICollection<KeyValuePair<string, byte[]>>.Contains(KeyValuePair<string, byte[]> item)
        {
            return ((ICollection<KeyValuePair<string, byte[]>>)mDictionary).Contains(item);
        }

        void ICollection<KeyValuePair<string, byte[]>>.CopyTo(KeyValuePair<string, byte[]>[] array, int arrayIndex)
        {
            ((ICollection<KeyValuePair<string, byte[]>>)mDictionary).CopyTo(array, arrayIndex);
        }

        bool ICollection<KeyValuePair<string, byte[]>>.Remove(KeyValuePair<string, byte[]> item)
        {
            byte[] value;
            if (!mDictionary.TryGetValue(item.Key, out value) || value != item.Value)
                return false;

            return Remove(item.Key);
        }

        internal static class Checksum
        {
            public static ulong Calculate(byte logType, byte[] key, byte[] value)
            {
                return Combine(Combine(logType, key), value);
            }

            private static ulong Combine(ulong checksum, byte[] array)
            {
                unsafe
                {
                    fixed (byte* ptr = array)
                    {
                        var curr = (ulong*)ptr;
                        var end = curr + array.Length / sizeof(ulong);
                        while (curr != end)
                            checksum = checksum * 239 + *curr++;

                        var rest = array.Length % sizeof(ulong);
                        if (rest > 0)
                        {
                            ulong v = 0;
                            var curr2 = (byte*)curr;
                            while (rest-- > 0)
                                v = v * 256 + curr2[rest];
                            checksum = checksum * 239 + v;
                        }

                        return checksum;
                    }
                }
            }
        }

        internal sealed class WriteAheadLogWriter : IDisposable
        {
            public string FilePath { get; private set; }
            public long Length { get; private set; }
            private BinaryWriter mBinaryWriter;

            public WriteAheadLogWriter(string filePath)
            {
                FilePath = filePath;

                mBinaryWriter =
                    new BinaryWriter(new FileStream(filePath, FileMode.OpenOrCreate, FileAccess.Write, FileShare.Read));
                mBinaryWriter.BaseStream.Seek(0, SeekOrigin.End);

                Length = mBinaryWriter.BaseStream.Length;
            }

            public void Append(byte logType, byte[] key, byte[] value)
            {
                Debug.Assert(mBinaryWriter.BaseStream.Length == Length);

                try
                {
                    mBinaryWriter.Write(Checksum.Calculate(logType, key, value));
                    mBinaryWriter.Write(logType);
                    mBinaryWriter.Write(key.Length);
                    mBinaryWriter.Write(key);
                    mBinaryWriter.Write(value.Length);
                    mBinaryWriter.Write(value);
                    mBinaryWriter.Flush();

                    Length += sizeof(ulong) + 1 + sizeof(int) + key.Length + sizeof(int) + value.Length;
                }
                catch
                {
                    try
                    {
                        mBinaryWriter.Flush();
                        mBinaryWriter.BaseStream.SetLength(Length);
                    }
                    catch (Exception e)
                    {
                        throw new WriteAheadLogCorruptionException(
                            string.Format("Found corrupted write ahead log [{0}], offset={1}", FilePath, Length), e);
                    }

                    throw;
                }
            }

            public void Dispose()
            {
                if (mBinaryWriter != null)
                {
                    mBinaryWriter.Dispose();
                    mBinaryWriter = null;
                }
            }
        }

        internal sealed class WriteAheadLogReader : IEnumerable<Tuple<byte, byte[], byte[]>>
        {
            public string FilePath { get; private set; }

            public WriteAheadLogReader(string filePath)
            {
                FilePath = filePath;
            }

            public Enumerator GetEnumerator()
            {
                return new Enumerator(FilePath);
            }

            IEnumerator<Tuple<byte, byte[], byte[]>> IEnumerable<Tuple<byte, byte[], byte[]>>.GetEnumerator()
            {
                return GetEnumerator();
            }

            IEnumerator IEnumerable.GetEnumerator()
            {
                return GetEnumerator();
            }

            public struct Enumerator : IEnumerator<Tuple<byte, byte[], byte[]>>
            {
                private readonly string mFilePath;
                private BinaryReader mBinaryReader;
                private Tuple<byte, byte[], byte[]> mCurrent;

                public Enumerator(string filePath)
                {
                    mFilePath = filePath;
                    mBinaryReader = new BinaryReader(File.OpenRead(filePath));
                    mCurrent = null;
                }

                public void Dispose()
                {
                    if (mBinaryReader != null)
                    {
                        mBinaryReader.Dispose();
                        mBinaryReader = null;
                    }
                }

                public bool MoveNext()
                {
                    if (mBinaryReader.BaseStream.Position == mBinaryReader.BaseStream.Length)
                        return false;

                    try
                    {
                        var position = mBinaryReader.BaseStream.Position;
                        var checksum = mBinaryReader.ReadUInt64();
                        var logType = mBinaryReader.ReadByte();
                        var keyLength = mBinaryReader.ReadInt32();
                        var key = mBinaryReader.ReadBytes(keyLength);
                        if (key.Length < keyLength) throw new EndOfStreamException();
                        var valueLength = mBinaryReader.ReadInt32();
                        var value = mBinaryReader.ReadBytes(valueLength);
                        if (value.Length < valueLength) throw new EndOfStreamException();
                        if (checksum != Checksum.Calculate(logType, key, value))
                        {
                            throw new WriteAheadLogCorruptionException(
                                string.Format("Found corrupted write ahead log [{0}], offset={1}", mFilePath, position));
                        }

                        mCurrent = Tuple.Create(logType, key, value);

                        return true;
                    }
                    catch (EndOfStreamException)
                    {
                        mBinaryReader.BaseStream.Position = mBinaryReader.BaseStream.Length;
                        return false;
                    }
                }

                public void Reset()
                {
                    throw new NotSupportedException();
                }

                public Tuple<byte, byte[], byte[]> Current
                {
                    get { return mCurrent; }
                }

                object IEnumerator.Current
                {
                    get { return Current; }
                }
            }
        }
    }
}