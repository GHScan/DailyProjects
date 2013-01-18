using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using System.IO;
using System.Collections;

namespace CSharp08
{
    interface IBinaryReader
    {
        void read(ArraySegment<byte> buf, out int readeBytes);
        void read(out int i, out int readedBytes);
    }

    interface IBinaryWriter
    {
        void write(ArraySegment<byte> buf, out int writedBytes);
        void write(int i, out int writedBytes);
    }

    class FileBinaryReader: 
        IBinaryReader
    {
        public FileBinaryReader(string path)
        {
            m_reader = new BinaryReader(new FileStream(path, FileMode.Open));
        }

        public void read(ArraySegment<byte> buf, out int readeBytes)
        {
            readeBytes = m_reader.Read(buf.Array, buf.Offset, buf.Count);
        }

        public void read(out int i, out int readedBytes)
        {
            if (m_reader.BaseStream.Position == m_reader.BaseStream.Length)
            {
                i = 0; readedBytes = 0; return;
            }

            i = m_reader.ReadInt32();
            readedBytes = 4;
        }

        private BinaryReader m_reader;
    }

    class FileBinaryWriter:
        IBinaryWriter
    {
        public FileBinaryWriter(string path)
        {
            m_writer = new BinaryWriter(new FileStream(path, FileMode.Create));
        }

        public void write(ArraySegment<byte> buf, out int writedBytes)
        {
            m_writer.Write(buf.Array, buf.Offset, buf.Count);
            writedBytes = buf.Count;
        }

        public void write(int i, out int writedBytes)
        {
            m_writer.Write(i);
            writedBytes = 4;
        }

        private BinaryWriter m_writer;
    }

    class MemoryBinaryReader:
        IBinaryReader
    {
        public MemoryBinaryReader(IEnumerable<byte> src)
        {
            m_src = src;
        }

        public void read(ArraySegment<byte> buf, out int readeBytes)
        {
            byte[] a = m_src.ToArray();
            readeBytes = Math.Min(buf.Count, a.Length);
            a.CopyTo(buf.Array, readeBytes);
        }

        public void read(out int i, out int readedBytes)
        {
            i = 0;
            int n = 0;
            foreach (var _i in m_src)
            {
                i |= ((int)_i << (n++ * 8));
                if (n == 4) break;
            }
            readedBytes = 4;
        }

        private IEnumerable<byte> m_src;
    }

    class MemroyBinaryWriter:
        IBinaryWriter
    {
        public MemroyBinaryWriter(List<byte> dest)
        {
            m_dest = dest;
        }

        public void write(ArraySegment<byte> buf, out int writedBytes)
        {
            byte[] a = new byte[buf.Count];
            Array.Copy(buf.Array, buf.Offset, a, 0, buf.Count);
            m_dest.AddRange(a);
            writedBytes = a.Length;
        }

        public void write(int i, out int writedBytes)
        {
            for (int _i = 0; _i < 4; ++_i)
            {
                checked 
                {
                    m_dest.Add((byte)((i >> (_i * 8)) & 0xff));
                }
            }
            writedBytes = 4;
        }

        private List<byte> m_dest;
    }

    class lzwEncoding
    {
        public static int MaxDictionaryBytes = 1 << 22;

        class ArraySegmentEqualComparer: IEqualityComparer<ArraySegment<byte>>
        {
            public bool Equals(ArraySegment<byte> x, ArraySegment<byte> y)
            {
                if (x.Count == y.Count)
                {
                    for (int i = 0; i < x.Count; ++i)
                    {
                        if (x.Array[x.Offset + i] != y.Array[y.Offset + i]) return false;
                    }
                    return true;
                }
                return false;
            }

            public int GetHashCode(ArraySegment<byte> obj)
            {
                unchecked
                {
                    int seed = 0;
                    for (int i = 0; i < obj.Count; ++i)
                    {
                        seed ^= obj.Array[obj.Offset + i].GetHashCode() + (int)0x9e3779b9 + (seed << 6) + (seed >> 2);
                    }
                    return seed;
                }
            }
        }

        private static ArraySegment<byte> lzwPack(List<int> src, byte[] dest)
        {
            int max = src.Max();
            int bitCnt = (int)Math.Ceiling(Math.Log(max, 2));

            int destOffset = 0;
            BitArray a = new BitArray(src.Count * bitCnt);
            for (int i = 0; i < src.Count; ++i)
            {
                for (int j = 0; j < bitCnt; ++j) a[i * bitCnt + j] = ((src[i] >> j) & 0x1) == 1;
            }

            dest[destOffset++] = (byte)bitCnt;
            a.CopyTo(dest, destOffset);

            return new ArraySegment<byte>(dest, 0, destOffset + (a.Length + 7) / 8);
        }

        private static void lzwUnpack(ArraySegment<byte> src, List<int> dest)
        {
            byte[] srcCopy = new byte[src.Count];
            Array.Copy(src.Array, src.Offset, srcCopy, 0, src.Count);

            BitArray a = new BitArray(srcCopy);

            byte bitCnt = srcCopy[0];

            int count = (a.Length - 8) / bitCnt;
            int i = 8;
            while (count-- > 0)
            {
                int val = 0;
                for (int j = 0; j < bitCnt; ++j) val |= (a[i + j] ? 1 : 0) << j;
                i += bitCnt;
                dest.Add(val);
            }
        }

        private static ArraySegment<byte> lzwEncodeImpl(ArraySegment<byte> src, byte[] dest)
        {
            List<int> tmpDest = new List<int>(1 << 20);

            Dictionary<ArraySegment<byte>, int> dic = new Dictionary<ArraySegment<byte>, int>(1 << 20, new ArraySegmentEqualComparer());

            byte[] baseArray = new byte[256];
            for (int i = 0; i < baseArray.Length; ++i)
            checked
            {
                baseArray[i] = (byte)i;
                dic[new ArraySegment<byte>(baseArray, i, 1)] = (byte)i;
            }

            int prefixOff = src.Offset, prefixLen = 1;
            while (prefixOff + prefixLen < src.Offset + src.Count)
            {
                if (dic.ContainsKey(new ArraySegment<byte>(src.Array, prefixOff, prefixLen + 1)))
                {
                    ++prefixLen;
                }
                else
                {
                    tmpDest.Add(dic[new ArraySegment<byte>(src.Array, prefixOff, prefixLen)]);

                    dic.Add(new ArraySegment<byte>(src.Array, prefixOff, prefixLen + 1), dic.Count);
                    prefixOff = prefixOff + prefixLen;
                    prefixLen = 1;
                }
            }

            tmpDest.Add(dic[new ArraySegment<byte>(src.Array, prefixOff, prefixLen)]);

            return lzwPack(tmpDest, dest);
        }

        private static ArraySegment<byte> lzwDecodeImpl(ArraySegment<byte> src, byte[] dest)
        {
            int destOffset = 0;

            List<ArraySegment<byte>> dic = new List<ArraySegment<byte>>();

            byte[] baseArray = new byte[256];
            for (int i = 0; i < baseArray.Length; ++i)
            checked
            {
                baseArray[i] = (byte)i;
                dic.Add(new ArraySegment<byte>(baseArray, i, 1));
            }

            List<int> tempSrc = new List<int>(1 << 20);
            lzwUnpack(src, tempSrc);

            for (int i = 0; i < tempSrc.Count - 1; ++i)
            {
                int t = tempSrc[i], n = tempSrc[i + 1];

                if (n == dic.Count)
                {
                    n = t;
                }

                Array.Copy(dic[t].Array, dic[t].Offset, dest, destOffset, dic[t].Count);
                dest[destOffset + dic[t].Count] = dic[n].Array[dic[n].Offset];

                dic.Add(new ArraySegment<byte>(dest, destOffset, dic[t].Count + 1));

                destOffset += dic[t].Count;
            }

            ArraySegment<byte> last = dic[tempSrc[tempSrc.Count - 1]];
            Array.Copy(last.Array, last.Offset, dest, destOffset, last.Count);
            destOffset += last.Count;

            return new ArraySegment<byte>(dest, 0, destOffset);
        }

        public static bool lzwEncode(IBinaryReader src, IBinaryWriter dest)
        {
            byte[] srcBuf = new byte[MaxDictionaryBytes];
            byte[] destBuf = new byte[MaxDictionaryBytes];
            int readedBytes;

            for (; ; )
            {
                src.read(new ArraySegment<byte>(srcBuf), out readedBytes);
                if (readedBytes == 0) break;

                ArraySegment<byte> ret = lzwEncodeImpl(new ArraySegment<byte>(srcBuf, 0, readedBytes), destBuf);

                int writedBytes;
                dest.write(ret.Count, out writedBytes);
                if (writedBytes != 4) return false;
                dest.write(ret, out writedBytes);
                if (writedBytes != ret.Count) return false;
            }
            return true;
        }

        public static bool lzwDecode(IBinaryReader src, IBinaryWriter dest)
        {
            byte[] srcBuf = new byte[MaxDictionaryBytes];
            byte[] destBuf = new byte[MaxDictionaryBytes];

            for (; ; )
            {
                int chunkBytes = 0;
                int readedBytes = 0;
                src.read(out chunkBytes, out readedBytes);
                if (readedBytes == 0) break;
                if (readedBytes < 4) return false;

                src.read(new ArraySegment<byte>(srcBuf, 0, chunkBytes), out readedBytes);
                if (chunkBytes != readedBytes) return false;

                ArraySegment<byte> r = lzwDecodeImpl(new ArraySegment<byte>(srcBuf, 0, chunkBytes), destBuf);

                int writedBytes = 0;
                dest.write(r, out writedBytes);
                if (writedBytes != r.Count) return false;
            }

            return true;
        }
    }

    partial class Program
    {
        static void Main(string[] args)
        {
            if (args.Length < 1) 
            {
                print("没有输入文件");
                return;
            }

            if (args.Length > 1)
            {
                lzwEncoding.MaxDictionaryBytes = int.Parse(args[1]);
            }

            string path = args[0];
            int dotPos = path.LastIndexOf('.');
            string posfix = path.Substring(dotPos);
            try
            {
                if (posfix == ".lz77")
                {
                    // 解压
                    timer(delegate()
                    {
                        lzwEncoding.lzwDecode(
                        new FileBinaryReader(path),
                        new FileBinaryWriter(path.Substring(0, dotPos) + ".lz77rst"));
                    });
                }
                else
                { 
                    // 压缩
                    timer(delegate()
                    {
                        lzwEncoding.lzwEncode(
                        new FileBinaryReader(path),
                        new FileBinaryWriter(path.Substring(0, dotPos) + ".lz77"));
                    });
                }
            }
            catch (Exception e)
            {
                print(e);
            }

            Console.ReadLine();
        }
    }
}