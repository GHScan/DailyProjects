using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using System.IO;
using System.Threading;

namespace LANFileTransfer
{
    enum FileTransState
    {
        Continue,
        Abort,
    }

    class FileTransSender : IDisposable
    {
        public FileTransSender(string localPath, Stream dest)
        {
            m_destReader = new BinaryReader(dest, Encoding.Default);
            m_destWriter = new BinaryWriter(dest, Encoding.Default);
            m_workStart = false;
            m_transState = FileTransState.Continue;

            if (Directory.Exists(localPath))
            {
                string[] files = Directory.GetFiles(localPath, "*", SearchOption.AllDirectories);
                if (files == null || files.Length == 0) throw new IOException("文件夹为空！");
                m_untransedFiles.InsertRange(0, files);

                m_localDir = localPath;
            }
            else
            {
                if (!File.Exists(localPath)) throw new IOException("文件不存在！");

                m_untransedFiles.Insert(0, localPath);

                m_localDir = Path.GetDirectoryName(localPath);
            }

            if (!m_localDir.EndsWith(Path.DirectorySeparatorChar.ToString())) m_localDir += Path.DirectorySeparatorChar;

            // 通知有多少个文件
            Program.Assert(FileCount > 0);
            m_destWriter.Write(FileCount);
            m_destWriter.Flush();
        }

        public int FileCount { get { return m_transedFiles.Count + m_untransedFiles.Count;  } }
        public int TransedFileCount { get { return m_transedFiles.Count;  } }
        public int TransedBytes { get { return m_transedBytes;  } }

        public event Action<string> TransCompleted;
        public event Action<string> FileTransing;
        public event Action Aborted;

        public void Abort()
        {
            m_transState = FileTransState.Abort;
        }

        public void Work()
        {
            if (m_workStart) return;
            m_workStart = true;

            ThreadPool.QueueUserWorkItem(this.WorkImpl_Async);
        }

        public void Dispose()
        {
            Dispose(true);
            GC.SuppressFinalize(this);
        }

        ~FileTransSender()
        {
            Dispose(false);
        }

        private void Dispose(bool dispose)
        {
            m_destWriter.Close(); m_destWriter = null;
            m_destReader.Close(); m_destReader = null;
        }

        private void WorkImpl_Async(object state)
        {
            try
            {
                while (m_untransedFiles.Count > 0)
                {
                    // 测试是否应该结束发送文件
                    {
                        m_destWriter.Write((int)m_transState);
                        m_destWriter.Flush();
                        int readInt = m_destReader.ReadInt32();
                        if (m_transState == FileTransState.Continue)
                        {
                            m_transState = (FileTransState)readInt;
                        }
                        if (m_transState == FileTransState.Abort)
                        {
                            if (Aborted != null) Aborted();
                            break;
                        }
                    }

                    string file = m_untransedFiles[m_untransedFiles.Count - 1];
                    m_untransedFiles.RemoveAt(m_untransedFiles.Count - 1);
                    if (m_transedFiles.Count > 0) m_transedFiles.Insert(m_transedFiles.Count - 1, file);
                    else m_transedFiles.Insert(0, file);

                    string relatFileName = file.Substring(m_localDir.Length);
                    int fileSize = 0;
                    BinaryReader reader = null;

                    try
                    {
                        fileSize = (int)new FileInfo(file).Length;
                        if (fileSize > 0)
                        reader = new BinaryReader(new BufferedStream(File.Open(file, FileMode.Open, FileAccess.Read), 1 << 22));
                    }
                    catch {  }
                    if (reader == null) fileSize = 0;

                    // 写入文件大小、文件名长度、文件名
                    m_destWriter.Write(fileSize);
                    m_destWriter.Write(relatFileName);

                    if (FileTransing != null) FileTransing(relatFileName);

                    if (fileSize > 0)
                    {
                        Program.Assert(reader != null);
                        byte[] buf = new byte[1024];
                        for (; ; )
                        {
                            int readedBytes = reader.Read(buf, 0, buf.Length);
                            if (readedBytes == 0) break;
                            m_destWriter.Write(buf, 0, readedBytes);

                            m_transedBytes += readedBytes;
                        }
                    }
                    m_destWriter.Flush();

                    if (reader != null) reader.Close();
                }
            }
            catch(Exception e)
            {
                if (TransCompleted != null) TransCompleted("未知的错误 : " + e);
                return;   
            }

            if (TransCompleted != null) TransCompleted(null);
        }


        private string m_localDir;
        private bool m_workStart;
        private BinaryReader m_destReader;
        private BinaryWriter m_destWriter;
        private List<string> m_transedFiles = new List<string>();
        private List<string> m_untransedFiles = new List<string>();
        private int m_transedBytes;
        FileTransState m_transState;
    }

    class FileTransReceiver : IDisposable
    {
        public FileTransReceiver(string localPath, Stream src)
        {
            m_srcReader = new BinaryReader(src, Encoding.Default);
            m_srcWriter = new BinaryWriter(src, Encoding.Default);
            m_workStart = false;
            m_transState = FileTransState.Continue;

            if (Directory.Exists(localPath))
            {
                m_localDir = localPath;
            }
            else
            {
                m_localDir = Path.GetDirectoryName(localPath);
            }

            if (!m_localDir.EndsWith(Path.DirectorySeparatorChar.ToString())) m_localDir += Path.DirectorySeparatorChar;

            m_transedFileCount = 0;
            m_fileCount = m_srcReader.ReadInt32();
        }

        public int FileCount { get { return m_fileCount;  } }
        public int TransedFileCount { get { return m_transedFileCount;  } }
        public int TransedBytes { get { return m_transedBytes;  } }

        public event Action<string> TransCompleted;
        public event Action<string> FileTransing;
        public event Action Aborted;

        public void Abort()
        {
            m_transState = FileTransState.Abort;
        }

        public void Work()
        {
            if (m_workStart) return;
            m_workStart = true;

            ThreadPool.QueueUserWorkItem(this.WorkImpl_Async);
        }

        public void Dispose()
        {
            Dispose(true);
            GC.SuppressFinalize(this);
        }

        ~FileTransReceiver()
        {
            Dispose(false);
        }

        private void Dispose(bool dispose)
        {
            m_srcReader.Close(); m_srcReader = null;
            m_srcWriter.Close(); m_srcWriter = null;
        }

        private void WorkImpl_Async(object state)
        {
            try
            {
                while (m_transedFileCount < m_fileCount)
                {
                    // 测试是否应该结束发送文件
                    {
                        int readInt = m_srcReader.ReadInt32();
                        if (m_transState == FileTransState.Continue)
                        {
                            m_transState = (FileTransState)readInt;
                        }
                        m_srcWriter.Write((int)m_transState);
                        m_srcWriter.Flush();
                        if (m_transState == FileTransState.Abort)
                        {
                            if (Aborted != null) Aborted();
                            break;
                        }
                    }

                    ++m_transedFileCount;

                    string relatFileName = null;
                    int fileSize = 0;
                    BinaryWriter writer = null;

                    {
                        fileSize = m_srcReader.ReadInt32();
                        relatFileName = m_srcReader.ReadString();

                        while (File.Exists(m_localDir + relatFileName)) relatFileName = "_" + relatFileName;
                        {
                            string dirName = Path.GetDirectoryName(m_localDir + relatFileName);
                            if (!Directory.Exists(dirName)) Directory.CreateDirectory(dirName);
                        }
                        writer = new BinaryWriter(new BufferedStream(
                            File.Open(m_localDir + relatFileName, FileMode.Create, FileAccess.Write), 1 << 22));
                    }

                    if (FileTransing != null) FileTransing(relatFileName);

                    if (fileSize > 0)
                    {
                        byte[] buf = new byte[1024];

                        int leftFileSize = fileSize;
                        while (leftFileSize > 0)
                        {
                            int readedBytes = m_srcReader.Read(buf, 0, Math.Min(buf.Length, leftFileSize));
                            if (writer != null) writer.Write(buf, 0, readedBytes);

                            m_transedBytes += readedBytes;
                            leftFileSize -= readedBytes;
                        }
                    }

                    if (writer != null) writer.Close();
                }
            }
            catch (Exception e)
            {
                if (TransCompleted != null) TransCompleted("未知的错误 :\n" + e);
                return;
            }

            if (TransCompleted != null) TransCompleted(null);
        }


        private string m_localDir;
        private bool m_workStart;
        private BinaryReader m_srcReader;
        private BinaryWriter m_srcWriter;
        int m_fileCount;
        int m_transedFileCount;
        private int m_transedBytes;
        FileTransState m_transState;
    }
}
