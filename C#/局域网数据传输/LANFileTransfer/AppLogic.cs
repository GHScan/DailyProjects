using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using System.Net.Sockets;
using System.Windows.Forms;
using System.Net;
using System.IO;
using System.Diagnostics;
using System.Xml.Serialization;

namespace LANFileTransfer
{
    enum AppLogicState
    {
        WaitingForDropFile,
        SendingFile,
        ReceivingFile,
    }

    class AppLogic : IDisposable
    {
        public AppLogicState State 
        { 
            get { return m_state; }
            private set
            {
                if (value != m_state)
                {
                    m_state = value;
                    if (StateChanged != null) StateChanged();
                }
            }
        }
        public float TransSpeed 
        {
            get
            {
                if (m_timer == null || m_timer.ElapsedMilliseconds == 0) return 0;
                if (m_fileTransReceiver != null)
                {
                    return (float)m_fileTransReceiver.TransedBytes / ((int)m_timer.ElapsedMilliseconds * 1000);
                }
                if (m_fileTransSender != null)
                {
                    return (float)m_fileTransSender.TransedBytes / ((int)m_timer.ElapsedMilliseconds * 1000);
                }
                return 0;
            }
        }
        public string TransingFileName
        {
            get { return m_transingFile;  }
        }
        public int TransedFileCount
        {
            get
            {
                if (m_fileTransSender != null) return m_fileTransSender.TransedFileCount;
                if (m_fileTransReceiver != null) return m_fileTransReceiver.TransedFileCount;
                return 0;
            }
        }
        public float TransSeconds
        {
            get 
            {
                if (m_timer != null) return m_timer.ElapsedMilliseconds / 1000.0f;
                return 0;
            }
        }

        public event Func<string, bool> FileTransFound;
        public event Action<string> ErrorHandler;
        public event Action<int> ProgressChanged;
        public event Action StateChanged;

        public AppLogic()
        {
            State = AppLogicState.WaitingForDropFile;

            m_udpFileReceiver = new UdpClient(UDP_LISTENER_PORT);
            m_udpFileReceiverResult = m_udpFileReceiver.BeginReceive(
                o => Program.WrapThreadSafeDelegate(this.UDPProtocol_0_1)(), null);
        }

        ~AppLogic()
        {
            Dispose(false);
        }

        public void Dispose()
        {
            Dispose(true);
            GC.SuppressFinalize(this);
        }

        public void SendFiles(string localPath)
        {
            Program.Assert(State == AppLogicState.WaitingForDropFile);
            Program.Assert(Directory.Exists(localPath) || File.Exists(localPath));

            using (UdpClient udpFileSender = new UdpClient(0))
            {
                udpFileSender.Client.ReceiveTimeout = UDP_RECEIVE_TIME_OUT;

                byte[] bytes =
                    UDP_PROTOCOL_STEP_0.Concat(
                        Encoding.Default.GetBytes(localPath.Substring(localPath.LastIndexOf(Path.DirectorySeparatorChar) + 1))
                    ).ToArray();
                udpFileSender.Send(bytes, bytes.Length, new IPEndPoint(IPAddress.Broadcast, UDP_FINDER_PORT));

                IPEndPoint receiverEndPoint = null;
                try
                {
                    bytes = udpFileSender.Receive(ref receiverEndPoint);
                }
                catch { bytes = null;  }
                if (bytes == null || bytes[0] != UDP_PROTOCOL_STEP_1[0]) return;

                udpFileSender.Send(UDP_PROTOCOL_STEP_2, 1, receiverEndPoint);
                State = AppLogicState.SendingFile;

                Program.Assert(m_tcpServer == null && m_tcpClient == null);

                m_tcpServer = new TcpListener(
                    Dns.GetHostAddresses(Dns.GetHostName()).Where(i => i.ToString().StartsWith("192.168.")).ToArray()[0], 0);
                m_tcpServer.Start();
                udpFileSender.Send(Program.ToByteArray(((IPEndPoint)m_tcpServer.LocalEndpoint).Port), 4, receiverEndPoint);

                m_tcpClient = m_tcpServer.AcceptTcpClient();

                Program.Assert(m_fileTransSender == null);
                m_fileTransSender = new FileTransSender(localPath, new BufferedStream(m_tcpClient.GetStream()));
                m_fileTransSender.Aborted += Program.WrapThreadSafeDelegate(() => 
                {
                    if (ErrorHandler != null) ErrorHandler("传输中断！"); OnTransformEnd();
                });
                m_fileTransSender.TransCompleted += Program.WrapThreadSafeDelegate((Action<string>)(s =>
                    {
                        if (s != null && ErrorHandler != null) ErrorHandler(s); OnTransformEnd();
                    }));
                m_fileTransSender.FileTransing += Program.WrapThreadSafeDelegate((Action<string>)(s =>
                {
                    m_transingFile = s;
                    if (ProgressChanged != null) ProgressChanged(m_fileTransSender.TransedFileCount * 100 / m_fileTransSender.FileCount);
                }));
                m_fileTransSender.Work();

                m_timer = new Stopwatch();
                m_timer.Start();
            }
        }

        public void CancelWork()
        {
            switch (State)
            {
                case AppLogicState.SendingFile:
                    m_fileTransSender.Abort();
                    break;
                case AppLogicState.ReceivingFile:
                    m_fileTransReceiver.Abort();
                    break;
                default:
                    break;
            }
        }

        private void UDPProtocol_0_1()
        {
            if (State != AppLogicState.WaitingForDropFile) return;

            do
            {
                IPEndPoint senderEndPoint = null;
                byte[] bytes = m_udpFileReceiver.EndReceive(m_udpFileReceiverResult, ref senderEndPoint);
#if !DEBUG
                if (Dns.GetHostAddresses(Dns.GetHostName()).Any(i => i.Equals(senderEndPoint.Address))) break;
#endif
                if (bytes == null || bytes[0] != UDP_PROTOCOL_STEP_0[0]) break;
                string relatPath = Encoding.Default.GetString(bytes.Skip(1).ToArray());

                if (FileTransFound != null &&
                    FileTransFound(senderEndPoint.ToString() + " 要传输文件'" + relatPath + "'，是否接受？"))
                {
                    m_udpFileReceiver.Send(UDP_PROTOCOL_STEP_1, 1, senderEndPoint);
                }
                else break;

                int oldTimeout = m_udpFileReceiver.Client.ReceiveTimeout; ;
                m_udpFileReceiver.Client.ReceiveTimeout = UDP_RECEIVE_TIME_OUT;
                IPEndPoint senderEndPoint2 = null;
                try
                {
                    bytes = m_udpFileReceiver.Receive(ref senderEndPoint2);
                }
                catch { bytes = null; }
                bool failed = false;
                if (!senderEndPoint.Equals(senderEndPoint2) || bytes == null || bytes[0] != UDP_PROTOCOL_STEP_2[0])
                {
                    failed = true;
                    ErrorHandler("连接失败！");
                }
                m_udpFileReceiver.Client.ReceiveTimeout = oldTimeout;
                if (failed) break;

                State = AppLogicState.ReceivingFile;

                do
                {
                    bytes = m_udpFileReceiver.Receive(ref senderEndPoint);
                    if (bytes.Length == 4 && senderEndPoint.Equals(senderEndPoint2)) break;
                } while (true);

                Program.Assert(m_tcpClient == null);
                m_tcpClient = new TcpClient();
                m_tcpClient.Connect(senderEndPoint.Address, Program.FromByteArray<int>(bytes));

                string localPath = null;
                {
                    FolderBrowserDialog folder = new FolderBrowserDialog();
                    while (folder.ShowDialog() != DialogResult.OK) ;
                    localPath = folder.SelectedPath;
                    if (!localPath.EndsWith(Path.DirectorySeparatorChar.ToString())) localPath += Path.DirectorySeparatorChar.ToString();

                    if (relatPath.IndexOf('.') == -1)
                    {
                        while (Directory.Exists(localPath + relatPath)) relatPath = "_" + relatPath;
                        localPath += relatPath;

                        Directory.CreateDirectory(localPath);
                    }
                    else
                    {
                        localPath += relatPath;
                    }
                }

                m_fileTransReceiver = new FileTransReceiver(localPath, m_tcpClient.GetStream());
                m_fileTransReceiver.Aborted += Program.WrapThreadSafeDelegate(() =>
                {
                    if (ErrorHandler != null) ErrorHandler("传输中断！"); OnTransformEnd();
                });
                m_fileTransReceiver.TransCompleted += Program.WrapThreadSafeDelegate((Action<string>)(s =>
                {
                    if (s != null && ErrorHandler != null) ErrorHandler(s); OnTransformEnd(); 
                }));
                m_fileTransReceiver.FileTransing += Program.WrapThreadSafeDelegate((Action<string>)(s =>
                {
                    m_transingFile = s;
                    if (ProgressChanged != null) ProgressChanged(m_fileTransReceiver.TransedFileCount * 100 / m_fileTransReceiver.FileCount); 
                }));
                m_fileTransReceiver.Work();

                m_timer = new Stopwatch();
                m_timer.Start();
            } while (false);

            if (State == AppLogicState.WaitingForDropFile && m_udpFileReceiverResult.IsCompleted)
            {
                m_udpFileReceiverResult = m_udpFileReceiver.BeginReceive(
                    o => Program.WrapThreadSafeDelegate(this.UDPProtocol_0_1)(), null);
            }
        }

        private void Dispose(bool dispose)
        {
            OnTransformEnd();
            m_udpFileReceiver.Close();
            m_udpFileReceiver = null;
        }

        private void OnTransformEnd()
        {
            switch (State)
            {
                case AppLogicState.SendingFile:
                    m_tcpClient.Close(); m_tcpClient = null;
                    m_tcpServer.Stop(); m_tcpServer = null;
                    m_fileTransSender.Dispose();  m_fileTransSender = null;

                    break;
                case AppLogicState.ReceivingFile:
                    m_tcpClient.Close(); m_tcpClient = null;
                    m_fileTransReceiver.Dispose();  m_fileTransReceiver = null;

                    break;
                default:
                    break;
            }

            State = AppLogicState.WaitingForDropFile;
            if (m_udpFileReceiverResult.IsCompleted)
            {
                m_udpFileReceiverResult = m_udpFileReceiver.BeginReceive(
                    o => Program.WrapThreadSafeDelegate(this.UDPProtocol_0_1)(), null);
            }

            m_transingFile = string.Empty;
            m_timer = null;
        }

        private AppLogicState m_state;
        private UdpClient m_udpFileReceiver;
        private IAsyncResult m_udpFileReceiverResult;
        private TcpListener m_tcpServer;
        private TcpClient m_tcpClient;
        private FileTransReceiver m_fileTransReceiver;
        private FileTransSender m_fileTransSender;
        private Stopwatch m_timer;
        private string m_transingFile;

        static AppLogic()
        {
            if (File.Exists("LANFT_config.xml"))
            {
                using (FileStream fs = File.OpenRead("LANFT_config.xml"))
                {
                    XmlSerializer x = new XmlSerializer(typeof(int[]));
                    int[] a = (int[])x.Deserialize(fs);
                    UDP_LISTENER_PORT = a[0];
                    UDP_FINDER_PORT = a[1];
                }
            }
            else
            {
                UDP_LISTENER_PORT = UDP_FILE_RECEIVER_PORT;
                UDP_FINDER_PORT = UDP_FILE_RECEIVER_PORT;
            }
        }

        private static int UDP_LISTENER_PORT;
        private static int UDP_FINDER_PORT;

        private static int UDP_FILE_RECEIVER_PORT = 1392;
        private static int UDP_RECEIVE_TIME_OUT = 5000;
        private static byte[] UDP_PROTOCOL_STEP_0 = new byte[] { 25, };
        private static byte[] UDP_PROTOCOL_STEP_1 = new byte[] { 154, };
        private static byte[] UDP_PROTOCOL_STEP_2 = new byte[] { 233, };
    }
}