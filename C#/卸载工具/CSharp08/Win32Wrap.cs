using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using System.Runtime.InteropServices;

namespace CSharp08
{
    static class Win32Wrap
    {
        public struct FileVersionInfo
        {
            public struct LanguageBasedFileInfo
            {
                public ushort language;
                public ushort codePage;

                public string comments;
                public string internalName;
                public string productName;
                public string companyName;
                public string legalCopyright;
                public string productVersion;
                public string fileDescription;
                public string legalTrademarks;
                public string privateBuild;
                public string fileVersion;
                public string originalFilename;
                public string specialBuild;
            }

            public Win32Import.VS_FixedFileInfo fixedInfo;
            public LanguageBasedFileInfo[] langBasedInfoList;
        }

        public static FileVersionInfo GetFileVersionInfo(string path)
        {
            uint unuse = 0;
            uint bufSize = Win32Import.GetFileVersionInfoSize(path, ref unuse);
            if (bufSize != 0)
            {
                byte[] buf = new byte[bufSize];
                if (Win32Import.GetFileVersionInfo(path, 0, bufSize, buf) != 0)
                {
                    FileVersionInfo fileVersionInfo = new FileVersionInfo();

                    IntPtr data = IntPtr.Zero;
                    uint dataSize = 0;
                    if (Win32Import.VerQueryValue(buf, @"\", ref data, ref dataSize) > 0 &&
                        dataSize == Marshal.SizeOf(fileVersionInfo.fixedInfo))
                    {
                        fileVersionInfo.fixedInfo = (Win32Import.VS_FixedFileInfo)Marshal.PtrToStructure(
                            data, typeof(Win32Import.VS_FixedFileInfo));
                    }

                    short[] langCodeList = null;
                    if (Win32Import.VerQueryValue(buf, @"\VarFileInfo\Translation", ref data, ref dataSize) > 0)
                    {
                        langCodeList = new short[dataSize / 2];
                        Marshal.Copy(data, langCodeList, 0, langCodeList.Length);
                    }

                    if (langCodeList != null)
                    {
                        fileVersionInfo.langBasedInfoList = new FileVersionInfo.LanguageBasedFileInfo[langCodeList.Length / 2];
                        for (int i = 0; i < fileVersionInfo.langBasedInfoList.Length; ++i)
                        {
                            FileVersionInfo.LanguageBasedFileInfo langBasedInfo = new FileVersionInfo.LanguageBasedFileInfo();

                            langBasedInfo.language = unchecked((ushort)langCodeList[i * 2]);
                            langBasedInfo.codePage = unchecked((ushort)langCodeList[i * 2 + 1]);

                            Func<string, string> verQueryString = s =>
                            {
                                string fmt =
                                string.Format(
                                "\\StringFileInfo\\{0:x4}{1:x4}\\{2}",
                                langBasedInfo.language, langBasedInfo.codePage, s);

                                if (Win32Import.VerQueryValue(buf, fmt, ref data, ref dataSize) > 0)
                                {
                                    char[] tempBuf = new char[dataSize / 2];
                                    Marshal.Copy(data, tempBuf, 0, tempBuf.Length);
                                    return new string(tempBuf);
                                }
                                return string.Empty;
                            };

                            langBasedInfo.comments = verQueryString("Comments");
                            langBasedInfo.internalName = verQueryString("InternalName"); ;
                            langBasedInfo.productName = verQueryString("ProductName"); ;
                            langBasedInfo.companyName = verQueryString("CompanyName"); ;
                            langBasedInfo.legalCopyright = verQueryString("LegalCopyright"); ;
                            langBasedInfo.productVersion = verQueryString("ProductVersion"); ;
                            langBasedInfo.fileDescription = verQueryString("FileDescription"); ;
                            langBasedInfo.legalTrademarks = verQueryString("LegalTrademarks"); ;
                            langBasedInfo.privateBuild = verQueryString("PrivateBuild"); ;
                            langBasedInfo.fileVersion = verQueryString("FileVersion"); ;
                            langBasedInfo.originalFilename = verQueryString("OriginalFilename"); ;
                            langBasedInfo.specialBuild = verQueryString("SpecialBuild"); ;

                            fileVersionInfo.langBasedInfoList[i] = langBasedInfo;
                        }
                    }

                    return fileVersionInfo;
                }
            }

            return new FileVersionInfo();
        }

        public static DateTime? GetRegstryKeyLastWriteTime(IntPtr rootKey, string path)
        {
            IntPtr key = IntPtr.Zero;
            if (Win32Import.RegOpenKeyEx(rootKey, path, 0, 0x20019, ref key) == 0)
            {
                ulong[] fileTime = new ulong[1];
                if (Win32Import.RegQueryInfoKey(
                    key, null, null, 0, null, null, null,
                    null, null, null, null, fileTime) == 0)
                {
                    return DateTime.FromFileTimeUtc(unchecked((long)fileTime[0])).ToLocalTime();
                }
                Win32Import.RegCloseKey(key);
            }
            return null;
        }

        public static bool WinExecAndWait(string cmd, uint timeout)
        {
            var startupInfo = new Win32Import.StartupInfo();
            startupInfo.cb = (uint)Marshal.SizeOf(startupInfo);
            var processInfo = new Win32Import.Process_Information();
            Win32Import.Win32Bool r = 
                Win32Import.CreateProcess(null, cmd.ToCharArray(),
                IntPtr.Zero, IntPtr.Zero, 0, 0, null, null, ref startupInfo, ref processInfo);
            if (r == Win32Import.Win32Bool.False) return false;
            Win32Import.WaitForSingleObject(processInfo.hProcess, timeout);
            Win32Import.CloseHandle(processInfo.hThread);
            Win32Import.CloseHandle(processInfo.hProcess);
            return true;
        }

        public static string GetLastErrorMsg()
        {
            return GetErrorMsg(Win32Import.GetLastError());
        }

        public static string GetErrorMsg(uint id)
        {
            char[] buf = new char[256];
            if (Win32Import.FormatMessage(
                Win32Import.FormatMessage_Flag.FromSystem,
                null, id, 0, buf, buf.Length, IntPtr.Zero) > 0)
                return new string(buf);
            return string.Empty;
        }
    }
}
