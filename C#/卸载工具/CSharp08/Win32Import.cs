using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using System.Runtime.InteropServices;

namespace CSharp08
{
    static class Win32Import
    {
        public enum Win32Bool
        {
            True = 1,
            False = 0,
        }

        public struct VS_FixedFileInfo
        {
            [Flags]
            public enum FileFlag
            {
                Debug = 0x00000001,
                PreRlease = 0x00000002,
                Patched = 0x00000004,
                PrivateBuild = 0x00000008,
                InfoInferred = 0x00000010,
                SpecialBuild = 0x00000020,
            }

            [Flags]
            public enum FileOS
            {
                Base = 0x00000000,
                Windows16 = 0x00000001,
                PM16 = 0x00000002,
                PM32 = 0x00000003,
                Windows32 = 0x00000004,

                Unknown = 0x00000000,
                Dos = 0x00010000,
                OS216 = 0x00020000,
                OS232 = 0x00030000,
                NT = 0x00040000,
                WinCE = 0x00050000,
            }

            public enum FileType
            {
                Unknown = 0x00000000,
                App = 0x00000001,
                Dll = 0x00000002,
                Drv = 0x00000003,
                Font = 0x00000004,
                Vxd = 0x00000005,
                StaticLib = 0x00000007,
            }

            public enum FileSubType_Drv
            {
                Unknown = 0x00000000,
                Printer = 0x00000001,
                Keyboard = 0x00000002,
                Language = 0x00000003,
                Display = 0x00000004,
                Mouse = 0x00000005,
                Network = 0x00000006,
                System = 0x00000007,
                Installable = 0x00000008,
                Sound = 0x00000009,
                Comm = 0x0000000A,
                InputMethod = 0x0000000B,
                VersionedPrinter = 0x0000000C,
            }

            public enum FileSubType_Font
            {
                Raster = 0x00000001,
                Vector = 0x00000002,
                TrueType = 0x00000003,
            }

            public uint signature;      // Contains the value 0xFEEF04BD
            public uint structVersion;  // Specifies the binary version number of this structure.
            public uint fileVersionHi;  // the most significant 32 bits of the file's binary version number. 
            public uint fileVersionLo;
            public uint productVersionHi;   // Specifies the most significant 32 bits of the binary version number of the product with which this file was distributed. 
            public uint productVersionLo;
            public uint fileFlagMask;   // Contains a bitmask that specifies the valid bits in dwFileFlags
            public uint fileFlag;   // This member can include one or more of enum FileFlag
            public FileOS fileOS;
            public FileType fileType;
            public uint fileSubType; // enum FileSubType_Drv or enum FileSubType_Font
            public uint fileDateHi;
            public uint fileDateLo;
        }
        
        [DllImport("shell32.dll", CharSet=CharSet.Unicode)]
        public static extern uint ExtractIconEx(
            string lpszFile,
            int nIconIndex,
            IntPtr[] phiconLarge,
            IntPtr[] phiconSmall,
            uint nIcons);

        public static readonly IntPtr HKEY_CLASSES_ROOT = new IntPtr(unchecked((int)0x80000000));
        public static readonly IntPtr HKEY_CURRENT_USER = new IntPtr(unchecked((int)0x80000001));
        public static readonly IntPtr HKEY_LOCAL_MACHINE = new IntPtr(unchecked((int)0x80000002));
        public static readonly IntPtr HKEY_USERS = new IntPtr(unchecked((int)0x80000003));

        [DllImport("Advapi32.dll", CharSet = CharSet.Unicode)]
        public static extern int RegOpenKeyEx(
            IntPtr hKey,
            string lpSubKey,
            uint ulOptions,
            uint samDesired,
            ref IntPtr phkResult);

        [DllImport("Advapi32.dll", CharSet = CharSet.Unicode)]
        public static extern int RegQueryInfoKey(
            IntPtr hKey,
            char[] lpClass,
            uint[] lpcClass,
            uint lpReserved,
            uint[] lpcSubKeys,
            uint[] lpcMaxSubKeyLen,
            uint[] lpcMaxClassLen,
            uint[] lpcValues,
            uint[] lpcMaxValueNameLen,
            uint[] lpcMaxValueLen,
            uint[] lpcbSecurityDescriptor,
            ulong[] lpftLastWriteTime);

        [DllImport("Advapi32.dll")]
        public static extern int RegCloseKey(
            IntPtr hKey);

        [DllImport("version.dll", CharSet = CharSet.Unicode)]
        public static extern uint GetFileVersionInfoSize(
            string lptstrFilename,
            ref uint lpdwHandle);

        [DllImport("version.dll")]
        public static extern Win32Bool GetFileVersionInfo(string lptstrFilename,
            uint dwHandle,
            uint dwLen,
            byte[] lpData);

        [DllImport("version.dll", CharSet = CharSet.Unicode)]
        public static extern int VerQueryValue(
            byte[] pBlock,
            string lpSubBlock,
            ref IntPtr lplpBuffer,
            ref uint puLen);

        [DllImport("shell32.dll", CharSet = CharSet.Unicode)]
        public static extern int ShellExecute(
            IntPtr hwnd,
            string lpOperation,
            string lpFile,
            string lpParameters,string lpDirectory,
            int nShowCmd);

        [DllImport("Kernel32.dll", CharSet = CharSet.Ansi, ExactSpelling=true)]
        public static extern uint WinExec(
            string lpCmdLine,
            int uCmdShow);

        [StructLayout(LayoutKind.Sequential)]
        public struct StartupInfo 
        {
            public uint   cb;
            public IntPtr lpReserved;
            public IntPtr lpDesktop;
            public IntPtr lpTitle;
            public uint dwX;
            public uint dwY;
            public uint dwXSize;
            public uint dwYSize;
            public uint dwXCountChars;
            public uint dwYCountChars;
            public uint dwFillAttribute;
            public uint dwFlags;
            public ushort wShowWindow;
            public ushort cbReserved2;
            public IntPtr lpReserved2;
            public IntPtr hStdInput;
            public IntPtr hStdOutput;
            public IntPtr hStdError;
        } 

        [StructLayout(LayoutKind.Sequential)]
        public struct Process_Information 
        {
            public IntPtr hProcess;
            public IntPtr hThread;
            public uint dwProcessId;
            public uint dwThreadId;
        } 

        [DllImport("Kernel32.dll", CharSet = CharSet.Unicode)]
        public static extern Win32Bool CreateProcess(
          string lpApplicationName,
          char[] lpCommandLine,
          IntPtr lpProcessAttributes,
          IntPtr lpThreadAttributes,
          int bInheritHandles,
          uint dwCreationFlags,
          char[] lpEnvironment,
          string lpCurrentDirectory,
          ref StartupInfo lpStartupInfo,
          ref Process_Information lpProcessInformation);

        [DllImport("Kernel32.dll")]
        public static extern Win32Bool CloseHandle(IntPtr hObject);

        [DllImport("Kernel32.dll")]
        public static extern int WaitForSingleObject(
            IntPtr hHandle,
            uint dwMilliseconds);

        [Flags]
        public enum FormatMessage_Flag
        {
            AllocateBuffer = 0x00000100,
            IgnoreInserts = 0x00000200,
            FromString = 0x00000400,
            FromHModule = 0x00000800,
            FromSystem = 0x00001000,
            AgumentArray = 0x00002000,
        }

        [DllImport("Kernel32.dll", CharSet = CharSet.Unicode)]
        public static extern uint FormatMessage(
            FormatMessage_Flag dwFlags,
            byte[] lpSource,
            uint dwMessageId,
            uint dwLanguageId,
            char[] lpBuffer,
            int nSize,
            IntPtr Arguments);

        [DllImport("Kernel32.dll")]
        public static extern uint GetLastError();
    }
}
