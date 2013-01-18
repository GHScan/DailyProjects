using System;

using System.Diagnostics;
using System.Runtime.InteropServices;
using System.Reflection;
using System.IO;

namespace CSharp08
{
    partial class Program
    {
        public static void Print(params object[] a)
        {
            foreach (var i in a)
            {
                Console.Write("{0}\t", i ?? "null");
            }
            Console.WriteLine("");
        }

        public static void Assert(bool b)
        {
            Debug.Assert(b);
        }

        public static void PerfTimerFunc(Action f, params string[] name)
        {
            Assert(name.Length <= 1);

            Stopwatch watch = new Stopwatch();
            watch.Start();
            f();
            watch.Stop();
            float seconds = (watch.ElapsedMilliseconds / 1000.0f);

            if (name.Length > 0) Print(name[0], ":", seconds);
            else Print(seconds);
        }

        public static byte[] ToByteArray<T>(T a)
        where T : struct
        {
            int size = Marshal.SizeOf(typeof(T));
            IntPtr p = Marshal.AllocHGlobal(size);
            try
            {
                Marshal.StructureToPtr(a, p, false);

                byte[] r = new byte[size];
                Marshal.Copy(p, r, 0, r.Length);
                return r;
            }
            finally
            {
                Marshal.FreeHGlobal(p);
            }
        }

        public static T FromByteArray<T>(byte[] a)
        where T : struct
        {
            Assert(a.Length == Marshal.SizeOf(typeof(T)));

            IntPtr p = Marshal.AllocHGlobal(a.Length);
            try
            {
                Marshal.Copy(a, 0, p, a.Length);

                T r = (T)Marshal.PtrToStructure(p, typeof(T));
                return r;
            }
            finally
            {
                Marshal.FreeHGlobal(p);
            }
        }

        public static void DumpPropertyAndFields(object o)
        {
            _DumpPropertyAndFields(o, null, null);
        }
        public static void DumpPropertyAndFields(object o, string dumpFile)
        {
            _DumpPropertyAndFields(o, null, dumpFile);
        }
        public static void DumpPropertyAndFields(Type t)
        {
            _DumpPropertyAndFields(null, t, null);
        }
        public static void DumpPropertyAndFields(Type t, string dumpFile)
        {
            _DumpPropertyAndFields(null, t, dumpFile);
        }
        private static void _DumpPropertyAndFields(object o, Type t, string dumpFile)
        {
            if (o == null) Assert(t != null);
            else
            {
                if (t == null) t = o.GetType();
                else Assert(t == o.GetType());
            }

            StreamWriter file = null;
            Action<string> printFunc = s =>
            {
                if (file != null) file.WriteLine(s ?? "null");
                else Print(s);
            };

            try
            {
                if (!string.IsNullOrEmpty(dumpFile))
                {
                    file = new StreamWriter(dumpFile, false, System.Text.Encoding.Default);
                }

                // Static Property
                printFunc("------------------------\nStatic Property :\n------------------------");
                foreach (PropertyInfo info in t.GetProperties(
                    BindingFlags.Public | BindingFlags.Static))
                {
                    object val = string.Empty;
                    if (info.GetIndexParameters().Length > 0) val = "...";
                    else
                    {
                        try
                        {
                            val = info.GetValue(null, null) ?? "null";
                        }
                        catch { }
                    }
                    printFunc(string.Format("{0,-32} = {1}", info.Name, val));
                }

                // Static Fields
                printFunc("------------------------\nStatic Fields :\n------------------------");
                foreach (FieldInfo info in t.GetFields(
                    BindingFlags.Public | BindingFlags.Static))
                {
                    object val = string.Empty;
                    try
                    {
                        val = info.GetValue(null) ?? "null";
                    }catch {}
                    printFunc(string.Format("{0,-32} = {1}", info.Name, val));
                }

                if (o != null)
                {
                    // Instance Property
                    printFunc("------------------------\nInstance Property :\n------------------------");

                    foreach (PropertyInfo info in o.GetType().GetProperties(
                        BindingFlags.Public | BindingFlags.Instance))
                    {
                        object val = string.Empty;
                        if (info.GetIndexParameters().Length > 0) val = "...";
                        else
                        {
                            try
                            {
                                val = info.GetValue(o, null) ?? "null";
                            }
                            catch { }
                        }
                        printFunc(string.Format("{0,-32} = {1}", info.Name, val));
                    }

                    // Instance Fields
                    printFunc("------------------------\nInstance Fields :\n------------------------");

                    foreach (FieldInfo info in o.GetType().GetFields(
                        BindingFlags.Public | BindingFlags.Instance))
                    {
                        object val = string.Empty;
                        try
                        {
                            val = info.GetValue(o) ?? "null";
                        }
                        catch { }
                        printFunc(string.Format("{0,-32} = {1}", info.Name, val));
                    }
                }
            }
            finally
            {
                if (file != null)
                {
                    file.Dispose();
                    file = null;
                }
            }
        }

        class ScopePerfTimer : IDisposable
        {
            public ScopePerfTimer(long[] sumMilliseconds)
            {
                m_sumMilliseconds = sumMilliseconds;
                m_watch = Stopwatch.StartNew();
            }
            public void Dispose()
            {
                if (m_watch == null) return;

                m_watch.Stop();
                m_sumMilliseconds[0] += m_watch.ElapsedMilliseconds;
                m_watch = null;
                m_sumMilliseconds = null;

                GC.SuppressFinalize(this);
            }
            private Stopwatch m_watch;
            private long[] m_sumMilliseconds;
        }

        class PropertyWraper<T>
        {
            public PropertyWraper(T val) { m_val = val; }
            public event Action<PropertyWraper<T>> ValueQuery;
            public event Action<PropertyWraper<T>> ValueChanged;
            public event Action<PropertyWraper<T>> ValueChanging;
            public T Value 
            {
                get 
                {
                    if (ValueQuery != null) ValueQuery(this);
                    return m_val;
                }
                set 
                {
                    if (ValueChanging != null) ValueChanging(this);
                    m_val = value;
                    if (ValueChanged != null) ValueChanged(this);
                }
            }
            private T m_val;
        }

        public class AppRegistryConfig : IDisposable
        {
            public AppRegistryConfig(string company, string app)
            {
                m_appKey = Microsoft.Win32.Registry.LocalMachine.CreateSubKey(string.Format(@"Software\{0}\{1}", company, app));
            }
            public string GetConfig(string conf, string def)
            {
                object val = m_appKey.GetValue(conf);
                if (val == null) return def;
                return val.ToString();
            }
            public void SetConfig(string conf, object val)
            {
                m_appKey.SetValue(conf, val.ToString());
            }

            public void Dispose()
            {
                if (m_appKey != null)
                {
                    m_appKey.Close();
                    m_appKey = null;
                    GC.SuppressFinalize(this);
                }
            }
            private Microsoft.Win32.RegistryKey m_appKey;
        }
    }
}