using System;
using System.Collections.Generic;
using System.Linq;
using System.Windows.Forms;

namespace LANFileTransfer
{
    static partial class Program
    {
        /// <summary>
        /// 应用程序的主入口点。
        /// </summary>
        [STAThread]
        static void Main()
        {
            AppDomain.CurrentDomain.UnhandledException +=
                (o, e) => { MessageBox.Show(e.ExceptionObject.ToString(), "发生异常！");  };

            Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);
            Application.Run(new FormMain());
        }

        public static Action WrapThreadSafeDelegate(Action a)
        {
            return (Action)(() => 
            {
                if (Application.OpenForms.Count > 0)
                {
                    Application.OpenForms[0].Invoke(a);
                }
            });
        }
        public static Action<T> WrapThreadSafeDelegate<T>(Action<T> a)
        {
            return (Action<T>)((p) =>
            {
                if (Application.OpenForms.Count > 0)
                {
                    Application.OpenForms[0].Invoke(a, p);
                }
            });
        }
    }
}
