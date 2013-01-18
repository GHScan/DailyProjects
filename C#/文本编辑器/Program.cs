using System;
using System.Collections.Generic;
using System.Windows.Forms;

namespace CSharp08Form
{
    static class DelegateQueue
    {
        public static void PostDelegate(Action a)
        {
            lock(cs_callbackQueue)
            {
                cs_callbackQueue.Enqueue(a);
            }
        }
        public static void InvokeDelegates()
        {
            Action[] a = null;
            lock (cs_callbackQueue)
            {
                a = cs_callbackQueue.ToArray();
                cs_callbackQueue.Clear();
            }
            foreach (var i in a) i();
        }

        static Queue<Action> cs_callbackQueue = new Queue<Action>();
    }

    static class Program
    {
        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        [STAThread]
        static void Main()
        {
            Application.Idle += (o, e) => { DelegateQueue.InvokeDelegates();  };

            Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);
            Application.Run(new Form1());
        }
    }
}
