using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using System.Diagnostics;
using System.Threading;
using System.Text.RegularExpressions;

namespace CSharp08
{
    partial class Program
    {
        static void Main(string[] args)
        {
            string[] urls = 
            {
                "www.baidu.com", "www.qq.com", "www.sina.com", "www.sohu.com",
                "www.taobao.com", "www.163.com", "www.tianya.cn", "www.mop.com",
            };

            int runningThread = urls.Length;
            AutoResetEvent evt = new AutoResetEvent(false);

            Process[] processes = new Process[urls.Length];
            for (int i = 0; i < urls.Length; ++i)
            {
                ProcessStartInfo info = new ProcessStartInfo("ping", urls[i])
                {
                    RedirectStandardOutput = true,
                    UseShellExecute = false,
                };
                processes[i] = Process.Start(info);
                processes[i].EnableRaisingEvents = true;
                processes[i].Exited += (o, e) => 
                {
                    if (Interlocked.Decrement(ref runningThread) == 0)
                    {
                        evt.Set();
                    }
                };
            }

            evt.WaitOne();

            for (int i = 0; i < urls.Length; ++i)
            {
                string output = processes[i].StandardOutput.ReadToEnd();

                int sum = 0;
                int n = 0;
                foreach (Match match in Regex.Matches(output, " time=(\\d+)ms TTL="))
                {
                    sum += int.Parse(match.Groups[1].ToString());
                    ++n;
                }
                Print(string.Format("{0,-16} : {1} ({2}/4)", urls[i], n > 0 ? sum / n : 0, n));
            }
        }
    }
}