using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using System.Runtime.InteropServices;


namespace _6_JIT {
    class Utils {
        [DllImport("Kernel32.dll")]
        static public extern bool QueryPerformanceCounter(out long lpPerformanceCount);
        [DllImport("Kernel32.dll")]
        static public extern bool QueryPerformanceFrequency(out long lpFrequency);
    }
}
