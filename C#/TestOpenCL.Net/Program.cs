using System.Drawing;
using System.Drawing.Imaging;
using System.Linq;
using System.Runtime.InteropServices;
using OpenCL.Net;
using OpenCL.Net.Extensions;

namespace CSharp2013
{
    internal class Program
    {
        private static void Main(string[] args)
        {
            int width = 800, height = 600;

            var buf = new int[width * height];

            {
                var env = "*N*".CreateCLEnvironment();
                var mem = env.Context.CreateBuffer<int>(width * height * 4, MemFlags.WriteOnly);

                var mainKernel = new julia.main(env.Context);
                mainKernel.Compile();
                mainKernel.Run(env.CommandQueues.First(), mem, 64, -0.1, 0.651, -1.5, 1.5, -1.5, 1.5, (uint)height,
                    (uint)width, 0, 0);

                env.CommandQueues.First().ReadFromBuffer(mem, buf);
            }

            var bm = new Bitmap(width, height, PixelFormat.Format32bppArgb);
            var data = bm.LockBits(new Rectangle(0, 0, width, height), ImageLockMode.ReadOnly,
                PixelFormat.Format32bppArgb);
            Marshal.Copy(buf, 0, data.Scan0, width * height);
            bm.UnlockBits(data);
            bm.Save("1.png");
        }
    }
}