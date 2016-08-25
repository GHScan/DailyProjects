using System;
using System.Diagnostics;
using System.Text;

namespace IDHashStore.Managed.Utils
{
    public static class CommandLine
    {
        public static bool Run(
            string fileName, string arguments, string input, TimeSpan timeout,
            out string output, out string error, bool killWhenTimeout = true)
        {
            using (var process = new Process())
            {
                process.StartInfo = new ProcessStartInfo(fileName, arguments)
                {
                    RedirectStandardInput = !string.IsNullOrEmpty(input),
                    RedirectStandardOutput = true,
                    RedirectStandardError = true,
                    CreateNoWindow = true,
                    WindowStyle = ProcessWindowStyle.Hidden,
                    UseShellExecute = false
                };

                var outputStringBuilder = new StringBuilder();
                process.OutputDataReceived += (sender, args) => outputStringBuilder.Append(args.Data);
                var errorStringBuilder = new StringBuilder();
                process.ErrorDataReceived += (sender, args) => errorStringBuilder.Append(args.Data);

                process.Start();
                process.BeginOutputReadLine();
                process.BeginErrorReadLine();

                if (!string.IsNullOrEmpty(input))
                {
                    process.StandardInput.Write(input);
                    process.StandardInput.Close();
                }

                if (!process.WaitForExit((int)timeout.TotalMilliseconds))
                {
                    if (killWhenTimeout)
                    {
                        process.Kill();
                    }

                    throw new TimeoutException(
                        string.Format("FileName={0}, Arguments='{1}', Input={2}\nOutput={3}\nError={4}",
                        fileName, arguments, input, outputStringBuilder, errorStringBuilder));
                }

                output = outputStringBuilder.ToString();
                error = errorStringBuilder.ToString();

                return process.ExitCode == 0;
            }
        }
    }
}
