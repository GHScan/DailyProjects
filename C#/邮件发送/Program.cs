using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using System.Net.Mail;
using System.Diagnostics;

namespace CSharp08
{
    partial class Program
    {
        static void SendMail(
            string _receivers, 
            string subject, string content, string attachments)
        {
            string[] receivers = _receivers.Split(';');
            string sender = "testuser@126.com";

            MailMessage msg = new MailMessage(sender, receivers[0])
            {
                Body = content,
                BodyEncoding = Encoding.UTF8,
                IsBodyHtml = false,
                Priority = MailPriority.Normal,
                ReplyTo = new MailAddress(sender),
                Sender = new MailAddress(sender),
                Subject = subject,
                SubjectEncoding = Encoding.UTF8,
            };
            for (int i = 1; i < receivers.Length; ++i) msg.CC.Add(receivers[i]);
            if (!string.IsNullOrEmpty(attachments))
            {
                foreach (var fileName in attachments.Split(';'))
                {
                    msg.Attachments.Add(new Attachment(fileName));
                }
            }

            SmtpClient client = new SmtpClient("smtp.126.com", 25)
            {
                Credentials = new System.Net.NetworkCredential("testuser", "123456"),
                EnableSsl = true,
            };
            client.Send(msg);
        }

        static void Main(string[] args)
        {
            SendMail("testuser@qq.com", "c#测试", "这是一行大姐夫卡死的\nfkjsdl \noyes！", @"D:\Downloads\1.txt"); 
        }
    }
}