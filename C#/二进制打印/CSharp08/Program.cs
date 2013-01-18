using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using System.Windows.Forms;
using System.Drawing;

namespace CSharp08
{
    partial class Program
    {
        [STAThread]
        static void Main(string[] args)
        {
            Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);

            Form form = new Form();
            FlowLayoutPanel flowLayoutPanelTop;
            RadioButton radioButtonText;
            RadioButton radioButtonNumber;
            ComboBox comboBox;
            Panel panelFill;
            TextBox textBoxInput;
            TextBox textBoxOutput;
            StatusStrip statusStrip;
            ToolStripStatusLabel statusLabel;

            // 布局
            {
                flowLayoutPanelTop = new FlowLayoutPanel();
                radioButtonText = new RadioButton();
                radioButtonNumber = new RadioButton();
                comboBox = new ComboBox();
                panelFill = new Panel();
                textBoxInput = new TextBox();
                textBoxOutput = new TextBox();
                statusStrip = new StatusStrip();
                statusLabel = new ToolStripStatusLabel();
                flowLayoutPanelTop.SuspendLayout();
                panelFill.SuspendLayout();
                statusStrip.SuspendLayout();
                form.SuspendLayout();
                // 
                // flowLayoutPanel1
                // 
                flowLayoutPanelTop.Controls.Add(radioButtonText);
                flowLayoutPanelTop.Controls.Add(radioButtonNumber);
                flowLayoutPanelTop.Controls.Add(comboBox);
                flowLayoutPanelTop.Dock = DockStyle.Top;
                flowLayoutPanelTop.Location = new Point(0, 0);
                flowLayoutPanelTop.Name = "flowLayoutPanel1";
                flowLayoutPanelTop.Size = new Size(363, 30);
                flowLayoutPanelTop.TabIndex = 0;
                // 
                // radioButton1
                // 
                radioButtonText.AutoSize = true;
                radioButtonText.Location = new Point(3, 3);
                radioButtonText.Name = "radioButton1";
                radioButtonText.Size = new Size(47, 16);
                radioButtonText.TabIndex = 0;
                radioButtonText.TabStop = true;
                radioButtonText.Text = "文本";
                radioButtonText.UseVisualStyleBackColor = true;
                // 
                // radioButton2
                // 
                radioButtonNumber.AutoSize = true;
                radioButtonNumber.Location = new Point(56, 3);
                radioButtonNumber.Name = "radioButton2";
                radioButtonNumber.Size = new Size(47, 16);
                radioButtonNumber.TabIndex = 0;
                radioButtonNumber.TabStop = true;
                radioButtonNumber.Text = "数字";
                radioButtonNumber.UseVisualStyleBackColor = true;
                // 
                // comboBox1
                // 
                comboBox.FormattingEnabled = true;
                comboBox.Location = new Point(109, 3);
                comboBox.Name = "comboBox1";
                comboBox.Size = new Size(121, 20);
                comboBox.TabIndex = 1;

                comboBox.AutoCompleteSource = AutoCompleteSource.ListItems;
                comboBox.AutoCompleteMode = AutoCompleteMode.Suggest;
                // 
                // panel1
                // 
                panelFill.Controls.Add(textBoxOutput);
                panelFill.Controls.Add(textBoxInput);
                panelFill.Dock = DockStyle.Fill;
                panelFill.Location = new Point(0, 30);
                panelFill.Name = "panel1";
                panelFill.Size = new Size(363, 230);
                panelFill.TabIndex = 1;
                // 
                // textBox1
                // 
                textBoxInput.Dock = DockStyle.Top;
                textBoxInput.Location = new Point(0, 0);
                textBoxInput.Multiline = true;
                textBoxInput.Name = "textBox1";
                textBoxInput.Size = new Size(363, 74);
                textBoxInput.TabIndex = 0;
                // 
                // textBox2
                // 
                textBoxOutput.Dock = DockStyle.Bottom;
                textBoxOutput.Location = new Point(0, 80);
                textBoxOutput.Multiline = true;
                textBoxOutput.Name = "textBox2";
                textBoxOutput.ReadOnly = true;
                textBoxOutput.Size = new Size(363, 150);
                textBoxOutput.TabIndex = 1;
                // 
                // statusStrip1
                // 
                statusStrip.Items.AddRange(new ToolStripItem[] {statusLabel});
                statusStrip.Location = new Point(0, 238);
                statusStrip.Name = "statusStrip1";
                statusStrip.Size = new Size(363, 22);
                statusStrip.TabIndex = 2;
                statusStrip.Text = "statusStrip1";
                // 
                // toolStripStatusLabel1
                // 
                statusLabel.Name = "toolStripStatusLabel1";
                statusLabel.Size = new Size(29, 17);
                statusLabel.Spring = true;
                statusLabel.TextAlign = ContentAlignment.MiddleLeft;
                // 
                // Form1
                // 
                form.AutoScaleDimensions = new SizeF(6F, 12F);
                form.AutoScaleMode = AutoScaleMode.Font;
                form.ClientSize = new Size(363, 260);
                form.Controls.Add(statusStrip);
                form.Controls.Add(panelFill);
                form.Controls.Add(flowLayoutPanelTop);
                form.Name = "Form1";
                form.Text = "二进制打印";
                form.StartPosition = FormStartPosition.CenterScreen;
                form.ImeMode = ImeMode.On;
                flowLayoutPanelTop.ResumeLayout(false);
                flowLayoutPanelTop.PerformLayout();
                panelFill.ResumeLayout(false);
                panelFill.PerformLayout();
                statusStrip.ResumeLayout(false);
                statusStrip.PerformLayout();
                form.ResumeLayout(false);
                form.PerformLayout();
            }

            radioButtonText.Click += (o, e) => 
            {
                textBoxInput.Text = "";
                textBoxOutput.Text = "";

                var commonUse = new List<string>();
                commonUse.Add(Encoding.Unicode.WebName);
                commonUse.Add(Encoding.UTF8.WebName);
                commonUse.Add(Encoding.Default.WebName);
                commonUse.Add(Encoding.UTF7.WebName);
                commonUse.Add(Encoding.UTF32.WebName);

                var fullList = Encoding.GetEncodings().Select(i=>i.GetEncoding().WebName);

                comboBox.DataSource = commonUse.Concat(fullList.Except(commonUse)).ToArray();
            };

            radioButtonNumber.Click += (o, e) =>
            {
                textBoxInput.Text = "";
                textBoxOutput.Text = "";

                comboBox.DataSource = new string[] { "unsigned oct", "signed oct", "hex", };
            };

            radioButtonText.PerformClick();

            Func<byte[], string> formatBytes = bytes =>
            {
                if (bytes == null || bytes.Length == 0) return string.Empty;

                var buf = new StringBuilder();
                foreach (byte b in bytes) buf.AppendFormat("{0:x2} ", b);
                buf.Remove(buf.Length - 1, 1);
                return buf.ToString();
            };

            EventHandler intput2output = (o, e) =>
            {
                textBoxOutput.Text = string.Empty;
                if (string.IsNullOrEmpty(textBoxInput.Text)) return;

                if (radioButtonText.Checked)
                {
                    try
                    {
                        Encoding encoding = Encoding.GetEncoding(comboBox.Text);
                        textBoxOutput.Text = formatBytes(encoding.GetBytes(textBoxInput.Text));
                    }
                    catch (Exception _e) 
                    { 
                        statusLabel.Text = _e.Message.Replace("\r\n", "\t");
                    }
                }
                else
                {
                    try
                    {
                        if (comboBox.Text == "hex")
                        {
                            uint i = uint.Parse(textBoxInput.Text, System.Globalization.NumberStyles.HexNumber);
                            textBoxOutput.Text = formatBytes(ToByteArray(i));
                        }
                        else if (comboBox.Text == "signed oct")
                        {
                            int i = int.Parse(textBoxInput.Text);
                            textBoxOutput.Text = formatBytes(ToByteArray(i));
                        }
                        else if (comboBox.Text == "unsigned oct")
                        {
                            uint i = uint.Parse(textBoxInput.Text);
                            textBoxOutput.Text = formatBytes(ToByteArray(i));
                        }
                        else
                        {
                            statusLabel.Text = "未知的格式。";
                        }
                    }
                    catch (Exception _e)
                    { 
                        statusLabel.Text = _e.Message.Replace("\r\n", "\t");
                    }
                }
            };

            textBoxInput.TextChanged += intput2output;
            comboBox.SelectedValueChanged += intput2output;

            textBoxOutput.DoubleClick += (o, e) =>
            {
                if (string.IsNullOrEmpty(textBoxOutput.Text)) return;
                textBoxOutput.SelectAll();
                textBoxOutput.Copy();
                statusLabel.Text = "复制到剪贴板。";
            };

            Timer timerCleanStatus = new Timer();
            timerCleanStatus.Interval = 3000;
            timerCleanStatus.Tick += (o, e) => { statusLabel.Text = ""; };
            timerCleanStatus.Start();

            Application.Run(form);
        }
    }
}