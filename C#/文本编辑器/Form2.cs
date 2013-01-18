using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

namespace CSharp08Form
{
    public partial class Form2 : WeifenLuo.WinFormsUI.Docking.DockContent
    {
        public Form2()
        {
            InitializeComponent();
        }

        public RichTextBox GetRichTextBox() { return richTextBox1;  }
    }
}
