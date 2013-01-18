using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;

namespace CSharp08Form
{
    public partial class Form3 : WeifenLuo.WinFormsUI.Docking.DockContent
    {
        public Form3()
        {
            InitializeComponent();
        }

        public PropertyGrid GetPropertyGrid() { return propertyGrid1;  }
    }
}
