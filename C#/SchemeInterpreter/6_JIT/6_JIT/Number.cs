using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using System.Numerics;

namespace _6_JIT {
    static public class Number {
        static public INumber Create(int value) {
            return new NumberInteger() { value=value};
        }
        static public INumber Create(BigInteger value) {
            return new NumberBigInteger() { value = value };
        }
        static public INumber Create(decimal value) {
            return new NumberDecimal() { value = value };
        }
        static public bool TryParse(string s, out INumber num) {
            int i; BigInteger bi; decimal f;

            if (int.TryParse(s, out i)) {
                num = Create(i);
                return true;
            } else if (BigInteger.TryParse(s, out bi)) {
                num = Create(bi);
                return true;
            } else if (decimal.TryParse(s, out f)) {
                num = Create(f);
                return true;
            } else {
                num = null;
                return false;
            }
        }
    }
    
    public interface INumber: IComparable, IFormattable, IEquatable<INumber> {

        INumber Add(INumber rightOperand);
        INumber DispatchAdd(NumberInteger leftOperand);
        INumber DispatchAdd(NumberBigInteger leftOperand);
        INumber DispatchAdd(NumberDecimal leftOperand);


        INumber Sub(INumber rightOperand);
        INumber DispatchSub(NumberInteger leftOperand);
        INumber DispatchSub(NumberBigInteger leftOperand);
        INumber DispatchSub(NumberDecimal leftOperand);


        INumber Mul(INumber rightOperand);
        INumber DispatchMul(NumberInteger leftOperand);
        INumber DispatchMul(NumberBigInteger leftOperand);
        INumber DispatchMul(NumberDecimal leftOperand);


        INumber Div(INumber rightOperand);
        INumber DispatchDiv(NumberInteger leftOperand);
        INumber DispatchDiv(NumberBigInteger leftOperand);
        INumber DispatchDiv(NumberDecimal leftOperand);



        INumber Mod(INumber rightOperand);
        INumber DispatchMod(NumberInteger leftOperand);
        INumber DispatchMod(NumberBigInteger leftOperand);
        INumber DispatchMod(NumberDecimal leftOperand);


        int CompareDispatch(NumberInteger leftOperand);
        int CompareDispatch(NumberBigInteger leftOperand);
        int CompareDispatch(NumberDecimal leftOperand);

        INumber CastToInteger();
        INumber CastToDecimal();
    }

    public class NumberInteger: INumber {
        public int value;
        public INumber Add(INumber rightOperand) {
            return rightOperand.DispatchAdd(this);
        }
        public INumber DispatchAdd(NumberInteger leftOperand) {
            try {
                return Number.Create(leftOperand.value + value);
            } catch (OverflowException) {
                return Number.Create((BigInteger)leftOperand.value + value);
            }
        }
        public INumber DispatchAdd(NumberBigInteger leftOperand) {
            return Number.Create(leftOperand.value + value);
        }
        public INumber DispatchAdd(NumberDecimal leftOperand) {
            return Number.Create(leftOperand.value + value);
        }


        public INumber Sub(INumber rightOperand) {
            return rightOperand.DispatchSub(this);
        }
        public INumber DispatchSub(NumberInteger leftOperand) {
            try {
                return Number.Create(leftOperand.value - value);
            } catch (OverflowException) {
                return Number.Create((BigInteger)leftOperand.value - value);
            }
        }
        public INumber DispatchSub(NumberBigInteger leftOperand) {
            return Number.Create(leftOperand.value - value);
        }
        public INumber DispatchSub(NumberDecimal leftOperand) {
            return Number.Create(leftOperand.value - value);
        }


        public INumber Mul(INumber rightOperand) {
            return rightOperand.DispatchMul(this);
        }
        public INumber DispatchMul(NumberInteger leftOperand) {
            try {
                return Number.Create(leftOperand.value * value);
            } catch (OverflowException) {
                return Number.Create((BigInteger)leftOperand.value * value);
            }
        }
        public INumber DispatchMul(NumberBigInteger leftOperand) {
            return Number.Create(leftOperand.value * value);
        }
        public INumber DispatchMul(NumberDecimal leftOperand) {
            return Number.Create(leftOperand.value * value);
        }


        public INumber Div(INumber rightOperand) {
            return rightOperand.DispatchDiv(this);
        }
        public INumber DispatchDiv(NumberInteger leftOperand) {
            try {
                return Number.Create(leftOperand.value / value);
            } catch (OverflowException) {
                return Number.Create((BigInteger)leftOperand.value / value);
            }
        }
        public INumber DispatchDiv(NumberBigInteger leftOperand) {
            return Number.Create(leftOperand.value / value);
        }
        public INumber DispatchDiv(NumberDecimal leftOperand) {
            return Number.Create(leftOperand.value / value);
        }


        public INumber Mod(INumber rightOperand) {
            return rightOperand.DispatchMod(this);
        }
        public INumber DispatchMod(NumberInteger leftOperand) {
            try {
                return Number.Create(leftOperand.value % value);
            } catch (OverflowException) {
                return Number.Create((BigInteger)leftOperand.value % value);
            }
        }
        public INumber DispatchMod(NumberBigInteger leftOperand) {
            return Number.Create(leftOperand.value % value);
        }
        public INumber DispatchMod(NumberDecimal leftOperand) {
            return Number.Create(leftOperand.value % value);
        }


        public int CompareTo(object o) {
            return ((INumber)o).CompareDispatch(this);
        }
        public int CompareDispatch(NumberInteger leftOperand) {
            return leftOperand.value.CompareTo(value);
        }
        public int CompareDispatch(NumberBigInteger leftOperand) {
            return leftOperand.value.CompareTo(value);
        }
        public int CompareDispatch(NumberDecimal leftOperand) {
            return leftOperand.value.CompareTo(value);
        }

        public INumber CastToInteger() {
            return this;
        }
        public INumber CastToDecimal() {
            return Number.Create((decimal)value);
        }

        public string ToString(string format, IFormatProvider formatProvider) {
            return value.ToString(format, formatProvider);
        }

        public bool Equals(INumber other) {
            return other.CompareDispatch(this) == 0;
        }
        override public bool Equals(object other) {
            var n = other as INumber;
            return n != null && Equals(n);
        }
        override public int GetHashCode() {
            return ((BigInteger)value).GetHashCode();
        }
    }

    public class NumberBigInteger: INumber {
        public BigInteger value;
        public INumber Add(INumber rightOperand) {
            return rightOperand.DispatchAdd(this);
        }
        public INumber DispatchAdd(NumberInteger leftOperand) {
            return Number.Create(leftOperand.value + value);
        }
        public INumber DispatchAdd(NumberBigInteger leftOperand) {
            return Number.Create(leftOperand.value + value);
        }
        public INumber DispatchAdd(NumberDecimal leftOperand) {
            return Number.Create(leftOperand.value + (decimal)value);
        }


        public INumber Sub(INumber rightOperand) {
            return rightOperand.DispatchSub(this);
        }
        public INumber DispatchSub(NumberInteger leftOperand) {
            return Number.Create(leftOperand.value - value);
        }
        public INumber DispatchSub(NumberBigInteger leftOperand) {
            return Number.Create(leftOperand.value - value);
        }
        public INumber DispatchSub(NumberDecimal leftOperand) {
            return Number.Create(leftOperand.value - (decimal)value);
        }


        public INumber Mul(INumber rightOperand) {
            return rightOperand.DispatchMul(this);
        }
        public INumber DispatchMul(NumberInteger leftOperand) {
            return Number.Create(leftOperand.value * value);
        }
        public INumber DispatchMul(NumberBigInteger leftOperand) {
            return Number.Create(leftOperand.value * value);
        }
        public INumber DispatchMul(NumberDecimal leftOperand) {
            return Number.Create(leftOperand.value * (decimal)value);
        }


        public INumber Div(INumber rightOperand) {
            return rightOperand.DispatchDiv(this);
        }
        public INumber DispatchDiv(NumberInteger leftOperand) {
            return Number.Create(leftOperand.value / value);
        }
        public INumber DispatchDiv(NumberBigInteger leftOperand) {
            return Number.Create(leftOperand.value / value);
        }
        public INumber DispatchDiv(NumberDecimal leftOperand) {
            return Number.Create(leftOperand.value / (decimal)value);
        }


        public INumber Mod(INumber rightOperand) {
            return rightOperand.DispatchMod(this);
        }
        public INumber DispatchMod(NumberInteger leftOperand) {
            return Number.Create(leftOperand.value % value);
        }
        public INumber DispatchMod(NumberBigInteger leftOperand) {
            return Number.Create(leftOperand.value % value);
        }
        public INumber DispatchMod(NumberDecimal leftOperand) {
            return Number.Create(leftOperand.value % (decimal)value);
        }

        public int CompareTo(object o) {
            return ((INumber)o).CompareDispatch(this);
        }
        public int CompareDispatch(NumberInteger leftOperand) {
            return -value.CompareTo(leftOperand.value);
        }
        public int CompareDispatch(NumberBigInteger leftOperand) {
            return leftOperand.value.CompareTo(value);
        }
        public int CompareDispatch(NumberDecimal leftOperand) {
            return leftOperand.value.CompareTo((decimal)value);
        }

        public INumber CastToInteger() {
            return this;
        }
        public INumber CastToDecimal() {
            return Number.Create((decimal)value);
        }

        public string ToString(string format, IFormatProvider formatProvider) {
            return value.ToString(format, formatProvider);
        }
        public bool Equals(INumber other) {
            return other.CompareDispatch(this) == 0;
        }
        override public bool Equals(object other) {
            var n = other as INumber;
            return n != null && Equals(n);
        }
        override public int GetHashCode() {
            return value.GetHashCode();
        }
    }

    public class NumberDecimal: INumber {
        public decimal value;
        public INumber Add(INumber rightOperand) {
            return rightOperand.DispatchAdd(this);
        }
        public INumber DispatchAdd(NumberInteger leftOperand) {
            return Number.Create(leftOperand.value + value);
        }
        public INumber DispatchAdd(NumberBigInteger leftOperand) {
            return Number.Create((decimal)leftOperand.value + value);
        }
        public INumber DispatchAdd(NumberDecimal leftOperand) {
            return Number.Create(leftOperand.value + value);
        }


        public INumber Sub(INumber rightOperand) {
            return rightOperand.DispatchSub(this);
        }
        public INumber DispatchSub(NumberInteger leftOperand) {
            return Number.Create(leftOperand.value - value);
        }
        public INumber DispatchSub(NumberBigInteger leftOperand) {
            return Number.Create((decimal)leftOperand.value - value);
        }
        public INumber DispatchSub(NumberDecimal leftOperand) {
            return Number.Create(leftOperand.value - value);
        }


        public INumber Mul(INumber rightOperand) {
            return rightOperand.DispatchMul(this);
        }
        public INumber DispatchMul(NumberInteger leftOperand) {
            return Number.Create(leftOperand.value * value);
        }
        public INumber DispatchMul(NumberBigInteger leftOperand) {
            return Number.Create((decimal)leftOperand.value * value);
        }
        public INumber DispatchMul(NumberDecimal leftOperand) {
            return Number.Create(leftOperand.value * value);
        }


        public INumber Div(INumber rightOperand) {
            return rightOperand.DispatchDiv(this);
        }
        public INumber DispatchDiv(NumberInteger leftOperand) {
            return Number.Create(leftOperand.value / value);
        }
        public INumber DispatchDiv(NumberBigInteger leftOperand) {
            return Number.Create((decimal)leftOperand.value / value);
        }
        public INumber DispatchDiv(NumberDecimal leftOperand) {
            return Number.Create(leftOperand.value / value);
        }


        public INumber Mod(INumber rightOperand) {
            return rightOperand.DispatchMod(this);
        }
        public INumber DispatchMod(NumberInteger leftOperand) {
            return Number.Create(leftOperand.value % value);
        }
        public INumber DispatchMod(NumberBigInteger leftOperand) {
            return Number.Create((decimal)leftOperand.value % value);
        }
        public INumber DispatchMod(NumberDecimal leftOperand) {
            return Number.Create(leftOperand.value % value);
        }

        public int CompareTo(object o) {
            return ((INumber)o).CompareDispatch(this);
        }
        public int CompareDispatch(NumberInteger leftOperand) {
            return -value.CompareTo(leftOperand.value);
        }
        public int CompareDispatch(NumberBigInteger leftOperand) {
            return ((decimal)leftOperand.value).CompareTo(value);
        }
        public int CompareDispatch(NumberDecimal leftOperand) {
            return leftOperand.value.CompareTo(value);
        }

        public INumber CastToInteger() {
            try {
                return Number.Create((int)value);
            } catch (OverflowException) {
                return Number.Create((BigInteger)value);
            }
        }
        public INumber CastToDecimal() {
            return this;
        }

        public string ToString(string format, IFormatProvider formatProvider) {
            return value.ToString(format, formatProvider);
        }
        public bool Equals(INumber other) {
            return other.CompareDispatch(this) == 0;
        }
        override public bool Equals(object other) {
            var n = other as INumber;
            return n != null && Equals(n);
        }
        override public int GetHashCode() {
            BigInteger bi = new BigInteger(value);
            if ((decimal)bi == value) {
                return bi.GetHashCode();
            }
            return value.GetHashCode();
        }
    }
}
