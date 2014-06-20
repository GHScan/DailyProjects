using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace _6_JIT {
    struct LocalAddress {
        public int index;
    }

    struct FreeAddress {
        public int envIndex;
        public int index;
        public override string ToString() {
            return string.Format("(FreeAddress_{0}_{1})", envIndex, index);
        }
        public FreeAddress GetOuterAddress() {
            if (envIndex == 0) throw new Exception("Invalid operation");
            return new FreeAddress{envIndex=envIndex - 1, index=index};
        }
    }

    struct GlobalAddress {
        public int index;
    }

    class SymbolTable {
        static private SymbolTable sGlobal = new SymbolTable(null);
        private SymbolTable mPrevTable;
        private Dictionary<string, int> mSym2Address = new Dictionary<string, int>();
        private int mNextAddress = 0;
        public SymbolTable(SymbolTable prevTable) {
            mPrevTable = prevTable;
        }

        public LocalAddress DefineLocalSymbol(string name) {
            if (mSym2Address.ContainsKey(name)) throw new Exception("Symbol is aready exist: " + name);
            return new LocalAddress { index = (mSym2Address[name] = mNextAddress++) };
        }
        public int GetLocalSymbolCount() {
            return mSym2Address.Count;
        }
        static public GlobalAddress DefineOrGetGlobalSymbol(string name) {
            int index = 0;
            if (sGlobal.mSym2Address.ContainsKey(name)) {
                index = sGlobal.mSym2Address[name];
            } else {
                index = sGlobal.mSym2Address[name] = sGlobal.mNextAddress++;
            }
            return new GlobalAddress { index = index };
        }
        static public int GetGlobalSymbolCount() {
            return sGlobal.mSym2Address.Count;
        }
        static public object Lookup(SymbolTable table, string name) {
            if (table != null && table.mSym2Address.ContainsKey(name)) {
                return new LocalAddress { index = table.mSym2Address[name] };
            }

            int envIndex = 0;
            while (table != null && !table.mSym2Address.ContainsKey(name)) {
                table = table.mPrevTable;
                ++envIndex;
            }

            if (table == null) {
                return DefineOrGetGlobalSymbol(name);
            } else {
                return new FreeAddress { envIndex = envIndex, index = table.mSym2Address[name] };
            }
        }
    }
}
