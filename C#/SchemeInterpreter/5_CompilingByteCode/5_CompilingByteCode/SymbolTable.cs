using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace _5_CompilingByteCode {
    class LocalVariable {
        public int index;
    }

    class FreeVariable {
        public int envIndex;
        public int index;
    }

    class GlobalVariable {
        public int index;
    }

    class SymbolTable {
        static private SymbolTable sGlobal = new SymbolTable(null);
        private SymbolTable mPrevTable;
        private Dictionary<string, int> mSymLoc = new Dictionary<string, int>();
        private int mNextLoc = 0;
        public SymbolTable(SymbolTable prevTable) {
            mPrevTable = prevTable;
        }

        public LocalVariable DefineLocalSymbol(string name) {
            if (mSymLoc.ContainsKey(name)) throw new Exception("Symbol is aready exist: " + name);
            return new LocalVariable { index = (mSymLoc[name] = mNextLoc++) };
        }
        public int GetLocalSymbolCount() {
            return mSymLoc.Count;
        }
        static public GlobalVariable DefineOrGetGlobalSymbol(string name) {
            int index = 0;
            if (sGlobal.mSymLoc.ContainsKey(name)) {
                index = sGlobal.mSymLoc[name];
            } else {
                index = sGlobal.mSymLoc[name] = sGlobal.mNextLoc++;
            }
            return new GlobalVariable { index = index };
        }
        static public int GetGlobalSymbolCount() {
            return sGlobal.mSymLoc.Count;
        }
        static public object Lookup(SymbolTable table, string name) {
            if (table != null && table.mSymLoc.ContainsKey(name)) {
                return new LocalVariable { index = table.mSymLoc[name] };
            }

            int envIndex = 0;
            while (table != null && !table.mSymLoc.ContainsKey(name)) {
                table = table.mPrevTable;
                ++envIndex;
            }

            if (table == null) {
                return DefineOrGetGlobalSymbol(name);
            } else {
                return new FreeVariable { envIndex = envIndex, index = table.mSymLoc[name] };
            }
        }
    }
}
