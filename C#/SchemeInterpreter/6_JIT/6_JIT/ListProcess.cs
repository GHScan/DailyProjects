using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace _6_JIT {
    //=================================================================
    class Pair {
        public object Car;
        public object Cdr;
    }       

    class ListProcess {
        static public Pair ListToPair(List<object> l) {
            Pair r = null;
            for (int i = l.Count - 1; i >= 0; --i) {
                r = new Pair() {
                    Car = l[i], Cdr = r
                };
            }
            return r;
        }

        static public List<object> PairToList(Pair l) {
            List<object> r = new List<object>();
            while (l != null) {
                r.Add(l.Car);
                l = (Pair)l.Cdr;
            }
            return r;
        }

        static public Pair ConcatListAndPairToNewPair(List<object> l, Pair p) {
            for (int i = l.Count - 1; i >= 0; --i) {
                p = new Pair { Car = l[i], Cdr = p };
            }
            return p;
        }

        static public object PairExpToListExp(object exp) {
            if (exp is Pair) {
                return PairToList((Pair)exp);
            } else {
                return exp;
            }
        }

        static public object ListExpToPairExp(object exp) {
            if (exp is List<object>) {
                return ListToPair((List<object>)exp);
            } else {
                return exp;
            }
        }

        static public void PrintListExp(object exp) {
            if (exp is List<object>) {
                List<object> o = (List<object>)exp;
                Console.Write("(");
                for (int i = 0; i < o.Count; ++i) {
                    if (i > 0) Console.Write(" ");
                    PrintListExp(o[i]);
                }
                Console.Write(")");
            } else {
                Console.Write(exp);
            }
        }

        static public void PrintPairExp(object exp) {
            PrintListExp(PairExpToListExp(exp));
            Console.WriteLine();
        }

        static public object TransformLibraryForms(object exp) {
            List<object> l = exp as List<object>;
            if (l == null) return exp;

            switch (l[0] as string) {
                case "quote":
                    return exp;
                case "if":
                    return new List<object>() { "if", TransformLibraryForms(l[1]), TransformLibraryForms(l[2]), TransformLibraryForms(l[3]) };
                case "lambda":
                    return new List<object>() { "lambda", l[1], TransformLibraryForms(new List<object>() { "begin" }.Concat(l.Skip(2)).ToList()) };
                case "begin":
                    return new List<object>() { "begin" }.Concat(l.Skip(1).Select(TransformLibraryForms)).ToList();
                case "cond":
                    List<object> caseExps = (List<object>)l[1];
                    if (l.Count == 2) {
                        return TransformLibraryForms(new List<object>() { "begin" }.Concat(caseExps).ToList());
                    } else {
                        return TransformLibraryForms(new List<object>(){"if", caseExps[0], 
                            new List<object>(){"begin"}.Concat(caseExps.Skip(1)).ToList(),
                            new List<object>(){"cond"}.Concat(l.Skip(2)).ToList()});
                    }
                case "define":
                    if (l[1] is string) {
                        return new List<object>() { "define", l[1], TransformLibraryForms(l[2]) };
                    } else {
                        List<object> nameAndFormals = (List<object>)l[1];
                        return new List<object>(){"define", nameAndFormals[0], 
                            TransformLibraryForms(
                                new List<object>(){ "lambda", nameAndFormals.Skip(1).ToList(), }.Concat(l.Skip(2)).ToList()
                            )};
                    }
                case "set!":
                    return new List<object>() { "set!", l[1], TransformLibraryForms(l[2]) };
                case "let":
                    List<object> nameValues = (List<object>)l[1];
                    var names = nameValues.Select(a => ((List<object>)a)[0]);
                    var values = nameValues.Select(a => ((List<object>)a)[1]);
                    List<object> lambda = new List<object> { "lambda", names.ToList() }.Concat(l.Skip(2)).ToList();
                    List<object> newExp = new List<object> { lambda }.Concat(values).ToList();
                    return TransformLibraryForms(newExp);
                default:
                    return l.Select(TransformLibraryForms).ToList();
            }
        }

        static public void FindDefinition(List<string> defines, List<object> exps, int off) {
            for (int i = off; i < exps.Count; ++i) {
                List<object> l = exps[i] as List<object>;
                if (l == null) continue;
                switch (l[0] as string) {
                    case "define":
                        defines.Add((string)l[1]);
                        break;
                    case "begin":
                        FindDefinition(defines, l, 1);
                        break;
                    default:
                        break;
                }
            }
        }
    }
      
}
