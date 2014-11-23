using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;
using System.Diagnostics;
using System.Reflection;
using System.Collections.Concurrent;
using System.Text.RegularExpressions;
using System.Linq.Expressions;

namespace CSharp13 {
    public class Program {
        public static class NamedFormat {
            private static readonly Regex NameParseRegex = new Regex(@"(?<!{){(\w+)(:[^}]+)?}(?!})", RegexOptions.Compiled | RegexOptions.Singleline | RegexOptions.Multiline);

            public static class FormatterCache<T> {
                public static readonly ConcurrentDictionary<string, Func<T, string>> Cache = new ConcurrentDictionary<string, Func<T, string>>();
                public static Func<T, string> GetFormatter(string formatStr) {
                    Func<T, string> formatter;
                    if (Cache.TryGetValue(formatStr, out formatter)) return formatter;

                    string[] fieldNames;
                    var normalizedFormatStr = NormalizeFormatString(formatStr, out fieldNames);

                    var obj = Expression.Parameter(typeof(T), "obj");
                    var args = Expression.NewArrayInit(typeof(object), fieldNames.Select(name => Expression.Convert(Expression.PropertyOrField(obj, name), typeof(object))));
                    var body = Expression.Call(
                        typeof(string).GetMethod("Format", new[] { typeof(string), typeof(object[]) }),
                        Expression.Constant(normalizedFormatStr), args);
                    formatter = Expression.Lambda<Func<T, string>>(body, obj).Compile();

                    Cache.TryAdd(formatStr, formatter);
                    return formatter;
                }
                private static string NormalizeFormatString(string formatStr, out string[] fieldNames) {
                    var name2Idx = new Dictionary<string, int>();
                    var result = NameParseRegex.Replace(formatStr, m => { 
                        string name = m.Groups[1].Value;
                        int idx;
                        if (!name2Idx.TryGetValue(name, out idx)) { 
                            idx = name2Idx.Count;
                            name2Idx.Add(name, idx);
                        }
                        return string.Format("{{{0}{1}}}", idx, m.Groups[2].Value);
                    });
                    fieldNames = name2Idx.OrderBy(kv => kv.Value).Select(kv => kv.Key).ToArray();
                    return result;
                }
            }

            public static string Format<T>(string formatStr, T obj) {
                return FormatterCache<T>.GetFormatter(formatStr)(obj);
            }
        }
        public static class NamedFormatForDictionary {
            private static readonly Regex NameParseRegex = new Regex(@"(?<!{){(\w+)(:[^}]+)?}(?!})", RegexOptions.Compiled | RegexOptions.Singleline | RegexOptions.Multiline);
            private class FormatStringMeta {
                public string[] Names;
                public object[] TempValues;
                public string NormalizedFormatStr;
            }
            private static readonly Dictionary<string, FormatStringMeta> FormatStrMetaCache = new Dictionary<string, FormatStringMeta>();
            private static FormatStringMeta ParseFormatString(string formatStr) {
                var name2Idx = new Dictionary<string, int>();
                string result = NameParseRegex.Replace(formatStr, m => { 
                    var name = m.Groups[1].Value;
                    int idx;
                    if (!name2Idx.TryGetValue(name, out idx)) { 
                        idx = name2Idx.Count;
                        name2Idx.Add(name, idx);
                    }
                    return string.Format("{{{0}{1}}}", idx, m.Groups[2].Value);
                });

                return new FormatStringMeta {
                    Names = name2Idx.OrderBy(kv => kv.Value).Select(kv => kv.Key).ToArray(),
                    TempValues = new object[name2Idx.Count],
                    NormalizedFormatStr = result
                };
            }
            public static string Format<T>(string formatStr, IDictionary<string, T> dict) {
                FormatStringMeta meta;
                if (!FormatStrMetaCache.TryGetValue(formatStr, out meta)) {
                    meta = ParseFormatString(formatStr);
                    FormatStrMetaCache.Add(formatStr, meta);
                }

                var names = meta.Names;
                var values = meta.TempValues;
                for (var i = 0; i < names.Length; ++i) {
                    values[i] = dict[names[i]];
                }

                return string.Format(meta.NormalizedFormatStr, values);
            }
        }

        public static void Main(string[] args) {
            const int N = 1000;
            string formatStr = "{Name} is {Age:0.0} years old";
            // string formatStr = "{Name} is {Age:0.0} years old, {Name} is {Age:0.000} years old, {Name} is {Age:0.####} years old";

            {
                string text = null;
                var input = new {
                    Name = "Santa",
                    Age = 1700.25
                };

                Utils.Timeit(N, () => {
                    text = NamedFormat.Format(formatStr, input);
                });

                Console.WriteLine(text);
            }

            {
                string text = null;
                var input = new Dictionary<string, object>
                { 
                    { "Name", "Santa" },
                    {"Age" , 1700.25}
                };

                Utils.Timeit(N, () => {
                    text = NamedFormatForDictionary.Format(formatStr, input);
                });

                Console.WriteLine(text);
            }

            {
                string text = null;

                Utils.Timeit(N, () => {
                    text = string.Format("{0} is {1:0.0} years old", "Santa", 1700.25);
                });

                Console.WriteLine(text);
            }
        }
    }
}
