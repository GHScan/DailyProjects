using System;

namespace Functional {
    using System.Linq;
    public class List<T> {
        public T Value { get; private set; }
        public List<T> Next { get; private set; }
        public List(T value, List<T> next) {
            Value = value;
            Next = next;
        }

        public override string ToString() {
            return string.Format("List({0})", string.Join(
                ",",
                Foldl(new System.Collections.Generic.List<T>(), (b, v) => { b.Add(v); return b; }).Select(v => v.ToString()).ToArray()));
        }

        public bool IsNil { get { return Next == null; } }
        public static List<T> Nil = new List<T>(default(T), null);

        public List<U> Select<U>(Func<T, U> f) {
            return IsNil ? List<U>.Nil : new List<U>(f(Value), Next.Select(f));
        }
        public List<T> Where(Func<T, bool> f) {
            return IsNil ? this : (f(Value) ? new List<T>(Value, Next.Where(f)) : Next.Where(f));
        }
        public U Foldl<U>(U init, Func<U, T, U> f) {
            return IsNil ? init : Next.Foldl(f(init, Value), f);
        }
        public List<T> Concat(List<T> o) {
            return IsNil ? o : new List<T>(Value, Next.Concat(o));
        }
    }
    public static class List {
        public static List<T> Items<T>(params T[] values) {
            var l = List<T>.Nil;
            for (var i = values.Length - 1; i >= 0; --i) {
                l = new List<T>(values[i], l);
            }
            return l;
        }
    }
}

namespace ParserCombinator {
    using Functional;

    namespace Parsers {
        public interface ILocation {
        }

        public interface IReader {
            ILocation Location { get; }
            bool Eof { get; }
        }

        public interface IParseResult<out T> {
            IParseResult<U> SelectMany<U>(Func<T, IReader, IParseResult<U>> f);
        }
        public interface IFailure<out T>: IParseResult<T> {
        }
        sealed public class Success<T>: IParseResult<T> {
            public T Value { get; private set; }
            public IReader Reader { get; private set; }
            public Success(T value, IReader reader) {
                Value = value;
                Reader = reader;
            }
            public IParseResult<U> SelectMany<U>(Func<T, IReader, IParseResult<U>> f) {
                return f(Value, Reader);
            }
            public override string ToString() {
                return string.Format("Success: {0}", Value);
            }
        }
        sealed public class Failure<T>: IFailure<T> {
            public IReader Reader { get; private set; }
            public string Message { get; private set; }
            public Failure(string message, IReader reader) {
                Message = message;
                Reader = reader;
            }
            public IParseResult<U> SelectMany<U>(Func<T, IReader, IParseResult<U>> f) {
                return new Failure<U>(Message, Reader);
            }
            public override string ToString() {
                return string.Format("Failure: {0}, {1}", Message, Reader);
            }
        }
        sealed public class Error<T>: IParseResult<T> {
            public IReader Reader { get; private set; }
            public string Message { get; private set; }
            public Error(string message, IReader reader) {
                Message = message;
                Reader = reader;
            }
            public IParseResult<U> SelectMany<U>(Func<T, IReader, IParseResult<U>> f) {
                return new Error<U>(Message, Reader);
            }
            public override string ToString() {
                return string.Format("Error: {0}, {1}", Message, Reader);
            }
        }

        public static class Parser {
            public static Parser<T> Succ<T>(T value) {
                return new Parser<T>(reader => new Success<T>(value, reader));
            }
            public static Parser<T> Fail<T>(string message) {
                return new Parser<T>(reader => new Failure<T>(message, reader));
            }
            public static Parser<T> Err<T>(string message) {
                return new Parser<T>(reader => new Error<T>(message, reader));
            }
        }

        public interface IParser<out T> {
            IParseResult<T> Parse(IReader reader);
        }
        public class Parser<T>: IParser<T> {
            private Func<IReader, IParseResult<T>> mFunc { get; set; }
            public Parser(Func<IReader, IParseResult<T>> f) {
                mFunc = f;
            }
            public virtual IParseResult<T> Parse(IReader reader) {
                return mFunc(reader);
            }

            public Parser<U> Select<U>(Func<T, U> f) {
                return new Parser<U>(reader => Parse(reader).SelectMany(
                    (v, reader2) => new Success<U>(f(v), reader2)));
            }
            public Parser<P> SelectMany<U, P>(Func<T, Parser<U>> f, Func<T, U, P> selector) {
                return new Parser<P>(reader => Parse(reader).SelectMany(
                    (v, reader2) => f(v).Parse(reader2).SelectMany(
                        (v2, reader3) => new Success<P>(selector(v, v2), reader3))));
            }
            public Parser<T> Where(Func<T, bool> f) {
                return new Parser<T>(reader => Parse(reader).SelectMany(
                    (v, reader2) => f(v) ? new Success<T>(v, reader2) as IParseResult<T> : new Failure<T>("Where failed:" + f, reader2)));
            }

            public static Parser<T> operator |(Parser<T> a, IParser<T> b) {
                return new Parser<T>(reader => {
                    var r = a.Parse(reader);
                    return r is IFailure<T> ? b.Parse(reader) : r;
                });
            }

            public Parser<object> asObject() {
                return Select(v => (object)v);
            }
            public Parser<U> Drop<U>(Parser<U> p) {
                return from _ in this
                       from x in p
                       select x;
            }
            public Parser<T> DropRight<U>(Parser<U> p) {
                return from x in this
                       from _ in p
                       select x;
            }
            public Parser<List<T>> Opt() {
                return Select(v => List.Items(v))
                    | Parser.Succ(List<T>.Nil);
            }
            public Parser<List<T>> Rep() {
                return (from x in this
                        from xs in Rep()
                        select new List<T>(x, xs))
                        | Parser.Succ(List<T>.Nil);
            }
            public Parser<List<T>> Rep1() {
                return (from x in this
                        from xs in Rep()
                        select new List<T>(x, xs));
            }
            public Parser<T> Chainl1<U>(Parser<U> elem, Parser<Func<T, U, T>> sep) {
                return from x in this
                       from pairs in (from op in sep
                                      from v in elem
                                      select new { Op = op, Value = v }).Rep()
                       select pairs.Foldl(x, (x2, v) => v.Op(x2, v.Value));
            }
            public Parser<T> Chainl1(Parser<Func<T, T, T>> sep) {
                return Chainl1(this, sep);
            }
            public Parser<List<T>> Repsep1<U>(Parser<U> sep) {
                return Select(v => List.Items(v)).Chainl1(
                    this,
                    sep.Select<Func<List<T>, T, List<T>>>(_ => (List<T> xs, T x) => xs.Concat(List.Items(x))));
            }
            public Parser<List<T>> Repsep<U>(Parser<U> sep) {
                return Repsep1(sep) | Parser.Succ(List<T>.Nil);
            }
        }

        public class ParserProxy<T>: Parser<T> {
            public Parser<T> Parser { get; set; }
            public ParserProxy()
                : base(null) {
            }
            public override IParseResult<T> Parse(IReader reader) {
                return Parser.Parse(reader);
            }
        }
    }

    namespace RegexParsers {
        using System.Text.RegularExpressions;
        using Parsers;

        public class StringLocation: ILocation {
            public int Line { get; private set; }
            public int Column { get; private set; }
            public StringLocation(int line, int column) {
                Line = line;
                Column = column;
            }
            public override string ToString() {
                return string.Format("Localtion(Line={0},Column={1})", Line, Column);
            }
        }

        public class StringReader: IReader {
            public string Source { get; private set; }
            public int Offset { get; private set; }
            public StringLocation Location { get; private set; }
            ILocation IReader.Location { get { return Location; } }
            public bool Eof { get { return Offset == Source.Length; } }
            public int RestLength { get { return Source.Length - Offset; } }
            public StringReader(string source, int offset, StringLocation location) {
                Source = source;
                Offset = offset;
                Location = location;
            }
            public StringReader Advance(int step) {
                int offset = Offset;
                int line = Location.Line, column = Location.Column;
                for (; step > 0; --step, ++offset) {
                    if (Source[offset] == '\n') {
                        ++line;
                        column = 1;
                    } else {
                        ++column;
                    }
                }
                return new StringReader(Source, offset, new StringLocation(line, column));
            }
            public override string ToString() {
                return string.Format("Reader(Prefix={0},Location={1})", Source.Substring(Offset, Math.Min(16, RestLength)), Location);
            }
        }

        public static class RegexParser {
            public static Parser<string> String(string s) {
                return WhiteSpaceParser.Opt().Drop(BuildParserFromString(s));
            }
            public static Parser<string> Regex(string pattern) {
                return WhiteSpaceParser.Opt().Drop(BuildParserFromRegex(pattern));
            }
            public static Parser<T> Phrase<T>(Parser<T> p) {
                var phrase = WhiteSpaceParser.Opt().Drop(p.DropRight(WhiteSpaceParser.Opt()));
                return new Parser<T>(reader => {
                    return phrase.Parse(reader).SelectMany((v, reader2) =>
                        reader2.Eof ? new Success<T>(v, reader2) as IParseResult<T> : new Failure<T>("Input is longer than a phrase", reader2));
                });
            }

            private static Parser<string> WhiteSpaceParser = BuildParserFromRegex(@"\s+");

            private static Parser<string> BuildParserFromString(string s) {
                return new Parser<string>(reader => {
                    var reader2 = reader as StringReader;
                    if (string.Compare(reader2.Source, reader2.Offset, s, 0, s.Length) == 0) {
                        return new Success<string>(s, reader2.Advance(s.Length));
                    } else {
                        return new Failure<string>(string.Format("Expected : {0}", s), reader2);
                    }
                });
            }
            private static Parser<string> BuildParserFromRegex(string pattern) {
                pattern = pattern[0] == '^' ? pattern : "^" + pattern;
                var re = new Regex(pattern, RegexOptions.Compiled | RegexOptions.Singleline);

                return new Parser<string>(reader => {
                    var reader2 = reader as StringReader;
                    var m = re.Match(reader2.Source, reader2.Offset, reader2.RestLength);
                    if (m.Success) {
                        return new Success<string>(m.Value, reader2.Advance(m.Value.Length));
                    } else {
                        return new Failure<string>(string.Format("Expected: {0}", pattern), reader2);
                    }
                });
            }
        }
    }
}

namespace ParserDemo {
    using Functional;
    using ParserCombinator.Parsers;
    using ParserCombinator.RegexParsers;
    public class JsonParser {
        private IParser<object> mParser { get; set; }
        public JsonParser() {
            Parser<object> valueParser = null;

            var stringParser =
                RegexParser.Regex("\".*?\"");
            var numberParser =
                RegexParser.Regex(@"\d+").Select(v => double.Parse(v));
            var keyValueParser =
                from key in stringParser
                from _ in RegexParser.String(":")
                from value in valueParser
                select Tuple.Create(key, value);
            var arrayParser =
                from _ in RegexParser.String("[")
                from xs in valueParser.Repsep(RegexParser.String(","))
                from _2 in RegexParser.String("]")
                select xs;
            var dictParser =
                from _ in RegexParser.String("{")
                from xs in keyValueParser.Repsep(RegexParser.String(","))
                from _2 in RegexParser.String("}")
                select xs;
            valueParser = dictParser.asObject()
               | arrayParser
               | numberParser.asObject()
               | stringParser
               | Parser.Fail<object>("Parse value failed");

            mParser = RegexParser.Phrase(valueParser);
        }
        public object Parse(string input) {
            return mParser.Parse(new StringReader(input, 0, new StringLocation(1, 1)));
        }
    }
}

namespace Test {
    using ParserDemo;
    using System.Collections.Generic;
    using System.Linq;
    using System.Text;
    public class Program {
        static void TestJsonParser() {
            foreach (var s in new string[]{
              @"234",
              @" ""afsdjk"" ",
              @"[3,2,""fads"",4]  ",
              @"{""a"" : 1  , ""b"":2,  ""c "":12}  ",
              @"{""a"":1,""b"":2, ""c"" : [1,2,{""d"":4,""e"":5}]}  ",
            }) {
                Console.WriteLine(new JsonParser().Parse(s));
            }
        }
        public static void Main(string[] args) {
            TestJsonParser();
        }
    }
}
