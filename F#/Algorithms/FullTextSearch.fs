module FullTextSearch

open NUnit.Framework 

type ISearcher =
    abstract Lookup : string -> seq<int>

type SearcherType =
    | BruteForce
    | SuffixArray

type private BruteForceSearcher(document : string) = 
    interface ISearcher with
        member this.Lookup(word : string) : seq<int> = 
            seq {
                for i in 0..document.Length-word.Length do
                    if System.String.CompareOrdinal(word, 0, document, i, word.Length) = 0 then
                        yield i
            }

type private SuffixArraySearcher(document : string) = 
    let suffixArray = [|0..document.Length-1|]
    do
        Array.sortInPlaceWith (fun i j -> System.String.CompareOrdinal(document, i, document, j, document.Length)) suffixArray

    interface ISearcher with
        member this.Lookup(word : string) : seq<int> = 
            let mutable pos = Utility.lowerBound suffixArray word (fun i word -> System.String.CompareOrdinal(document, i, word, 0, word.Length))
            [|
                while pos < suffixArray.Length 
                    && document.Length - suffixArray.[pos] >= word.Length 
                    && System.String.CompareOrdinal(word, 0, document, suffixArray.[pos], word.Length) = 0
                    do
                        yield suffixArray.[pos]
                        pos <- pos + 1
            |] :> seq<int>

let newSearcher (searcherType : SearcherType) (document : string) = 
    match searcherType with
    | SearcherType.BruteForce -> BruteForceSearcher(document) :> ISearcher
    | SearcherType.SuffixArray -> SuffixArraySearcher(document) :> ISearcher

[<TestFixture>]
type internal FullTextSearchTest() =
    
    [<Test>]
    member this.TestWithFixedString() =
        for t in [SearcherType.BruteForce; SearcherType.SuffixArray] do
            do
                let searcher = newSearcher t "aaaaa"
                Assert.AreEqual([0;1;2;3;4], searcher.Lookup "a" |> List.ofSeq |> List.sort)
                Assert.AreEqual([0;1;2;3], searcher.Lookup "aa" |> List.ofSeq |> List.sort)
                Assert.AreEqual([0;1;2], searcher.Lookup "aaa" |> List.ofSeq |> List.sort)
                Assert.AreEqual([0;1], searcher.Lookup "aaaa" |> List.ofSeq |> List.sort)
                Assert.AreEqual([0;], searcher.Lookup "aaaaa" |> List.ofSeq |> List.sort)
                Assert.AreEqual([], searcher.Lookup "b" |> List.ofSeq |> List.sort)
                Assert.AreEqual([], searcher.Lookup "bb" |> List.ofSeq |> List.sort)

                let searcher = newSearcher t "abc def abcabc"
                Assert.AreEqual([0;8;11], searcher.Lookup "a" |> List.ofSeq |> List.sort)
                Assert.AreEqual([0;8;11], searcher.Lookup "abc" |> List.ofSeq |> List.sort)
                Assert.AreEqual([], searcher.Lookup "abcd" |> List.ofSeq |> List.sort)
                Assert.AreEqual([4], searcher.Lookup "def" |> List.ofSeq |> List.sort)
                Assert.AreEqual([2], searcher.Lookup "c d" |> List.ofSeq |> List.sort)
                Assert.AreEqual([10], searcher.Lookup "ca" |> List.ofSeq |> List.sort)

    [<Test>]
    member this.TestWithRandomString() =
        for _ in 0..10 do
            let doc = new string(Utility.genRandoms 100 0 3 |> Seq.map ((+) 97 >> char) |> Array.ofSeq)

            let searcher = newSearcher SearcherType.BruteForce doc
            for t in [SearcherType.SuffixArray] do
                let testSearcher = newSearcher t doc

                for _ in 0..100 do
                    let word = new string(Utility.genRandoms 4 0 3 |> Seq.map ((+) 97 >> char) |> Array.ofSeq)
                    Assert.AreEqual(searcher.Lookup word |> List.ofSeq, testSearcher.Lookup word |> List.ofSeq |> List.sort)

    // [<Test>]
    member this.BenchWithRandomString() =
        let doc = new string(Utility.genRandoms 10000 0 3 |> Seq.map ((+) 97 >> char) |> Array.ofSeq)
        let words = [| for _ in 0..100 -> new string(Utility.genRandoms 8 0 3 |> Seq.map ((+) 97 >> char) |> Array.ofSeq) |]
        
        for t in [SearcherType.BruteForce; SearcherType.SuffixArray] do
            let searcher = newSearcher t doc
            Utility.timeit ("Bench FullTextSearch : " + t.ToString()) 3 
                (fun ()-> for w in words do searcher.Lookup w |> Seq.length |> ignore)