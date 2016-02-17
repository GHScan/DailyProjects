module StringSearch

open NUnit.Framework

type ISearcher =
    abstract LookupFrom : string -> seq<int> 

type SearcherType =
    | BruteForce = 0
    | KMP1 = 1
    | KMPK = 2
    | BM = 3
    | BMH = 4
    | RK = 5

let private kCharCount = 128
let inline private getChar (s : string) (index : int) = 
    if index < s.Length then
        let c = int s.[index]
        assert (c < kCharCount)
        c
    elif index = s.Length then
        0
    else
        failwith "Index out of range"

type private BruteForceSearcher(word : string) =
    interface ISearcher with
        member this.LookupFrom(document : string) : seq<int> =
            assert (document.Length > 0 && word.Length > 0)

            seq {
                for i in 0..document.Length-word.Length do
                    if System.String.Compare(word, 0, document, i, word.Length) = 0 then
                        yield i
            }

// Knuth–Morris–Pratt

type private OneWayKMPSearcher(word : string) =
    let failTable = 
        let a = Array.create word.Length -1
        for i in 1..word.Length-1 do
            let mutable j = a.[i-1]
            while j >= 0 && word.[j] <> word.[i-1] do
                j <- a.[j]
            a.[i] <- j + 1
        a

    interface ISearcher with
        member this.LookupFrom(document : string) : seq<int> =
            assert (document.Length > 0 && word.Length > 0)

            let mutable i, j = 0, 0
            seq {
                while i < document.Length do
                    if word.[j] = document.[i] && j < word.Length-1 then
                        j <- j + 1
                        i <- i + 1
                    else
                        if word.[j] = document.[i] then
                            yield i + 1 - word.Length
                        j <- failTable.[j]
                        if j < 0 then
                            j <- 0
                            i <- i + 1
            }

type private KWayKMPSearcher(word : string) =
    let gotoTable = 
        let a = Array2D.create word.Length kCharCount 0
        let mutable j = 0
        if word.Length > 1 then
            a.[0, getChar word 0] <- 1
        for i in 1..word.Length-1 do
            for c in 0..kCharCount-1 do
                a.[i, c] <- a.[j, c]
            if i <> word.Length-1 then
                a.[i, getChar word i] <- i + 1
            j <- a.[j, getChar word i]
        a

    interface ISearcher with
        member this.LookupFrom(document : string) : seq<int> =
            assert (document.Length > 0 && word.Length > 0)

            let mutable i, j = 0, 0
            seq {
                while i < document.Length do
                    if j = word.Length-1 && word.[j] = document.[i] then
                        yield i + 1 - word.Length
                    j <- gotoTable.[j, getChar document i]
                    i <- i + 1
            }

// Boyer–Moore

type private BMSearcher(word : string) =

    let longestSuffix (a : string) (b : string) = 
        let mutable len = 0
        while len < a.Length && len < b.Length 
            && a.[a.Length-1-len] = b.[b.Length-1-len] do
                len <- len + 1
        len

    let nextCharSkip = 
        let a = Array.create kCharCount (word.Length+1)
        for i in 0..word.Length-1 do
            a.[getChar word i] <- word.Length - i
        a

    let goodSuffixSkip = 
        let a = Array.create word.Length 0
        do 
            let mutable skip = word.Length
            for len in 1..word.Length-1 do
                if word.StartsWith(word.[word.Length-len..]) then
                    skip <- word.Length - len
                a.[word.Length-1-len] <- skip
            for i in 1..word.Length-2 do
                let len = longestSuffix word word.[..i]
                if len > 0 && len <= i then
                    a.[word.Length-1-len] <- word.Length - 1 - i
        a

    interface ISearcher with
        member this.LookupFrom(document : string) : seq<int> =
            assert (document.Length > 0 && word.Length > 0)

            let mutable i = 0
            seq {
                while i <= document.Length - word.Length do
                    let mutable j = word.Length-1
                    while j >= 0 && word.[j] = document.[i+j] do
                        j <- j - 1
                    if j < 0 then
                        yield i
                        i <- i + nextCharSkip.[getChar document (i+word.Length)]
                    else
                        i <- i + max goodSuffixSkip.[j] nextCharSkip.[getChar document (i+word.Length)]
            }

// Boyer–Moore–Horspool

type private BMHSearcher(word : string) =
    let skipTable = 
        let a = Array.create kCharCount (word.Length + 1)
        for i in 0..word.Length-1 do
            a.[getChar word i] <- word.Length - i
        a

    interface ISearcher with
        member this.LookupFrom(document : string) : seq<int> =
            assert (document.Length > 0 && word.Length > 0)

            let mutable i = 0
            seq {
                while i <= document.Length - word.Length do
                    if System.String.Compare(word, 0, document, i, word.Length) = 0 then
                        yield i
                    i <- i + skipTable.[getChar document (i + word.Length)]
            }

// Rabin–Karp

type private RKSearcher(word : string) =
    let R = 101
    let RM = Seq.reduce (*) (Array.create (word.Length-1) R)
    let wordHash = Seq.fold (fun v c -> v * R + int c) 0 word

    interface ISearcher with
        member this.LookupFrom(document : string) : seq<int> =
            assert (document.Length > 0 && word.Length > 0)

            if document.Length < word.Length then 
                Seq.empty
            else
                let mutable hash = Seq.fold (fun v c -> v * R + int c) 0 document.[..word.Length-2]
                seq {
                    for i in 0..document.Length-word.Length do
                        hash <- hash * R + int document.[i+word.Length-1]
                        if hash = wordHash 
                            && System.String.Compare(word, 0, document, i, word.Length) = 0 then
                                yield i
                        hash <- hash- RM * int document.[i]
                }

let newSearcher (searcherType : SearcherType) (word : string) =
    match searcherType with
    | SearcherType.BruteForce -> BruteForceSearcher(word) :> ISearcher
    | SearcherType.KMP1 -> OneWayKMPSearcher(word) :> ISearcher 
    | SearcherType.KMPK -> KWayKMPSearcher(word) :> ISearcher 
    | SearcherType.BM -> BMSearcher(word) :> ISearcher
    | SearcherType.BMH -> BMHSearcher(word) :> ISearcher
    | SearcherType.RK -> RKSearcher(word) :> ISearcher
    | _ -> failwith (sprintf "Invalid type %A" searcherType)

[<TestFixture>]
type internal StringSearchTest() =
    
    [<Test>]
    member this.TestWithFixedString() =
        for t in System.Enum.GetValues(typeof<SearcherType>) do
            
            let searcher = newSearcher (unbox<SearcherType> t) "aaa"
            Assert.AreEqual([], searcher.LookupFrom("aa") |> List.ofSeq |> List.sort)
            Assert.AreEqual([0], searcher.LookupFrom("aaa") |> List.ofSeq |> List.sort)
            Assert.AreEqual([0;1], searcher.LookupFrom("aaaa") |> List.ofSeq |> List.sort)
            Assert.AreEqual([0;1;8], searcher.LookupFrom("aaaa aa aaa") |> List.ofSeq |> List.sort)

            let searcher = newSearcher (unbox<SearcherType> t) "abbcab abbaabbc a"
            Assert.AreEqual([], searcher.LookupFrom("aa") |> List.ofSeq |> List.sort)
            Assert.AreEqual([0], searcher.LookupFrom("abbcab abbaabbc a") |> List.ofSeq |> List.sort)
            Assert.AreEqual([], searcher.LookupFrom("abbcab abbbabbc a") |> List.ofSeq |> List.sort)
            Assert.AreEqual([4], searcher.LookupFrom("abbcabbcab abbaabbc a") |> List.ofSeq |> List.sort)
            Assert.AreEqual([0;16], searcher.LookupFrom("abbcab abbaabbc abbcab abbaabbc a") |> List.ofSeq |> List.sort)

    [<Test>]
    member this.TestWithRandomString() =
        for _ in 0..100 do
            let word = new string(Utility.genRandoms 10 0 2 |> Seq.map ((+) 97 >> char) |> Array.ofSeq)

            let searcher = newSearcher SearcherType.BruteForce word
            for t in unbox (System.Enum.GetValues(typeof<SearcherType>)) |> Array.filter (fun v->v<>SearcherType.BruteForce) do
                let testSearcher = newSearcher t word

                for _ in 0..10 do
                    let doc = new string(Utility.genRandoms 100 0 2 |> Seq.map ((+) 97 >> char) |> Array.ofSeq)
                    Assert.AreEqual(searcher.LookupFrom doc |> List.ofSeq, testSearcher.LookupFrom doc |> List.ofSeq |> List.sort)

    member this.BenchSelfSimilarWord() =
        let words = [| for _ in 0..100 -> new string(Utility.genRandoms 16 0 2 |> Seq.map ((+) 97 >> char) |> Array.ofSeq) |]
        let docs = [| for _ in 0..10 -> new string(Utility.genRandoms 1000 0 2 |> Seq.map ((+) 97 >> char) |> Array.ofSeq) |]
        
        for t in System.Enum.GetValues(typeof<SearcherType>) do
            let searchers = [for word in words -> newSearcher (unbox<SearcherType> t) word]
            Utility.timeit ("Bench StringSearch - BenchSelfSimilarWord : " + t.ToString()) 3 
                (fun ()-> 
                    for searcher in searchers do
                        for doc in docs do 
                            searcher.LookupFrom doc |> Seq.length |> ignore)

    member this.BenchNormalWord() =
        let words = [| for _ in 0..100 -> new string(Utility.genRandoms 16 0 25 |> Seq.map ((+) 97 >> char) |> Array.ofSeq) |]
        let docs = [| for _ in 0..10 -> new string(Utility.genRandoms 1000 0 25 |> Seq.map ((+) 97 >> char) |> Array.ofSeq) |]
        
        for t in System.Enum.GetValues(typeof<SearcherType>) do
            let searchers = [for word in words -> newSearcher (unbox<SearcherType> t) word]
            Utility.timeit ("Bench StringSearch - BenchNormalWord : " + t.ToString()) 3 
                (fun ()-> 
                    for searcher in searchers do
                        for doc in docs do 
                            searcher.LookupFrom doc |> Seq.length |> ignore)