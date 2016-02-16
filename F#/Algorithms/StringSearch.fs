module StringSearch

open NUnit.Framework

type ISearcher =
    abstract LookupFrom : string -> seq<int> 

type SearcherType =
    | BruteForce
    | KMP1
    | KMPK
    | BM
    | BMH
    | RK

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
            if j >= 0 then
                a.[i] <- j + 1
            else
                a.[i] <- 0
        a

    interface ISearcher with
        member this.LookupFrom(document : string) : seq<int> =
            let mutable j = 0
            seq {
                for i in 0..document.Length-1 do
                    while j >= 0 && document.[i] <> word.[j] do
                        j <- failTable.[j]
                    if j = word.Length-1 then
                        j <- j - 1
                        yield i
                    j <- j + 1
            }

type private KWayKMPSearcher(word : string) =
    let gotoTable = 
        let a = Array.create word.Length (Array.create kCharCount 0)
        let mutable j = 0
        if word.Length > 1 then
            a.[0].[getChar word 0] <- 1
        for i in 1..word.Length-1 do
            for c in 0..kCharCount do
                a.[i].[c] <- a.[j].[c]
            if i <> word.Length-1 then
                a.[i].[getChar word i] <- i + 1
        a

    interface ISearcher with
        member this.LookupFrom(document : string) : seq<int> =
            let mutable j = 0
            seq {
                for i in 0..document.Length-1 do
                    if j = word.Length-1 && word.[j] = document.[i] then
                        yield i
                    j <- gotoTable.[j].[getChar document i]
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
            let mutable i = 0
            seq {
                while i <= document.Length - word.Length do
                    if System.String.Compare(word, 0, document, i, word.Length) = 0 then
                        yield i
                    i <- i + skipTable.[getChar document (i + word.Length)]
            }

// Rabin–Karp

type private RKSearcher(word : string) =
    let Q = 295075153
    let R = 101
    let RM = Seq.fold (fun v _ -> v * R % Q) 1 [2..word.Length]
    let wordHash = Seq.fold (fun v c -> (v * R + int c) % Q) 0 word

    interface ISearcher with
        member this.LookupFrom(document : string) : seq<int> =
            let mutable hash = Seq.fold (fun v c -> (v * R + int c) % Q) 0 document.[..word.Length-2]
            seq {
                for i in 0..document.Length-word.Length do
                    hash <- (hash * R + int document.[i+word.Length-1]) % Q
                    if hash = wordHash 
                        && System.String.Compare(word, 0, document, i, word.Length) = 0 then
                            yield i
                    hash <- (hash - RM * int document.[i]) % Q
            }

let newSearcher (searcherType : SearcherType) (word : string) =
    match searcherType with
    | SearcherType.BruteForce -> BruteForceSearcher(word) :> ISearcher
    | SearcherType.KMP1 -> OneWayKMPSearcher(word) :> ISearcher 
    | SearcherType.KMPK -> KWayKMPSearcher(word) :> ISearcher 
    | SearcherType.BM -> BMSearcher(word) :> ISearcher
    | SearcherType.BMH -> BMHSearcher(word) :> ISearcher
    | SearcherType.RK -> RKSearcher(word) :> ISearcher

[<TestFixture>]
type internal StringSearchTest() =
    
    [<Test>]
    member this.TestWithFixedString() =
        ()