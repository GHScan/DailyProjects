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

type private BruteForceSearcher(word : string) =
    interface ISearcher with
        member this.LookupFrom(document : string) : seq<int> =
            Seq.empty

// Knuth–Morris–Pratt

type private OneWayKMPSearcher(word : string) =
    interface ISearcher with
        member this.LookupFrom(document : string) : seq<int> =
            Seq.empty

type private KWayKMPSearcher(word : string) =
    interface ISearcher with
        member this.LookupFrom(document : string) : seq<int> =
            Seq.empty

// Boyer–Moore

type private BMSearcher(word : string) =
    interface ISearcher with
        member this.LookupFrom(document : string) : seq<int> =
            Seq.empty

// Boyer–Moore–Horspool

type private BMHSearcher(word : string) =
    interface ISearcher with
        member this.LookupFrom(document : string) : seq<int> =
            Seq.empty

let newSearcher (searcherType : SearcherType) (word : string) =
    match searcherType with
    | SearcherType.BruteForce -> BruteForceSearcher(word) :> ISearcher
    | SearcherType.KMP1 -> OneWayKMPSearcher(word) :> ISearcher 
    | SearcherType.KMPK -> KWayKMPSearcher(word) :> ISearcher 
    | SearcherType.BM -> BMSearcher(word) :> ISearcher
    | SearcherType.BMH -> BMHSearcher(word) :> ISearcher
    | _ -> failwith (sprintf "Invalid type %A" searcherType)

[<TestFixture>]
type internal StringSearchTest() =
    
    [<Test>]
    member this.TestWithFixedString() =
        ()