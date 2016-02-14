module MultipleStringSearch

open NUnit.Framework

type ISearcher = 
    abstract LookupFrom : string -> seq<int * int>

type SearcherType =
    | BruteForce = 0
    | ACAutomaton = 1

type private BruteForceSearcher(words : seq<string>) =
    interface ISearcher with
        member this.LookupFrom(document : string) : seq<int * int> =
            Seq.empty

// Aho–Corasick

type private ACAutomatonSearcher(words : seq<string>) =
    interface ISearcher with
        member this.LookupFrom(document : string) : seq<int * int> =
            Seq.empty

let newSeacher (searcherType : SearcherType) (words : seq<string>) =
    match searcherType with
    | SearcherType.BruteForce -> BruteForceSearcher(words) :> ISearcher
    | SearcherType.ACAutomaton -> ACAutomatonSearcher(words) :> ISearcher
    | _ -> failwith (sprintf "Invalid type %A" searcherType)

[<TestFixture>]
type MultipleStringSearcherTest() =
    
    [<Test>]
    member this.TestWithFixedString() =
        ()