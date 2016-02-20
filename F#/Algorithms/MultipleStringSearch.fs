module MultipleStringSearch

open NUnit.Framework

type ISearcher = 
    abstract LookupFrom : string -> seq<int * int>

type SearcherType =
    | BruteForce = 0
    | ACAutomaton = 1
    | RK = 2

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

type private BruteForceSearcher(words : seq<string>) =
    let words = Array.ofSeq words

    interface ISearcher with
        member this.LookupFrom(document : string) : seq<int * int> =
            seq {
                for i in 0..document.Length-1 do
                    for j in 0..words.Length-1 do
                        if document.Length-i >= words.[j].Length &&
                            System.String.CompareOrdinal(words.[j], 0, document, i, words.[j].Length) = 0 then
                                yield (i, j)
            } 

// Aho–Corasick

type private ACAutomatonNode(parent : ACAutomatonNode option, depth : int) = 
    let children : ACAutomatonNode option[] = Array.create kCharCount None 
    let mutable value : int option = None

    member val Fail : ACAutomatonNode = Unchecked.defaultof<ACAutomatonNode> with get, set

    member this.Parent = parent
    member this.Depth = depth
    member this.Children = children
    member this.Value = value

    member this.Insert(key : string, _value : int) = 
        if depth = key.Length then
            assert (value.IsNone || value.Value = _value)
            value <- Some _value
        else
            let c = getChar key depth
            if children.[c].IsNone then
                children.[c] <- Some (ACAutomatonNode(Some this, depth + 1))
            children.[c].Value.Insert(key, _value)

    static member BuildFails(root : ACAutomatonNode) =
        let q = new System.Collections.Generic.Queue<int * ACAutomatonNode>()
        root.Children |> Array.iteri (fun i c -> c |> Option.iter (fun c -> q.Enqueue(i, c)))

        while q.Count > 0 do
            let (c, n) = q.Dequeue()
            if obj.ReferenceEquals(root, n.Parent.Value) then
                n.Fail <- root
            else
                let mutable fail = n.Parent.Value.Fail
                while fail.Children.[c].IsNone && fail.Parent.IsSome do
                    fail <- fail.Fail
                if fail.Children.[c].IsSome then
                    fail <- fail.Children.[c].Value
                n.Fail <- fail
            n.Children |> Array.iteri (fun i c -> c |> Option.iter (fun c -> q.Enqueue(i, c)))

type private ACAutomatonSearcher(words : seq<string>) =
    let root = 
        let n = ACAutomatonNode(None, 0)
        words |> Seq.iteri (fun i w -> n.Insert(w, i))
        ACAutomatonNode.BuildFails(n)
        n

    interface ISearcher with
        member this.LookupFrom(document : string) : seq<int * int> =
            let genOccurs (n : ACAutomatonNode) (i : int) = 
                let mutable i = i
                let mutable n = n
                seq {
                    while n.Parent.IsSome do
                        match n.Value with
                        | None -> ()
                        | Some j -> yield (i - n.Depth, j)
                        i <- i - 1
                        n <- n.Parent.Value
                }

            let mutable n = root
            seq {
                for i in 0..document.Length do
                    let c = getChar document i
                    while n.Parent.IsSome && n.Children.[c].IsNone do
                        yield! genOccurs n i
                        n <- n.Fail
                    if n.Children.[c].IsSome then
                        n <- n.Children.[c].Value
            }

// Rabin–Karp 

type private RKSearcher(words : seq<string>) =
    let words = words |> Array.ofSeq
    let wordLength = 
        assert (words |> Seq.exists (fun w -> w.Length <> words.[0].Length) |> not)
        words.[0].Length
    let R = 101
    let RM = Seq.reduce (*) (Array.create (wordLength - 1) R)
    let wordHashs = 
        words 
        |> Seq.mapi (fun i w->(Seq.fold (fun v c -> v * R + int c) 0 w, i)) 
        |> Seq.groupBy fst 
        |> Seq.map (fun (k,l) -> (k, l |> Seq.map snd))
        |> Map.ofSeq

    interface ISearcher with
        member this.LookupFrom(document : string) : seq<int * int> =
            if document.Length < wordLength then
                Seq.empty
            else
                let mutable hash = Seq.fold (fun v c -> v * R + int c) 0 document.[0..wordLength-2]
                seq {
                    for i in 0..document.Length-wordLength do
                        hash <- hash * R + int document.[i+wordLength-1]
                        match wordHashs.TryFind hash with
                        | None -> ()
                        | Some js -> 
                            for j in js do
                                if System.String.CompareOrdinal(words.[j], 0, document, i, wordLength) = 0 then
                                    yield (i, j)
                        hash <- hash - RM * int document.[i]
                }

let newSearcher (searcherType : SearcherType) (words : seq<string>) =
    match searcherType with
    | SearcherType.BruteForce -> BruteForceSearcher(words) :> ISearcher
    | SearcherType.ACAutomaton -> ACAutomatonSearcher(words) :> ISearcher
    | SearcherType.RK -> RKSearcher(words) :> ISearcher
    | _ -> failwith (sprintf "Invalid type %A" searcherType)

[<TestFixture>]
type internal MultipleStringSearcherTest() =
    
    [<Test>]
    member this.TestWithFixedString() =
        for t in unbox (System.Enum.GetValues(typeof<SearcherType>)) |> Array.filter (fun v->v<>SearcherType.RK) do
            
            let searcher = newSearcher (unbox<SearcherType> t) ["a";"ab";"abc";"b";"bc";"c";"d"]
            Assert.AreEqual([0,0], searcher.LookupFrom("a") |> List.ofSeq |> List.sort)
            Assert.AreEqual([1,0], searcher.LookupFrom(" a") |> List.ofSeq |> List.sort)
            Assert.AreEqual([0,0;1,0;2,0;2,1;3,3], searcher.LookupFrom("aaab") |> List.ofSeq |> List.sort)
            Assert.AreEqual([1,0;1,1;2,3], searcher.LookupFrom(" ab") |> List.ofSeq |> List.sort)
            Assert.AreEqual([1,0;1,1;1,2;2,3;2,4;3,5], searcher.LookupFrom(" abc") |> List.ofSeq |> List.sort)
            Assert.AreEqual([1,0;1,1;1,2;2,3;2,4;3,5;4,6], searcher.LookupFrom(" abcd") |> List.ofSeq |> List.sort)

        for t in System.Enum.GetValues(typeof<SearcherType>) do
            
            let searcher = newSearcher (unbox<SearcherType> t) ["ab";"ac";"ca"]
            Assert.AreEqual([], searcher.LookupFrom("a") |> List.ofSeq |> List.sort)
            Assert.AreEqual([1,0], searcher.LookupFrom(" ab") |> List.ofSeq |> List.sort)
            Assert.AreEqual([1,1;2,2], searcher.LookupFrom("aaca") |> List.ofSeq |> List.sort)
            Assert.AreEqual([0,0;2,1;4,2], searcher.LookupFrom("abacca") |> List.ofSeq |> List.sort)

    [<Test>]
    member this.TestWithFixedLenRandomString() =
        for _ in 0..3 do
            let words = 
                seq { for _ in 1..32 -> new string(Utility.genRandoms 8 0 2 |> Seq.map ((+) 97 >> char) |> Array.ofSeq) }
                |> Seq.distinct
                |> Array.ofSeq

            let searcher = newSearcher SearcherType.BruteForce words
            for t in unbox (System.Enum.GetValues(typeof<SearcherType>)) |> Array.filter (fun v->v<>SearcherType.BruteForce) do
                let testSearcher = newSearcher t words

                for _ in 0..10 do
                    let doc = new string(Utility.genRandoms 256 0 2 |> Seq.map ((+) 97 >> char) |> Array.ofSeq)
                    Assert.AreEqual(searcher.LookupFrom doc |> List.ofSeq, testSearcher.LookupFrom doc |> List.ofSeq |> List.sort)

    [<Test>]
    member this.TestWithVarLenRandomString() =
        for _ in 0..3 do
            let words = 
                seq { 
                    for len in 1..4 do
                        for _ in 1..int (System.Math.Pow(3.0, float len)) do 
                            yield new string(Utility.genRandoms len 0 2 |> Seq.map ((+) 97 >> char) |> Array.ofSeq) 
                }
                |> Seq.distinct
                |> Array.ofSeq

            let searcher = newSearcher SearcherType.BruteForce words
            for t in unbox (System.Enum.GetValues(typeof<SearcherType>)) |> Array.filter (fun v->v<>SearcherType.BruteForce && v<>SearcherType.RK) do
                let testSearcher = newSearcher t words

                for _ in 0..10 do
                    let doc = new string(Utility.genRandoms 256 0 2 |> Seq.map ((+) 97 >> char) |> Array.ofSeq)
                    Assert.AreEqual(searcher.LookupFrom doc |> List.ofSeq, testSearcher.LookupFrom doc |> List.ofSeq |> List.sort)


     member this.BenchWithRandomString() =
        let wordsList = 
            [ 
                for _ in 1..3 -> 
                    [| for _ in 0..256 -> new string(Utility.genRandoms 8 0 25 |> Seq.map ((+) 97 >> char) |> Array.ofSeq) |]
            ]
        let docs = [ for _ in 0..10 -> new string(Utility.genRandoms 1000 0 25 |> Seq.map ((+) 97 >> char) |> Array.ofSeq) ]
        
        for t in System.Enum.GetValues(typeof<SearcherType>) do
            let searchers = [for words in wordsList -> newSearcher (unbox<SearcherType> t) words]
            Utility.timeit ("Bench MultiStringSearch: " + t.ToString()) 3 
                (fun ()-> 
                        for searcher in searchers do
                            for doc in docs do 
                                searcher.LookupFrom doc |> Seq.length |> ignore)