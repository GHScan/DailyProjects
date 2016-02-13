module Trie

open System.Collections
open System.Collections.Generic

open NUnit.Framework

type IMap<'k, 'v> =
    inherit IEnumerable<'k * 'v>
    abstract Item : 'k -> 'v with get, set
    abstract Remove : 'k -> bool
    abstract Contains : 'k -> bool
    abstract Items : seq<'k * 'v>
    abstract MatchPrefix : 'k -> seq<'k * 'v>
    abstract MatchPattern : 'k -> seq<'k * 'v>

type IAlphabet<'k> =
    abstract Size : int
    abstract GetCharacter : 'k * int -> int option
    abstract AppendCharacter : 'k * int -> 'k
    abstract Zero : 'k
    abstract IsWildcard : int -> bool

type private ANSIAlphabet() =
    let kSize = 128

    interface IAlphabet<string> with
        member this.Size = 
            kSize

        member this.GetCharacter(key : string, index : int) : int option = 
            if index >= key.Length then 
                None
            else
                let c = int key.[index]
                assert (c < kSize)
                Some c

        member this.AppendCharacter(key : string, c : int) : string = 
            assert (c < kSize)
            key + (char c).ToString()

        member this.Zero = ""

        member this.IsWildcard(c : int) = 
            c = int '.'

type private TrieNode<'k, 'v>(alphabetSize : int) =
    let mutable value : 'v option = None
    let children : TrieNode<'k, 'v> option[] = Array.create (alphabetSize) None

    member this.Value 
        with get () = value
        and set v = value <- v

    member this.Get(alphabet : IAlphabet<'k>, key : 'k, index : int) : TrieNode<'k, 'v> option = 
        match alphabet.GetCharacter(key, index) with
        | None -> Some this
        | Some c -> children.[c] |> Option.bind (fun n -> n.Get(alphabet, key, index + 1))

    member this.GetOrCreate(alphabet : IAlphabet<'k>, key : 'k, index : int) : TrieNode<'k, 'v> =
        match alphabet.GetCharacter(key, index) with
        | None -> this
        | Some c -> 
            if Option.isNone children.[c] then
                children.[c] <- Some (TrieNode<'k, 'v>(alphabet.Size))
            children.[c].Value.GetOrCreate(alphabet, key, index + 1)

    member this.Descendents(alphabet : IAlphabet<'k>, key : 'k) : seq<'k * 'v> = 
        seq {
            match value with
            | None -> ()
            | Some v -> yield key, v

            for c in 0..alphabet.Size-1 do 
                match children.[c] with
                | None -> ()
                | Some n -> yield! n.Descendents(alphabet, alphabet.AppendCharacter(key, c))
        }

    member this.Match(alphabet : IAlphabet<'k>, pattern : 'k, index : int, key : 'k) : seq<'k * TrieNode<'k, 'v>> =
        match alphabet.GetCharacter(pattern, index) with
        | None -> Seq.singleton (key, this)
        | Some c ->
            let cs = if alphabet.IsWildcard c then seq { 0..alphabet.Size-1 } else seq {c..c}
            seq {
                for c in cs do
                    match children.[c] with
                    | None -> ()
                    | Some n -> yield! n.Match(alphabet, pattern, index + 1, alphabet.AppendCharacter(key, c))
            }
            

type TrieMap<'k, 'v>(alphabet : IAlphabet<'k>) =
    let root = TrieNode<'k, 'v>(alphabet.Size)

    interface IMap<'k, 'v> with

        member this.Item
            with get key = 
                root.Get(alphabet, key, 0) |> Option.bind (fun n -> n.Value) |> Option.get
            and set key value = 
                root.GetOrCreate(alphabet, key, 0).Value <- Some value

        member this.Remove(key : 'k) : bool = 
            match root.Get(alphabet, key, 0) with
            | Some n when Option.isSome (n.Value) -> 
                n.Value <- None
                true
            | _ -> false

        member this.Contains(key : 'k) : bool = 
            root.Get(alphabet, key, 0) |> Option.bind (fun n -> n.Value) |> Option.isSome

        member this.Items : seq<'k * 'v> = 
            root.Descendents(alphabet, alphabet.Zero)

        member this.MatchPrefix(key : 'k) : seq<'k * 'v> = 
            match root.Get(alphabet, key, 0) with
            | None -> Seq.empty
            | Some n -> n.Descendents(alphabet, key)

        member this.MatchPattern(pattern : 'k) : seq<'k * 'v> = 
            root.Match(alphabet, pattern, 0, alphabet.Zero) 
            |> Seq.filter (fun (k, n) -> Option.isSome n.Value) 
            |> Seq.map (fun (k, n) -> (k, n.Value.Value))

        member this.GetEnumerator() = 
            (this :> IMap<'k, 'v>).Items.GetEnumerator()

        member this.GetEnumerator() = 
            (this :> IMap<'k, 'v>).Items.GetEnumerator() :> IEnumerator

let newStringTrie<'v>() = TrieMap<string, 'v>(ANSIAlphabet()) :> IMap<string, 'v>


[<TestFixture>]
type TrieMapTest() =

    [<Test>]
    member this.TestWithFixedStrings() =
        let trie = newStringTrie()

        let strs = [ "a"; "ab"; "abc"; "ac"; "acd"; "bc" ]
        for s in strs do
            trie.[s] <- s.Length
        for s in strs do
            Assert.IsTrue(trie.Contains(s))
            Assert.AreEqual(trie.[s], s.Length)
        Assert.IsFalse(trie.Contains("b"))
        Assert.IsFalse(trie.Contains("abcd"))
        Assert.IsFalse(trie.Contains("ad"))
        for (k, v) in trie.Items do
            Assert.AreEqual(v, k.Length)
        for (k, v) in trie do
            Assert.AreEqual(v, k.Length)

        Assert.AreEqual(["a";"ab";"abc";"ac";"acd"] |> List.sort, trie.MatchPrefix("a") |> List.ofSeq |> List.map fst |> List.sort)
        Assert.AreEqual(["ab";"abc"] |> List.sort, trie.MatchPrefix("ab") |> List.ofSeq |> List.map fst |> List.sort)
        Assert.AreEqual(["abc"] |> List.sort, trie.MatchPrefix("abc") |> List.ofSeq |> List.map fst |> List.sort)
        Assert.AreEqual([] |> List.sort, trie.MatchPrefix("abcd") |> List.ofSeq |> List.map fst |> List.sort)

        Assert.AreEqual(["ac";"bc"], trie.MatchPattern(".c") |> List.ofSeq |> List.map fst)
        Assert.AreEqual(["abc";"acd"], trie.MatchPattern("a..") |> List.ofSeq |> List.map fst)

        for i in 0..2..strs.Length-1 do
            Assert.IsTrue(trie.Remove(strs.[i]))
        for i in 0..strs.Length-1 do
            Assert.AreEqual(i % 2 = 1, trie.Contains(strs.[i]))

    [<Test>]
    member this.TestRandomStrings() =
        let trie = newStringTrie()
        let mutable map = Map.empty

        let strs = Utility.genRandoms 1000 0 1000 |> Seq.map (fun v -> v.ToString()) |> Array.ofSeq 
        for i in 0..strs.Length-1 do
            trie.[strs.[i]] <- i
            map <- map.Add(strs.[i], i)
        for i in 0..2..strs.Length-1 do
            if map.ContainsKey(strs.[i]) then 
                Assert.IsTrue(trie.Remove(strs.[i]))
                map <- map.Remove(strs.[i])
        for i in 0..strs.Length-1 do
            Assert.AreEqual(map.ContainsKey(strs.[i]), trie.Contains(strs.[i]))
            if map.ContainsKey(strs.[i]) then
                Assert.AreEqual(map.[strs.[i]], trie.[strs.[i]])