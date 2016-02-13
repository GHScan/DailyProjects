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

// Trie

type private TrieNode<'k, 'v>(alphabetSize : int) =
    let children : TrieNode<'k, 'v> option[] = Array.create (alphabetSize) None

    member val Value : 'v option = None with get, set

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
            match this.Value with
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


// TernarySearchTree

type private TSTNode<'k, 'v> = 
    {
        Character : int
        mutable Value : 'v option
        mutable Left : TSTNode<'k, 'v> option
        mutable Mid : TSTNode<'k, 'v> option
        mutable Right : TSTNode<'k, 'v> option
    }

    static member New(c) = { Character = c; Value = None; Left = None; Mid = None; Right = None; }

    static member Get(n : TSTNode<'k, 'v> option, alphabet : IAlphabet<'k>, key : 'k, index : int) : TSTNode<'k, 'v> option = 
        match (alphabet.GetCharacter(key, index), n) with
        | (None, _) -> failwith "invalid key"
        | (_, None) -> None
        | (Some c, Some n) when c < n.Character -> TSTNode.Get(n.Left, alphabet, key, index)
        | (Some c, Some n) when c > n.Character -> TSTNode.Get(n.Right, alphabet, key, index)
        | (Some c, Some n) when Option.isSome (alphabet.GetCharacter(key, index + 1)) -> TSTNode.Get(n.Mid, alphabet, key, index + 1)
        | (Some c, Some n) -> Some n

    static member GetOrCreate(n : TSTNode<'k, 'v> option, alphabet : IAlphabet<'k>, key : 'k, index : int) : TSTNode<'k, 'v> * TSTNode<'k, 'v> =
        let c = 
            match alphabet.GetCharacter(key, index) with
            | None -> failwith "invalid key"
            | Some c -> c
        let n = 
            match n with
            | None -> TSTNode<_, _>.New(c)
            | Some n -> n
        if c < n.Character then
            let newLeft, ret = TSTNode.GetOrCreate(n.Left, alphabet, key, index)
            n.Left <- Some newLeft
            n, ret
        elif c > n.Character then
            let newRight, ret = TSTNode.GetOrCreate(n.Right, alphabet, key, index)
            n.Right <- Some newRight
            n, ret
        elif Option.isSome (alphabet.GetCharacter(key, index + 1)) then
            let newMid, ret = TSTNode.GetOrCreate(n.Mid, alphabet, key, index + 1)
            n.Mid <- Some newMid
            n, ret
        else
            n, n

    static member Match(n : TSTNode<'k, 'v> option, alphabet : IAlphabet<'k>, pattern : 'k, index : int, key : 'k) : seq<'k * TSTNode<'k, 'v>> = 
        match (alphabet.GetCharacter(pattern, index), n) with
        | (None, _) -> Seq.empty
        | (_, None) -> Seq.empty
        | (Some c, Some n) when alphabet.IsWildcard c ->
            seq {
                yield! TSTNode.Match(n.Left, alphabet, pattern, index, key)
                yield! TSTNode.Match(n.Right, alphabet, pattern, index, key)

                let nextKey = alphabet.AppendCharacter(key, n.Character)
                if Option.isSome (alphabet.GetCharacter(pattern, index + 1)) then
                    yield! TSTNode.Match(n.Mid, alphabet, pattern, index + 1, nextKey)
                else
                    yield nextKey, n
            }
        | (Some c, Some n) when c < n.Character -> TSTNode.Match(n.Left, alphabet, pattern, index, key)
        | (Some c, Some n) when c > n.Character -> TSTNode.Match(n.Right, alphabet, pattern, index, key)
        | (Some c, Some n) -> 
            seq {
                let nextKey = alphabet.AppendCharacter(key, n.Character)
                if Option.isSome (alphabet.GetCharacter(pattern, index + 1)) then
                    yield! TSTNode.Match(n.Mid, alphabet, pattern, index + 1, nextKey)
                else
                    yield nextKey, n
            }

    static member Traverse(n : TSTNode<'k, 'v> option, alphabet : IAlphabet<'k>, key : 'k) : seq<'k * 'v> = 
        match n with
            | None -> Seq.empty
            | Some n -> 
                seq {
                    yield! TSTNode.Traverse(n.Left, alphabet, key)
                    yield! TSTNode.Traverse(n.Right, alphabet, key)

                    let nextKey = alphabet.AppendCharacter(key, n.Character)
                    match n.Value with
                    | None -> ()
                    | Some v -> yield (nextKey, v)
                    yield! TSTNode.Traverse(n.Mid, alphabet, nextKey)
                }

    static member Descendents(n : TSTNode<'k, 'v> option, alphabet : IAlphabet<'k>, key : 'k) : seq<'k * 'v> = 
        match n with 
        | None -> Seq.empty
        | Some n -> 
            seq {
                match n.Value with
                | None -> ()
                | Some v -> yield (key, v)
                yield! TSTNode.Traverse(n.Mid, alphabet, key)
            }

type TSTMap<'k, 'v>(alphabet : IAlphabet<'k>) =
    let mutable root : TSTNode<'k, 'v> option = None

    interface IMap<'k, 'v> with

        member this.Item
            with get (key : 'k) = 
                TSTNode.Get(root, alphabet, key, 0) |> Option.bind (fun n -> n.Value) |> Option.get
            and set (key : 'k) (value : 'v) = 
                let newRoot, ret = TSTNode.GetOrCreate(root, alphabet, key, 0)
                root <- Some newRoot
                ret.Value <- Some value

        member this.Remove(key : 'k) : bool =
            match TSTNode.Get(root, alphabet, key, 0) with
            | Some n when Option.isSome n.Value -> 
                n.Value <- None
                true
            | _ -> false

        member this.Contains(key : 'k) : bool = 
            TSTNode.Get(root, alphabet, key, 0) |> Option.bind (fun n -> n.Value) |> Option.isSome

        member this.Items : seq<'k * 'v> =
            TSTNode.Traverse(root, alphabet, alphabet.Zero)

        member this.MatchPrefix(key : 'k) : seq<'k * 'v> =
            TSTNode.Descendents(TSTNode.Get(root, alphabet, key, 0), alphabet, key)

        member this.MatchPattern(pattern : 'k) : seq<'k * 'v> =
            TSTNode.Match(root, alphabet, pattern, 0, alphabet.Zero) 
            |> Seq.filter (fun (k, n) -> Option.isSome n.Value)
            |> Seq.map (fun (k, n) -> (k, n.Value.Value))

        member this.GetEnumerator() = 
            (this :> IMap<'k, 'v>).Items.GetEnumerator()

        member this.GetEnumerator() = 
            (this :> IMap<'k, 'v>).Items.GetEnumerator() :> IEnumerator


let newStringTST<'v>() = TSTMap<string, 'v>(ANSIAlphabet()) :> IMap<string, 'v>

// Unit test

[<TestFixture>]
type StringMapTest() =

    [<Test>]
    member this.TestWithFixedStrings() =

        for map in [newStringTrie();newStringTST()] do

            let strs = [ "a"; "ab"; "abc"; "ac"; "acd"; "bc" ]
            for s in strs do
                map.[s] <- s.Length
            for s in strs do
                Assert.IsTrue(map.Contains(s))
                Assert.AreEqual(map.[s], s.Length)
            Assert.IsFalse(map.Contains("b"))
            Assert.IsFalse(map.Contains("abcd"))
            Assert.IsFalse(map.Contains("ad"))
            for (k, v) in map.Items do
                Assert.AreEqual(v, k.Length)
            for (k, v) in map do
                Assert.AreEqual(v, k.Length)

            Assert.AreEqual(["a";"ab";"abc";"ac";"acd"] |> List.sort, map.MatchPrefix("a") |> List.ofSeq |> List.map fst |> List.sort)
            Assert.AreEqual(["ab";"abc"] |> List.sort, map.MatchPrefix("ab") |> List.ofSeq |> List.map fst |> List.sort)
            Assert.AreEqual(["abc"] |> List.sort, map.MatchPrefix("abc") |> List.ofSeq |> List.map fst |> List.sort)
            Assert.AreEqual([] |> List.sort, map.MatchPrefix("abcd") |> List.ofSeq |> List.map fst |> List.sort)

            Assert.AreEqual(["ac";"bc"] |> List.sort, map.MatchPattern(".c") |> List.ofSeq |> List.map fst |> List.sort)
            Assert.AreEqual(["abc";"acd"] |> List.sort, map.MatchPattern("a..") |> List.ofSeq |> List.map fst |> List.sort)

            for i in 0..2..strs.Length-1 do
                Assert.IsTrue(map.Remove(strs.[i]))
            for i in 0..strs.Length-1 do
                Assert.AreEqual(i % 2 = 1, map.Contains(strs.[i]))

    [<Test>]
    member this.TestRandomStrings() =

        for map in [newStringTrie();newStringTST()] do

            let mutable builtinMap = Map.empty

            let strs = Utility.genRandoms 1000 0 1000 |> Seq.map (fun v -> v.ToString()) |> Array.ofSeq 
            for i in 0..strs.Length-1 do
                map.[strs.[i]] <- i
                builtinMap <- builtinMap.Add(strs.[i], i)
            for i in 0..2..strs.Length-1 do
                if builtinMap.ContainsKey(strs.[i]) then 
                    Assert.IsTrue(map.Remove(strs.[i]))
                    builtinMap <- builtinMap.Remove(strs.[i])
            for i in 0..strs.Length-1 do
                Assert.AreEqual(builtinMap.ContainsKey(strs.[i]), map.Contains(strs.[i]))
                if builtinMap.ContainsKey(strs.[i]) then
                    Assert.AreEqual(builtinMap.[strs.[i]], map.[strs.[i]])