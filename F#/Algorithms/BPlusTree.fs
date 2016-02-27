module BPlusTree

open System.Collections.Generic
open NUnit.Framework

type MutableList<'a> = System.Collections.Generic.List<'a>

let private kPageSize = 4 * 1024

type private ResourceGuard<'a>(getRes : unit -> 'a, cleanup : unit -> unit) =
    member val Resource = getRes() with get
    interface System.IDisposable with
        member this.Dispose() =
            cleanup()

type INode<'k, 'v when 'k : comparison> =
    abstract PageIndex : int
    abstract MaximumKey : 'k
    abstract Count : int
    abstract Contains : 'k -> bool
    abstract Get : 'k -> 'v option
    abstract Add : 'k * 'v -> unit
    abstract IsFull : bool
    abstract Split : unit -> INode<'k, 'v> * INode<'k, 'v>
    abstract Delete : 'k -> bool
    abstract IsUnderHalfFull : bool
    abstract IsHalfFull : bool
    abstract Rearrange : INode<'k, 'v> * int -> unit
    abstract TryCollapse : unit -> INode<'k, 'v>
    abstract NodeStore : obj with get, set

type IOCounter = 
    { 
        mutable ReadCount : int; 
        mutable WriteCount : int; 
        mutable DeleteCount : int 
    }
    static member New() = { ReadCount = 0; WriteCount = 0; DeleteCount = 0 }

type INodeStore<'k, 'v when 'k : comparison> =
    abstract NewPageIndex : unit -> int
    abstract Read : int -> INode<'k, 'v>
    abstract Write : INode<'k, 'v> -> unit
    abstract Delete : INode<'k, 'v> -> unit

type PersistentNodeStore<'k, 'v when 'k : comparison>(counter : IOCounter) =
    let mutable index2Bytes = Map.empty
    let mutable nextPageIndex = 0

    interface INodeStore<'k, 'v> with
        member this.NewPageIndex() : int = 
            nextPageIndex <- nextPageIndex + 1
            nextPageIndex

        member this.Read(pageIndex : int) : INode<'k, 'v> = 
            counter.ReadCount <- counter.ReadCount + 1
            let store, bytes = index2Bytes.[pageIndex]
            let node : INode<'k, 'v> = Utility.deserialize bytes
            node.NodeStore <- store
            node

        member this.Write(node : INode<'k, 'v>) : unit = 
            counter.WriteCount <- counter.WriteCount + 1
            index2Bytes <- index2Bytes.Remove(node.PageIndex).Add(node.PageIndex, (node.NodeStore, Utility.serialize node))

        member this.Delete(node : INode<'k, 'v>) : unit = 
            counter.DeleteCount <- counter.DeleteCount + 1
            index2Bytes <- index2Bytes.Remove(node.PageIndex)

type CacheNodeStore<'k, 'v when 'k : comparison>(store : INodeStore<'k, 'v>, cacheSize : int, cacheWay : int) = 
    let buckets = Array.init (cacheSize / cacheWay) (fun _-> new MutableList<INode<'k, 'v> * bool>())

    interface INodeStore<'k, 'v> with
        member this.NewPageIndex() : int = store.NewPageIndex()

        member this.Read(pageIndex : int) : INode<'k, 'v> = 
            let slot = pageIndex % buckets.Length
            let list = buckets.[slot]
            let index = list.FindIndex(fun (n, i) -> n.PageIndex = pageIndex)
            if index = -1 then
                let n = store.Read(pageIndex)
                if list.Count = cacheWay then
                    match list.[list.Count - 1] with
                    | (n, true) -> store.Write(n)
                    | _ -> ()
                    list.RemoveAt(list.Count - 1)
                list.Insert(0, (n, false))
                n
            else
                let (n, _) as entry = list.[index]
                list.RemoveAt(index)
                list.Insert(0, entry)
                n

        member this.Write(node : INode<'k, 'v>) : unit = 
            let slot = node.PageIndex % buckets.Length
            let list = buckets.[slot]
            let index = list.FindIndex(fun (n, i) -> n = node)
            if index = -1 then
                store.Write(node)
            else
                list.[index] <- (node, true)

        member this.Delete(node : INode<'k, 'v>) : unit = 
            let slot = node.PageIndex % buckets.Length
            let list = buckets.[slot]
            list.RemoveAll(fun (n, _) -> n = node) |> ignore
            store.Delete(node)

let private lowerBound<'k when 'k : comparison> (list : IReadOnlyList<'k>) (key : 'k) =
    if list.Count = 0 then
        0
    else
        let mutable lo, hi = 0, list.Count-1
        if list.[lo] >= key then
            0
        elif list.[hi] < key then
            list.Count
        else
            while lo + 1 < hi do
                let mid = lo + (hi - lo) / 2
                if list.[mid] < key then
                    lo <- mid
                else
                    hi <- mid
            hi
 
let inline private smallerEven (v : int) = if v % 2 = 0 then v else v - 1
      
type private InternalNode<'k, 'v when 'k : comparison>(store : INodeStore<'k, 'v>) =
    [<System.NonSerialized>]
    let mutable store = store
    let pageIndex = store.NewPageIndex()
    let keys = new MutableList<'k>() :> IReadOnlyList<_>
    let childrenIndexs = new MutableList<int>() :> IReadOnlyList<_>

    static member val FullCount = kPageSize / (sizeof<'k> + sizeof<int>) |> smallerEven with get, set

    static member New(store : INodeStore<'k, 'v>, a : INode<'k, 'v>, b : INode<'k, 'v>) = 
        let node = InternalNode<'k, 'v>(store)
        use guard : ResourceGuard<_> = node.AcquireWriter()
        let (keys : MutableList<_>), (childrenIndexs : MutableList<_>) = guard.Resource
        keys.Add(a.MaximumKey); keys.Add(b.MaximumKey)
        childrenIndexs.Add(a.PageIndex); childrenIndexs.Add(b.PageIndex)
        node

    member private this.Keys = keys
    member public this.ChildrenIndexs = childrenIndexs

    member private this.AcquireWriter() = 
        new ResourceGuard<_>(
            (fun () -> (keys :?> MutableList<'k>, childrenIndexs :?> MutableList<int>)), 
            (fun () -> store.Write(this)))

    member private this.SyncChildKey(pos : int) =
        let child = store.Read(childrenIndexs.[pos])
        if keys.[pos] <> child.MaximumKey then
            use guard = this.AcquireWriter()
            let keys, _ = guard.Resource
            keys.[pos] <- child.MaximumKey
    
    interface INode<'k, 'v> with
        member this.PageIndex = pageIndex
        member this.MaximumKey = keys.[keys.Count - 1]
        member this.Count = keys.Count
        member this.Contains(key : 'k) = (this :> INode<'k, 'v>).Get(key).IsSome

        member this.Get(key : 'k) : 'v option = 
            let pos = lowerBound keys key
            if pos = keys.Count then
                None
            else
                store.Read(childrenIndexs.[pos]).Get(key)
            
        member this.Add(key : 'k, value : 'v) = 
            let pos = lowerBound keys key
            let pos = min pos (keys.Count-1)
            let child = store.Read(childrenIndexs.[pos])
            child.Add(key, value)
            if child.IsFull then
                let a, b = child.Split()
                use guard = this.AcquireWriter()
                let keys, childrenIndexs = guard.Resource
                keys.[pos] <- b.MaximumKey
                childrenIndexs.[pos] <- b.PageIndex
                keys.Insert(pos, a.MaximumKey)
                childrenIndexs.Insert(pos, a.PageIndex)
            else
                this.SyncChildKey(pos)
            
        member this.IsFull = keys.Count = InternalNode<'k, 'v>.FullCount

        member this.Split() = 
            let this = this :> INode<'k, 'v>
            let sibling = InternalNode<'k, 'v>(store) :> INode<'k, 'v>
            this.Rearrange(sibling, keys.Count - keys.Count / 2)
            this, sibling

        member this.Delete(key : 'k) = 
            let pos = lowerBound keys key
            if pos = keys.Count then
                false
            else
                let child = store.Read(childrenIndexs.[pos])
                if child.Delete(key) then
                    if child.IsUnderHalfFull then
                        let first, shouldMerge = 
                            if pos = keys.Count-1 then
                                keys.Count-2, store.Read(childrenIndexs.[keys.Count-2]).IsHalfFull
                            else
                                pos, store.Read(childrenIndexs.[pos + 1]).IsHalfFull
                        let child0, child1 = store.Read(childrenIndexs.[first]), store.Read(childrenIndexs.[first + 1])
                        if shouldMerge then
                            use guard = this.AcquireWriter()
                            let keys, childrenIndexs = guard.Resource    
                            child0.Rearrange(child1, child0.Count + child1.Count)
                            store.Delete(child1)
                            keys.[first] <- child0.MaximumKey
                            keys.RemoveAt(first + 1) |> ignore
                            childrenIndexs.RemoveAt(first + 1) |> ignore
                        else
                            let totalCount = child0.Count + child1.Count
                            child0.Rearrange(child1, totalCount - totalCount / 2)
                            this.SyncChildKey(first)
                            this.SyncChildKey(first + 1)
                    else
                        this.SyncChildKey(pos)
                    true
                else
                    false

        member this.IsHalfFull = keys.Count = InternalNode<'k, 'v>.FullCount / 2
        member this.IsUnderHalfFull = keys.Count = InternalNode<'k, 'v>.FullCount / 2 - 1

        member this.Rearrange(node : INode<'k, 'v>, reserveCount : int) : unit = 
            let move2Left = reserveCount - keys.Count
            if move2Left <> 0 then
                use guardL = this.AcquireWriter()
                use guardR = (node :?> InternalNode<'k, 'v>).AcquireWriter()
                let keysL, childrenIndexsL = guardL.Resource
                let keysR, childrenIndexsR = guardR.Resource
                if move2Left > 0 then
                    keysL.InsertRange(keysL.Count, seq { for i in 0..move2Left-1 -> keysR.[i] })
                    childrenIndexsL.InsertRange(childrenIndexsL.Count, seq { for i in 0..move2Left-1 -> childrenIndexsR.[i] })
                    keysR.RemoveRange(0, move2Left)
                    childrenIndexsR.RemoveRange(0, move2Left)
                else
                    keysR.InsertRange(0, seq { for i in keysL.Count+move2Left..keysL.Count-1 -> keysL.[i] })
                    childrenIndexsR.InsertRange(0, seq { for i in childrenIndexsL.Count+move2Left..childrenIndexsL.Count-1 -> childrenIndexsL.[i] })
                    keysL.RemoveRange(keysL.Count+move2Left, -move2Left)
                    childrenIndexsL.RemoveRange(childrenIndexsL.Count+move2Left, -move2Left)

        member this.TryCollapse() : INode<'k, 'v> =
            if keys.Count = 1 then
                let child = store.Read(childrenIndexs.[0])
                store.Delete(this)
                child
            else
                this :> INode<'k, 'v>

        member this.NodeStore 
            with get() = store :> obj
            and set v = store <- v :?> INodeStore<'k, 'v>

type private LeafNode<'k, 'v when 'k : comparison>(store : INodeStore<'k, 'v>) = 
    [<System.NonSerialized>]
    let mutable store = store
    let pageIndex = store.NewPageIndex()
    let keys = new MutableList<'k>() :> IReadOnlyList<_>
    let values = new MutableList<'v>() :> IReadOnlyList<_>

    static member val FullCount = kPageSize / (sizeof<'k> + sizeof<'v>) |> smallerEven with get, set

    member private this.Keys = keys
    member private this.Values = values

    member private this.AcquireWriter() = 
        new ResourceGuard<_>(
            (fun () -> (keys :?> MutableList<'k>, values :?> MutableList<'v>)), 
            (fun () -> store.Write(this)))

    interface INode<'k, 'v> with
        member this.PageIndex = pageIndex
        member this.MaximumKey = keys.[keys.Count - 1]
        member this.Count = keys.Count
        member this.Contains(key : 'k) = (this :> INode<'k, 'v>).Get(key).IsSome

        member this.Get(key : 'k) = 
            let pos = lowerBound keys key
            if pos < keys.Count && keys.[pos] = key then
                Some values.[pos]
            else
                None

        member this.Add(key : 'k, value : 'v) = 
            let pos = lowerBound keys key
            assert (pos = keys.Count || keys.[pos] <> key)
            use guard = this.AcquireWriter()
            let keys, values = guard.Resource
            keys.Insert(pos, key)
            values.Insert(pos, value)

        member this.IsFull = keys.Count = LeafNode<'k, 'v>.FullCount

        member this.Split() = 
            let this = this :> INode<'k, 'v>
            let sibling = LeafNode<'k, 'v>(store) :> INode<'k, 'v>
            this.Rearrange(sibling, keys.Count - keys.Count / 2)
            this, sibling

        member this.Delete(key : 'k) = 
            let pos = lowerBound keys key
            if pos < keys.Count && keys.[pos] = key then
                use guard = this.AcquireWriter()
                let keys, values = guard.Resource
                keys.RemoveAt(pos)
                values.RemoveAt(pos)
                true
            else
                false

        member this.IsHalfFull = keys.Count = LeafNode<'k, 'v>.FullCount / 2
        member this.IsUnderHalfFull = keys.Count = LeafNode<'k, 'v>.FullCount / 2 - 1

        member this.Rearrange(node : INode<'k, 'v>, reserveCount : int) : unit = 
            let move2Left = reserveCount - keys.Count
            if move2Left <> 0 then
                use guardL = this.AcquireWriter()
                use guardR = (node :?> LeafNode<'k, 'v>).AcquireWriter()
                let keysL, childrenIndexsL = guardL.Resource
                let keysR, childrenIndexsR = guardR.Resource
                if move2Left > 0 then
                    keysL.InsertRange(keysL.Count, seq { for i in 0..move2Left-1 -> keysR.[i] })
                    childrenIndexsL.InsertRange(childrenIndexsL.Count, seq { for i in 0..move2Left-1 -> childrenIndexsR.[i] })
                    keysR.RemoveRange(0, move2Left)
                    childrenIndexsR.RemoveRange(0, move2Left)
                else
                    keysR.InsertRange(0, seq { for i in keysL.Count+move2Left..keysL.Count-1 -> keysL.[i] })
                    childrenIndexsR.InsertRange(0, seq { for i in childrenIndexsL.Count+move2Left..childrenIndexsL.Count-1 -> childrenIndexsL.[i] })
                    keysL.RemoveRange(keysL.Count+move2Left, -move2Left)
                    childrenIndexsL.RemoveRange(childrenIndexsL.Count+move2Left, -move2Left)

        member this.TryCollapse() = this :> INode<'k, 'v>

        member this.NodeStore 
            with get() = store :> obj
            and set v = store <- v :?> INodeStore<'k, 'v>

type Tree<'k, 'v when 'k : comparison>(store : INodeStore<'k, 'v>) = 
    let mutable rootIndex = 
        let node = LeafNode<'k, 'v>(store)
        store.Write(node)
        (node :> INode<'k, 'v>).PageIndex

    let mutable count = 0

    member this.Count = count

    member this.Contains(key : 'k) : bool =
        store.Read(rootIndex).Contains(key)

    member this.Get(key : 'k) : 'v option =
        store.Read(rootIndex).Get(key)

    member this.RangeGet(keyStart : 'k, keyEnd : 'k) : seq<'v> =
        failwith "not implmented"

    member this.Add(key : 'k, value : 'v) : unit =
        let mutable node = store.Read(rootIndex)
        node.Add(key, value)
        if node.IsFull then
            let a, b = node.Split()
            node <- InternalNode<'k, 'v>.New(store, a, b)
        rootIndex <- node.PageIndex
        count <- count + 1

    member this.Delete(key : 'k) : bool =
        let mutable node = store.Read(rootIndex)
        let succ = node.Delete(key)
        node <- node.TryCollapse()
        rootIndex <- node.PageIndex
        if succ then
            count <- count - 1
        succ

    member this.Print() : unit = 
        let rec printLayer (indexs : seq<int>) = 
            let nextIndexs = new MutableList<int>()
            for index in indexs do
                if index = -1 then
                    printf "-"
                else
                    let child = store.Read(index)
                    printf "%d " (child.Count)
                    if child :? InternalNode<'k, 'v> then
                        let child = child :?> InternalNode<'k, 'v>
                        nextIndexs.AddRange(child.ChildrenIndexs)
                        nextIndexs.Add(-1)
            printfn ""
            if nextIndexs.Count > 0 then
                printLayer nextIndexs

        printfn "--- Tree ---"
        printLayer [rootIndex]

[<TestFixture>]
type internal BPlusTreeTest() =
    
    [<Test>]
    member this.TestSequentialAdd() =
        InternalNode<int, int>.FullCount <- 4
        LeafNode<int, int>.FullCount <- 4

        let tree = Tree(PersistentNodeStore(IOCounter.New()))
        for i in 40..-2..1 do
            tree.Add(i, i * i)
            // tree.Print()

        for i in 40..-1..1 do
            if i % 2 = 0 then
                Assert.AreEqual(i * i, tree.Get(i).Value)
            else
                Assert.IsFalse(tree.Contains(i))

    [<Test>]
    member this.TestRandomAdd() =
        InternalNode<int, int>.FullCount <- 4
        LeafNode<int, int>.FullCount <- 4

        let tree = Tree(PersistentNodeStore(IOCounter.New()))

        for i in Utility.randomShuffle [|40..-2..1|] do
            tree.Add(i, i * i)
            // tree.Print()

        for i in 40..-1..1 do
            if i % 2 = 0 then
                Assert.AreEqual(i * i, tree.Get(i).Value)
            else
                Assert.IsFalse(tree.Contains(i))


    [<Test>]
    member this.TestDelete() =
        InternalNode<int, int>.FullCount <- 4
        LeafNode<int, int>.FullCount <- 4

        let tree = Tree(PersistentNodeStore(IOCounter.New()))

        for i in Utility.randomShuffle [|40..-2..1|] do
            tree.Add(i, i * i)

        for i in 40..-1..1 do
            if i % 2 = 0 then
                Assert.AreEqual(i * i, tree.Get(i).Value)
                Assert.IsTrue(tree.Delete(i))
                Assert.IsFalse(tree.Contains(i))
                // tree.Print()
            else
                Assert.IsFalse(tree.Contains(i))

        Assert.AreEqual(0, tree.Count)


    [<Test>]
    member this.TestCache() =
        InternalNode<int, int>.FullCount <- 4
        LeafNode<int, int>.FullCount <- 4

        let adds = Utility.randomShuffle [|0..1000|]
        let gets = Utility.genRandoms 10000 0 1000 |> List.ofSeq
        let dels = Utility.randomShuffle [|0..1000|]

        do
            let stopwatch = System.Diagnostics.Stopwatch.StartNew()

            let counter = IOCounter.New()
            let tree = Tree(PersistentNodeStore(counter))
            for i in adds do
                tree.Add(i, i * i)
            for i in gets do
                tree.Get(i) |> ignore
            for i in dels do
                tree.Delete(i) |> ignore

            // printfn "The counter without cache:\nElapse=%f\n%A" stopwatch.Elapsed.TotalSeconds counter

        do 
            let stopwatch = System.Diagnostics.Stopwatch.StartNew()

            let counter = IOCounter.New()
            let tree = Tree(CacheNodeStore(PersistentNodeStore(counter), 256, 4))
            for i in adds do
                tree.Add(i, i * i)
            for i in gets do
                tree.Get(i) |> ignore
            for i in dels do
                tree.Delete(i) |> ignore

            // printfn "The counter with cache:\nElapse=%f\n%A" stopwatch.Elapsed.TotalSeconds counter

        do 
            let stopwatch = System.Diagnostics.Stopwatch.StartNew()

            let counter = IOCounter.New()
            let tree = Tree(CacheNodeStore(PersistentNodeStore(counter), 256, 4))
            for i in adds |> Seq.sort do
                tree.Add(i, i * i)
            for i in gets |> Seq.sort do
                tree.Get(i) |> ignore
            for i in dels |> Seq.sort do
                tree.Delete(i) |> ignore

            // printfn "The counter with cache (Sequential access):\nElapse=%f\n%A" stopwatch.Elapsed.TotalSeconds counter


    [<Test>]
    member this.TestWithLargeData() =
        InternalNode<int, int>.FullCount <- 4
        LeafNode<int, int>.FullCount <- 4

        let dict = new Dictionary<int, int>()
        let tree = Tree(CacheNodeStore(PersistentNodeStore(IOCounter.New()), 1024, 4))

        let adds = Utility.genRandoms 10000 0 3000 |> List.ofSeq
        let gets = Utility.genRandoms 30000 0 3000 |> List.ofSeq
        let dels = Utility.genRandoms 20000 0 3000 |> List.ofSeq

        for i in adds do
            if dict.ContainsKey(i) then
                Assert.IsTrue(tree.Contains(i))
            else
                dict.Add(i, i * i)
                tree.Add(i, i * i)
        for i in gets do
            Assert.AreEqual(dict.ContainsKey(i), tree.Contains(i))
        for i in dels do
            Assert.AreEqual(dict.Remove(i), tree.Delete(i))
        for i in gets do
            Assert.AreEqual(dict.ContainsKey(i), tree.Contains(i))
        for i in 0..3000 do
            Assert.AreEqual(dict.Remove(i), tree.Delete(i))
        Assert.AreEqual(0, tree.Count)