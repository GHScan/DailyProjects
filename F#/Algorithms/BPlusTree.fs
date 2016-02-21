module BPlusTree

open System.Collections.Generic
open NUnit.Framework

type MutableList<'a> = System.Collections.Generic.List<'a>

let mutable private kPageSize = 4 * 1024
let private kCacheWay = 4
let private kCacheWriteBufferSize = 8

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
    abstract Balance : INode<'k, 'v> -> unit
    abstract Merge : INode<'k, 'v> -> unit
    abstract TryCollapse : unit -> INode<'k, 'v>

type IOCounter() =
    member val ReadCount = 0 with get, set
    member val WriteCount = 0 with get, set
    member val DeleteCount = 0 with get, set

type NodeStore<'k, 'v when 'k : comparison>(counter : IOCounter) =
    let mutable index2Node = Map.empty
    let mutable nextIndex = 0
    
    member this.Read(index : int) : INode<'k, 'v> = 
        counter.ReadCount <- counter.ReadCount + 1
        index2Node.[index]

    member this.Write(node : INode<'k, 'v>) =
        counter.WriteCount <- counter.WriteCount + 1
        index2Node <- index2Node.Remove(node.PageIndex).Add(node.PageIndex, node)

    member this.Delete(node : INode<'k, 'v>) = 
        counter.DeleteCount <- counter.DeleteCount + 1
        index2Node <- index2Node.Remove(node.PageIndex)

    member this.NewPageIndex() : int = 
        nextIndex <- nextIndex + 1
        nextIndex

type NodeCache<'k, 'v when 'k : comparison>(store : NodeStore<'k, 'v>, cacheSize : int) = 
    let buckets = Array.create (cacheSize / kCacheWay) List.empty
    let mutable nodes2Write = new MutableList<_>()
    
    member this.Read(index : int) : INode<'k, 'v> =
        let slot = index % buckets.Length
        let list = buckets.[slot]
        match List.tryFind (fun n -> (n :> INode<'k, 'v>).PageIndex = index) list with
        | Some n -> 
            buckets.[slot] <- n :: List.filter ((<>) n) list
            n
        | None -> 
            let n = store.Read(index)
            buckets.[slot] <- n :: list |> Seq.take kCacheWay |> List.ofSeq
            n

    member this.Write(node : INode<'k, 'v>) = 
        nodes2Write.Remove(node) |> ignore
        nodes2Write.Add(node)
        if nodes2Write.Count = kCacheWriteBufferSize then 
            for n in nodes2Write do
                store.Write(node)
            nodes2Write.Clear()

    member this.Flush() =
        for n in nodes2Write do
            store.Write(n)
        nodes2Write.Clear()

    member this.Delete(node : INode<'k, 'v>) = store.Delete(node)
    member this.NewPageIndex() = store.NewPageIndex()

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
      
type private InternalNode<'k, 'v when 'k : comparison>(cache : NodeCache<'k, 'v>) =
    let pageIndex = cache.NewPageIndex()
    let keys = new MutableList<'k>() :> IReadOnlyList<_>
    let childrenIndexs = new MutableList<int>() :> IReadOnlyList<_>

    static member val FullCount = kPageSize / (sizeof<'k> + sizeof<int>) |> smallerEven

    static member New(cache : NodeCache<'k, 'v>, a : INode<'k, 'v>, b : INode<'k, 'v>) = 
        let node = InternalNode<'k, 'v>(cache)
        use guard : ResourceGuard<_> = node.RequireWriter()
        let (keys : MutableList<_>), (childrenIndexs : MutableList<_>) = guard.Resource
        keys.Add(a.MaximumKey); keys.Add(b.MaximumKey)
        childrenIndexs.Add(a.PageIndex); childrenIndexs.Add(b.PageIndex)
        node

    member private this.Keys = keys
    member private this.ChildrenIndexs = childrenIndexs

    member private this.RequireWriter() = 
        new ResourceGuard<_>(
            (fun () -> (keys :?> MutableList<'k>, childrenIndexs :?> MutableList<int>)), 
            (fun () -> cache.Write(this)))

    member private this.SyncChildKey(pos : int) =
        let child = cache.Read(childrenIndexs.[pos])
        if keys.[pos] <> child.MaximumKey then
            use guard = this.RequireWriter()
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
                cache.Read(childrenIndexs.[pos]).Get(key)
            
        member this.Add(key : 'k, value : 'v) = 
            let pos = lowerBound keys key
            let pos = min pos (keys.Count-1)
            let child = cache.Read(childrenIndexs.[pos])
            child.Add(key, value)
            if child.IsFull then
                let a, b = child.Split()
                use guard = this.RequireWriter()
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
            let sibling = InternalNode<'k, 'v>(cache) :> INode<'k, 'v>
            this.Balance(sibling)
            this, sibling

        member this.Delete(key : 'k) = 
            let pos = lowerBound keys key
            if pos = keys.Count then
                false
            else
                let child = cache.Read(childrenIndexs.[pos])
                let succ = child.Delete(key)
                if child.IsUnderHalfFull then
                    let first, shouldMerge = 
                        if pos = keys.Count-1 then
                            keys.Count-2, cache.Read(childrenIndexs.[keys.Count-2]).IsHalfFull
                        else
                            pos, cache.Read(childrenIndexs.[pos + 1]).IsHalfFull
                    if shouldMerge then
                        use guard = this.RequireWriter()
                        let keys, childrenIndexs = guard.Resource
                        let child = cache.Read(childrenIndexs.[first]) 
                        child.Merge(cache.Read(childrenIndexs.[first + 1]))
                        keys.[first] <- child.MaximumKey
                        keys.RemoveAt(first + 1) |> ignore
                        childrenIndexs.RemoveAt(first + 1) |> ignore
                    else
                        cache.Read(childrenIndexs.[first]).Balance(cache.Read(childrenIndexs.[first + 1]))
                        this.SyncChildKey(first)
                        this.SyncChildKey(first + 1)
                else
                    this.SyncChildKey(pos)
                succ

        member this.IsHalfFull = keys.Count = InternalNode<'k, 'v>.FullCount / 2
        member this.IsUnderHalfFull = keys.Count = InternalNode<'k, 'v>.FullCount / 2 - 1

        member this.Balance(o : INode<'k, 'v>) = 
            let move2Left = (o.Count - keys.Count) / 2
            if move2Left <> 0 then
                use guardL = this.RequireWriter()
                use guardR = (o :?> InternalNode<'k, 'v>).RequireWriter()
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

        member this.Merge(o : INode<'k, 'v>) = 
            let o = o :?> InternalNode<'k, 'v>
            use guard = this.RequireWriter()
            let keys, childrenIndexs = guard.Resource
            keys.InsertRange(keys.Count, o.Keys)
            childrenIndexs.InsertRange(childrenIndexs.Count, o.ChildrenIndexs)
            cache.Delete(o)

        member this.TryCollapse() : INode<'k, 'v> =
            if keys.Count = 1 then
                let child = cache.Read(childrenIndexs.[0])
                cache.Delete(this)
                child
            else
                this :> INode<'k, 'v>

type private LeafNode<'k, 'v when 'k : comparison>(cache : NodeCache<'k, 'v>) = 
    let pageIndex = cache.NewPageIndex()
    let keys = new MutableList<'k>() :> IReadOnlyList<_>
    let values = new MutableList<'v>() :> IReadOnlyList<_>

    static member val FullCount = kPageSize / (sizeof<'k> + sizeof<'v>) |> smallerEven

    member private this.Keys = keys
    member private this.Values = values

    member private this.RequireWriter() = 
        new ResourceGuard<_>(
            (fun () -> (keys :?> MutableList<'k>, values :?> MutableList<'v>)), 
            (fun () -> cache.Write(this)))

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
            use guard = this.RequireWriter()
            let keys, values = guard.Resource
            keys.Insert(pos, key)
            values.Insert(pos, value)

        member this.IsFull = keys.Count = InternalNode<'k, 'v>.FullCount

        member this.Split() = 
            let this = this :> INode<'k, 'v>
            let sibling = LeafNode<'k, 'v>(cache) :> INode<'k, 'v>
            this.Balance(sibling)
            this, sibling

        member this.Delete(key : 'k) = 
            let pos = lowerBound keys key
            if pos < keys.Count && keys.[pos] = key then
                use guard = this.RequireWriter()
                let keys, values = guard.Resource
                keys.RemoveAt(pos)
                values.RemoveAt(pos)
                true
            else
                false

        member this.IsHalfFull = keys.Count = InternalNode<'k, 'v>.FullCount / 2
        member this.IsUnderHalfFull = keys.Count = InternalNode<'k, 'v>.FullCount / 2 - 1

        member this.Balance(o : INode<'k, 'v>) = 
            let move2Left = (o.Count - keys.Count) / 2
            if move2Left <> 0 then
                use guardL = this.RequireWriter()
                use guardR = (o :?> LeafNode<'k, 'v>).RequireWriter()
                let keysL, valuesL = guardL.Resource
                let keysR, valuesR = guardR.Resource
                if move2Left > 0 then
                    keysL.InsertRange(keysL.Count, seq { for i in 0..move2Left-1 -> keysR.[i] })
                    valuesL.InsertRange(valuesL.Count, seq { for i in 0..move2Left-1 -> valuesR.[i] })
                    keysR.RemoveRange(0, move2Left)
                    valuesR.RemoveRange(0, move2Left)
                else
                    keysR.InsertRange(0, seq { for i in keysL.Count+move2Left..keysL.Count-1 -> keysL.[i] })
                    valuesR.InsertRange(0, seq { for i in valuesL.Count+move2Left..valuesL.Count-1 -> valuesL.[i] })
                    keysL.RemoveRange(keysL.Count+move2Left, -move2Left)
                    valuesL.RemoveRange(valuesL.Count+move2Left, -move2Left)

        member this.Merge(o : INode<'k, 'v>) = 
            let o = o :?> LeafNode<'k, 'v>
            use guard = this.RequireWriter()
            let keys, values = guard.Resource
            keys.InsertRange(keys.Count, o.Keys)
            values.InsertRange(values.Count, o.Values)
            cache.Delete(o)

        member this.TryCollapse() = this :> INode<'k, 'v>

type Tree<'k, 'v when 'k : comparison>(cache : NodeCache<'k, 'v>) = 
    let mutable rootIndex = 
        let node = LeafNode<'k, 'v>(cache)
        cache.Write(node)
        (node :> INode<'k, 'v>).PageIndex

    let mutable count = 0

    member this.Count = count

    member this.Contains(key : 'k) : bool =
        cache.Read(rootIndex).Contains(key)

    member this.Get(key : 'k) : 'v option =
        cache.Read(rootIndex).Get(key)

    member this.RangeGet(keyStart : 'k, keyEnd : 'k) : seq<'v> =
        failwith "not implmented"

    member this.Add(key : 'k, value : 'v) : unit =
        let mutable node = cache.Read(rootIndex)
        node.Add(key, value)
        if node.IsFull then
            let a, b = node.Split()
            node <- InternalNode<'k, 'v>.New(cache, a, b)
        rootIndex <- node.PageIndex
        count <- count + 1

    member this.Delete(key : 'k) : bool =
        let mutable node = cache.Read(rootIndex)
        let succ = node.Delete(key)
        node <- node.TryCollapse()
        rootIndex <- node.PageIndex
        if succ then
            count <- count - 1
        succ

[<TestFixture>]
type BPlusTreeTest() =
    
    [<Test>]
    member this.TestWithFixedData() =
        ()