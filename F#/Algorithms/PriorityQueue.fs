module PriorityQueue

open System
open System.Collections.Generic
open NUnit.Framework

type internal KBasedArray<'a>(k : int, length : int, initValue : 'a) = 
    let array = Array.create length initValue

    member this.Length = length
    
    member this.Item 
        with get index = array.[index - k]
        and set index value = array.[index - k] <- value
    
    member this.Swap(i, j) = 
        let a, b = this.[i], this.[j]
        this.[i] <- b
        this.[j] <- a

type internal OneBasedList<'a>() = 
    let list = new List<'a>()

    member this.Length = list.Count
    member this.PushBack(value) = list.Add(value)
    
    member this.PopBack() = 
        let index = list.Count - 1
        let v = list.[index]
        list.RemoveAt(index)
        v
    
    member this.Item 
        with get index = list.[index - 1]
        and set index value = list.[index - 1] <- value
    
    member this.Swap(i, j) = 
        let a, b = this.[i], this.[j]
        this.[i] <- b
        this.[j] <- a

let internal swap (a, b) = (b, a)

let internal sink (list : OneBasedList<'a>) (index : int) (onSwap : 'a -> 'a -> unit) : unit = 
    let rec loop index = 
        if index * 2 <= list.Length then 
            let mutable smallChild = index * 2
            if smallChild + 1 <= list.Length && list.[smallChild + 1] < list.[smallChild] then 
                smallChild <- smallChild + 1
            if list.[index] > list.[smallChild] then 
                onSwap list.[index] list.[smallChild]
                list.Swap(index, smallChild)
                loop smallChild
    loop index

let internal lift (list : OneBasedList<'a>) (index : int) (onSwap : 'a -> 'a -> unit) : unit = 
    let mutable index = index
    while index / 2 >= 1 && list.[index] < list.[index / 2] do
        onSwap list.[index] list.[index / 2]
        list.Swap(index, index / 2)
        index <- index / 2

type PriorityQueue<'a when 'a : comparison>() = 
    let list = new OneBasedList<'a>()

    member this.Length = list.Length
    member this.Empty = this.Length = 0
    
    member this.Push(value) = 
        list.PushBack(value)
        lift list list.Length (fun _ _ -> ())
    
    member this.Pop() = 
        if this.Empty then invalidOp "Could not pop item from an empty queue"
        list.Swap(1, list.Length)
        let value = list.PopBack()
        sink list 1 (fun _ _ -> ())
        value

type IndexablePriorityQueue<'a when 'a : comparison>(minIndex : int, maxIndex : int) = 
    let index2QueueIndex = new KBasedArray<int>(minIndex, maxIndex - minIndex + 1, 0)
    let list = new OneBasedList<'a * int>()

    member this.Length = list.Length
    member this.Empty = this.Length = 0
    
    member this.Push(index : int, value : 'a) : unit = 
        index2QueueIndex.[index] <- list.Length + 1
        list.PushBack(value, index)
        lift list list.Length (fun a b -> index2QueueIndex.Swap(snd a, snd b))
    
    member this.Pop() : int * 'a = this.Remove(snd list.[1])
    member this.Contains(index : int) : bool = index2QueueIndex.[index] <> 0
    member this.Get(index : int) : 'a = fst list.[index2QueueIndex.[index]]

    member this.Remove(index : int) : int * 'a = 
        let queueIndex = index2QueueIndex.[index]
        index2QueueIndex.Swap(index, snd list.[list.Length])
        list.Swap(queueIndex, list.Length)
        let value = list.PopBack()
        index2QueueIndex.[snd value] <- 0
        sink list queueIndex (fun a b -> index2QueueIndex.Swap(snd a, snd b))
        swap value

[<TestFixture>]
type internal PriorityQueueTest() = 
    
    [<Test>]
    member this.TestPop() = 
        let q = new PriorityQueue<_>()
        Assert.IsTrue(q.Empty)
        q.Push(3)
        q.Push(1)
        q.Push(2)
        Assert.AreEqual(3, q.Length)
        Assert.AreEqual(1, q.Pop())
        Assert.AreEqual(2, q.Pop())
        Assert.AreEqual(3, q.Pop())
    
    [<Test>]
    member this.TestSort() = 
        let q = new PriorityQueue<_>()
        let ints = [ 3; 1; 5; 4; 2 ]
        for v in ints do
            q.Push(v)
        Assert.AreEqual(ints |> List.sort, 
                        [ while not q.Empty do
                              yield q.Pop() ])

[<TestFixture>]
type IndexablePriorityQueueTest() = 
    
    [<Test>]
    member this.TestPop() = 
        let q = new IndexablePriorityQueue<_>(0, 2)
        Assert.IsTrue(q.Empty)
        q.Push(0, 3)
        q.Push(2, 1)
        q.Push(1, 2)
        Assert.AreEqual(3, q.Length)
        Assert.AreEqual((2, 1), q.Pop())
        Assert.AreEqual((1, 2), q.Pop())
        Assert.AreEqual((0, 3), q.Pop())
    
    [<Test>]
    member this.TestSort() = 
        let q = new IndexablePriorityQueue<_>(0, 5)
        
        let tuples = 
            [ 3, 5
              5, 2
              4, 0
              0, 1
              1, 3 ]
        for (k, v) in tuples do
            q.Push(k, v)
        Assert.AreEqual(tuples |> List.sortBy snd, 
                        [ while not q.Empty do
                              yield q.Pop() ])
    
    [<Test>]
    member this.TestContains() = 
        let q = new IndexablePriorityQueue<_>(0, 4)
        q.Push(3, 1)
        q.Push(1, 3)
        q.Push(0, 2)
        Assert.IsTrue(q.Contains(0))
        Assert.IsTrue(q.Contains(1))
        Assert.IsFalse(q.Contains(2))
        Assert.IsTrue(q.Contains(3))
        Assert.IsFalse(q.Contains(4))
    
    [<Test>]
    member this.TestGet() = 
        let q = new IndexablePriorityQueue<_>(0, 3)
        q.Push(3, 1)
        q.Push(1, 3)
        q.Push(0, 2)
        Assert.AreEqual(2, q.Get(0))
        Assert.AreEqual(3, q.Get(1))
        Assert.AreEqual(1, q.Get(3))
    
    [<Test>]
    member this.TestRemove() = 
        let q = new IndexablePriorityQueue<_>(0, 9)
        let ints = [ 3; 1; 0; 4; 2; 5; 9; 6; 7; 8 ]
        let dels = [ 3; 8; 7; 5; 4 ]
        let alives = ints |> List.filter (fun x -> not (List.exists (fun y -> x = y) dels))
        for i in ints do
            q.Push(i, i * i)
        for d in dels do
            q.Remove(d) |> ignore
        Assert.AreEqual(alives
                        |> List.map (fun x -> (x, x * x))
                        |> List.sortBy snd, 
                        [ while not q.Empty do
                              yield q.Pop() ])
