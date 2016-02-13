module SkipList

open NUnit.Framework

type private Node<'k, 'v when 'k : comparison>(key : 'k, value : 'v, height : int) = 
    let links : Node<'k, 'v> option [] = Array.create height None

    member this.Links = links
    member this.Key = key
    member val Value = value with get, set

type SkipList<'k, 'v when 'k : comparison>(height : int) = 
    let mutable count = 0
    let head = new Node<_, _>(Unchecked.defaultof<'k>, Unchecked.defaultof<'v>, height)
    let random = new System.Random()
    
    let rec forward (prev : Node<'k, 'v>) (index : int) (key : 'k) : Node<'k, 'v> = 
        match prev.Links.[index] with
        | Some next when next.Key < key -> forward next index key
        | _ -> prev
    
    let rec insert (prev : Node<'k, 'v>) (index : int) (node : Node<'k, 'v>) (nodeHeight : int) : bool = 
        let prev = forward prev index (node.Key)
        let succ = 
            if index = 0 then 
                match prev.Links.[index] with
                | Some next when next.Key = node.Key -> false
                | _ -> true
            else 
                insert prev (index - 1) node nodeHeight
        if succ && index < nodeHeight then 
            node.Links.[index] <- prev.Links.[index]
            prev.Links.[index] <- Some node
        succ
    
    let rec remove (prev : Node<'k, 'v>) (index : int) (key : 'k) : bool = 
        let prev = forward prev index key
        let succ = 
            if index = 0 then 
                match prev.Links.[index] with
                | Some next when next.Key = key -> true
                | _ -> false
            else 
                remove prev (index - 1) key
        if succ then 
            match prev.Links.[index] with
            | Some next when next.Key = key -> prev.Links.[index] <- next.Links.[index]
            | _ -> ()
        succ
    
    let rec get (prev : Node<'k, 'v>) (index : int) (key : 'k) : Node<'k, 'v> option = 
        let prev = forward prev index key
        if index = 0 then 
            match prev.Links.[index] with
            | Some next when next.Key = key -> Some next
            | _ -> None
        else 
            get prev (index - 1) key
    
    let newNodeHeight (maxHeight : int) (random : System.Random) = 
        let rec loop height randomNum = 
            if height < maxHeight && randomNum % 2 = 0 
                then loop (height + 1) (randomNum / 2)
            else 
                height
        loop 1 (random.Next())
    
    let getNodeCount (prev : Node<'k, 'v>) (index : int) = 
        let rec loop (prev : Node<'k, 'v>) (acc : int) = 
            match prev.Links.[index] with
            | Some next -> loop next (acc + 1)
            | _ -> acc
        loop prev 0
    
    member this.Count = count
    
    member this.Add(key, value) : bool = 
        let nodeHeight = newNodeHeight height random
        let node = new Node<_, _>(key, value, nodeHeight)
        let succ = insert head (height - 1) node nodeHeight
        if succ then count <- count + 1
        succ
    
    member this.Remove(key) = 
        let succ = remove head (height - 1) key
        if succ then count <- count - 1
        succ
    
    member this.Contains(key) = get head (height - 1) key |> Option.isSome
    member this.Get(key) = (get head (height - 1) key |> Option.get).Value
    member this.Set(key, value) = (get head (height - 1) key |> Option.get).Value <- value

    override this.ToString() = 
        sprintf "SkipList(%A)" ([ for i in 1..height -> getNodeCount head (i - 1) ])

[<TestFixture>]
type SkipListTest() = 
    
    [<Test>]
    member this.TestSimpleCase() = 
        let sl = new SkipList<_, _>(4)
        for i in 0..2..9 do
            sl.Add(i, i.ToString()) |> ignore
        Assert.AreEqual(5, sl.Count)
        for i in 0..9 do
            Assert.AreEqual(i % 2 = 0, sl.Contains(i))
        for i in 0..2..9 do
            Assert.AreEqual(i.ToString(), sl.Get(i))
        for i in 1..2..9 do
            sl.Remove(i) |> ignore
            Assert.AreEqual(5, sl.Count)
        for i in 0..4 do
            sl.Remove(0 + i * 2) |> ignore
            Assert.AreEqual(4 - i, sl.Count)
    
    [<Test>]
    member this.TestRandom() = 
        let kHeight = 10
        let kCount = 1 <<< kHeight
        let sl = new SkipList<_, _>(kHeight)
        let dict = new System.Collections.Generic.Dictionary<int, string>()
        let random = new System.Random()
        for i in 0..kCount do
            let num = random.Next(0, kCount)
            if not (dict.ContainsKey(num)) then dict.Add(num, num.ToString())
            if not (sl.Contains(num)) then sl.Add(num, num.ToString()) |> ignore
            Assert.AreEqual(dict.Count, sl.Count)
        for i in 0..kCount do
            let num = random.Next(0, kCount)
            if dict.ContainsKey(num) then dict.Remove(num) |> ignore
            if sl.Contains(num) then sl.Remove(num) |> ignore
            Assert.AreEqual(dict.Count, sl.Count)
        for kv in dict do
            sl.Remove(kv.Key) |> ignore
        Assert.AreEqual(0, sl.Count)
