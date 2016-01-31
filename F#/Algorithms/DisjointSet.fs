module DisjointSet

open NUnit.Framework

type DisjointSet(count: int) =
    let mutable count = count
    let parents = [|0..count - 1|]
    let weights = Array.create count 1

    let rec root index = 
        let parent = parents.[index]
        if parent = index then index else root parent

    member this.Count = count

    member this.Union(a, b) = 
        let ra, rb = root a, root b
        if ra <> rb then
            count <- count - 1
            if weights.[ra] >= weights.[rb] then
                weights.[ra] <- weights.[ra] + weights.[rb]
                parents.[rb] <- ra
            else
                weights.[rb] <- weights.[rb] + weights.[ra]
                parents.[ra] <- rb

    member this.IsSame(a, b) = root a = root b

[<TestFixture>]
type DisjointSetTest() =

    [<Test>]
    member this.TestUnion() =
        let set = new DisjointSet(10)
        Assert.AreEqual(10, set.Count)
        for i in 0..2..9 do
            set.Union(i, i + 1)
        Assert.AreEqual(5, set.Count)
        set.Union(0, 2)
        set.Union(0, 4)
        set.Union(0, 6)
        set.Union(8, 9)
        Assert.AreEqual(2, set.Count)
        set.Union(0, 1)
        set.Union(3, 4)
        set.Union(8, 9)
        Assert.AreEqual(2, set.Count)
        set.Union(0, 9)
        Assert.AreEqual(1, set.Count)

    [<Test>]
    member this.TestIsSame() =
        let set = new DisjointSet(5)
        for i in 1..4 do
            Assert.IsFalse(set.IsSame(0, i))
        set.Union(0, 1)
        set.Union(0, 2)
        set.Union(3, 4)
        for i in 1..2 do
            Assert.IsTrue(set.IsSame(0, i))
        Assert.IsTrue(set.IsSame(3, 4))
        Assert.IsFalse(set.IsSame(0, 4))
        set.Union(2, 4)
        Assert.IsTrue(set.IsSame(0, 4))