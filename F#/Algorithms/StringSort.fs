module StringSort

open System.Collections
open System.Collections.Generic
open System.Threading
open System.Diagnostics

open NUnit.Framework

type private ListSegment<'a>(list : IList<'a>, offset : int, count : int) =
    new (list : IList<'a>) = ListSegment<'a>(list, 0, list.Count)

    member this.Length = 
        count

    member this.GetSlice(first : int option, last : int option) =
        let first, last = defaultArg first 0, defaultArg last (count - 1)
        if first <= last && first >= 0 && last < count then
            ListSegment(list, offset + first, last - first + 1)
        else
            ListSegment(list, offset, 0)

    member this.Item 
        with get index = 
            assert (index < count)
            list.[offset + index]
        and set index value = 
            assert (index < count)
            list.[offset + index] <- value

    member this.Swap(i, j) =
        let a, b = this.[i], this.[j]
        this.[i] <- b
        this.[j] <- a
         
    member this.CopyTo(o : ListSegment<'a>) =
        assert (this.Length = o.Length)
        for i in 0..this.Length-1 do
            o.[i] <- this.[i]

    interface IEnumerable<'a> with
        member this.GetEnumerator() = (seq {for i in offset..offset+count-1 -> list.[i]}).GetEnumerator()
        member this.GetEnumerator() = (seq {for i in offset..offset+count-1 -> list.[i]}).GetEnumerator() :> IEnumerator

let private kMaxChar = 255
let private kDigitCount = kMaxChar + 2

let inline private digit (s : string) (i : int) = 
    if i >= s.Length then 0
    elif int s.[i] > kMaxChar then invalidArg "s" "found invalid char"
    else int s.[i] + 1

let inline private swapPair (a,b) = 
    (b,a)

let private insertionSort (list : ListSegment<_>) =
    for i in 1..list.Length-1 do 
        let v = list.[i]
        let mutable j = i - 1
        while j >= 0 && list.[j] > v do
            list.[j + 1] <- list.[j]
            j <- j - 1
        list.[j + 1] <- v

let private radixSortBaseOnDigit (inputs : ListSegment<string>) (outputs : ListSegment<string>) index =
    let counts = Array.create kDigitCount 0
    for s in inputs do 
        let c = digit s index
        counts.[c] <- counts.[c] + 1

    let offsets = Array.scan (+) 0 counts

    for s in inputs do
        let c = digit s index
        let offset = offsets.[c]
        outputs.[offset] <- s
        offsets.[c] <- offset + 1

    counts

let private sCachedTempList = new ThreadLocal<List<string>>(fun () -> new List<string>())

let builtinSort (strs : IList<string>) = 
    match strs with
    | :? List<string> as l -> l.Sort()
    | :? (string[]) as a -> Array.sortInPlace(a)
    | :? IList as l -> ArrayList.Adapter(l).Sort({ new IComparer with member this.Compare(a, b) = (a :?> string).CompareTo(b :?> string) })
    | _ -> invalidArg "strs" "invalid type"

let simpleQuickSort (strs : IList<string>) =
    let rec sort (list : ListSegment<string>) =
        if list.Length <= 8 then 
            insertionSort list
        else 
            let v = list.[0]
            let mutable p = 0
            for i in 1..list.Length-1 do 
                if list.[i] <= v then
                    p <- p + 1
                    list.Swap(p, i)
            list.Swap(0, p)

            sort (list.[..p - 1])
            sort (list.[p + 1..])

    sort (ListSegment(strs))

let radixSortLSDFixedLen (strs : IList<string>) =
    let tempList = sCachedTempList.Value
    tempList.AddRange(strs)

    if strs.Count = 0 then ()
    else
        let len = strs.[0].Length
        Debug.Assert(not (strs |> Seq.exists (fun v -> v.Length <> len)))
        
        let mutable listPair = (ListSegment(strs), ListSegment(tempList))
        for index in len-1..-1..0 do
            radixSortBaseOnDigit (fst listPair) (snd listPair) index |> ignore
            listPair <- swapPair listPair

        if len % 2 = 1 then
            (fst listPair).CopyTo(snd listPair)

    tempList.Clear()

let radixSortMSD (strs : IList<string>) = 
    let rec sort (list : ListSegment<string>) (tempList : ListSegment<string>) (index : int) =
        if list.Length <= 8 || index >= 16 then
            insertionSort list
        else
            let counts = radixSortBaseOnDigit list tempList index |> Array.filter (fun c -> c <> 0)
            let mutable offset = 0
            for c in counts do
                let slist, stempList = list.[offset..offset+c-1], tempList.[offset..offset+c-1]
                sort stempList slist (index + 1)
                stempList.CopyTo(slist)
                offset <- offset + c

    let tempList = sCachedTempList.Value
    tempList.AddRange(strs)
    sort (ListSegment(strs)) (ListSegment(tempList)) 0
    tempList.Clear()

let threeWayQuickSortMSD (strs : IList<string>) =
    let rec sort (list : ListSegment<string>) (index : int) = 
        if list.Length <= 8 || index >= 16 then
             insertionSort list
        else
            let mutable lo, hi, i = 0, list.Length, 1
            let c = digit list.[0] index
            while i < hi do
                let nc = digit list.[i] index
                if nc < c then
                    lo <- lo + 1
                    list.Swap(lo, i)
                    i <- i + 1
                elif nc > c then
                    hi <- hi - 1
                    list.Swap(hi, i)
                else
                    i <- i + 1
            list.Swap(0, lo)

            sort list.[..lo-1] index
            sort list.[lo..hi-1] (index + 1)
            sort list.[hi..] index

    sort (ListSegment(strs)) 0

[<TestFixture>]
type internal StringSortTest() =

    [<Test>]
    member this.TestSort() =
        let sortFuncs : list<IList<string> -> unit> = 
            [
                builtinSort
                fun l -> insertionSort (ListSegment(l))
                simpleQuickSort
                radixSortMSD
                threeWayQuickSortMSD
            ]

        let fullArray = [|3;1;2;0;5;4;6;9;8;7;|] |> Array.map (fun i -> i.ToString())
        for len in 0..fullArray.Length do
            let subArray = fullArray.[0..len-1]
            let orderedList = subArray |> List.ofArray |> List.sort
            for sort in sortFuncs do
                let testArray = subArray.[0..]
                sort testArray
                Assert.AreEqual(orderedList, testArray |> List.ofArray)

        for len in 0..10 do
            for _ in 0..3 do
                let randomArray = [| for v in (Utility.genRandoms len 0 10) -> v.ToString() |]
                let orderedList = randomArray |> List.ofArray |> List.sort
                for sort in sortFuncs do
                    let testArray = randomArray.[0..]
                    sort testArray
                    Assert.AreEqual(orderedList, testArray |> List.ofArray)

    [<Test>]
    member this.TestSortWithFixedLenString() =
        let sortFuncs : list<IList<string> -> unit> = 
            [
                radixSortLSDFixedLen
                radixSortMSD
                threeWayQuickSortMSD
            ]

        for strLen in 1..8 do    
            let fullArray = 
                Utility.genRandoms 16 0 9 
                |> Seq.windowed strLen 
                |> Seq.map (fun digits -> new string(digits |> Array.map (fun c -> char (c + int '0'))))
                |> Array.ofSeq
            for len in 0..fullArray.Length do
                let subArray = fullArray.[0..len-1]
                let orderedList = subArray |> List.ofArray |> List.sort
                for sort in sortFuncs do
                    let testArray = subArray.[0..]
                    sort testArray
                    Assert.AreEqual(orderedList, testArray |> List.ofArray)
        
    // [<Test>]
    member this.BenchSortFixedLengString() =
        let sortFuncs : list<string * (IList<string> -> unit)> = 
            [
                "builtinSort", builtinSort
                "simpleQuickSort", simpleQuickSort
                "radixSortLSDFixedLen", radixSortLSDFixedLen
            ]
       
        for (strLen, count) in [3,100000;5,1000000;8,1000000] do
            let strs = 
                Utility.genRandoms count 0 9 
                |> Seq.windowed strLen 
                |> Seq.map (fun digits -> new string(digits |> Array.map (fun c -> char (c + int '0'))))
                |> Array.ofSeq

            printfn "Bench SortFixedLenString: Len=%d,Count=%d" strLen count
            for (name, sort) in sortFuncs do
                let testStrs = strs.[0..]
                Utility.timeit ("\t" + name) 1 (fun ()-> sort testStrs)
    