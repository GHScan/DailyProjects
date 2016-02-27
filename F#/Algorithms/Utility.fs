module Utility

open System
open System.Reflection
open System.Collections.Generic
open System.Runtime.Serialization.Formatters.Binary
open System.IO
open NUnit.Framework

let timeit name times f = 
    if times > 1 then f() |> ignore

    let gcCounts = [ for i in 0..GC.MaxGeneration -> GC.CollectionCount(i) ]
    
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()
    for _ in 1..times do f() |> ignore
    stopwatch.Stop()

    let deltaCounts = gcCounts |> List.mapi (fun i c -> GC.CollectionCount(i) - c)
    printfn "%s : Latency=%fs GC=%A" name (stopwatch.Elapsed.TotalSeconds / float times) deltaCounts

let runUnitTests () = 
    let getMethods (t : Type) (attrType : Type) = 
        t.GetMethods(BindingFlags.Instance ||| BindingFlags.NonPublic ||| BindingFlags.Public)
        |> Array.filter (fun m -> m.GetCustomAttribute(attrType) <> null)

    let types = 
        Assembly.GetCallingAssembly().GetTypes() 
        |> Array.filter (fun t -> t.GetCustomAttribute<TestFixtureAttribute>() <> null)

    for t in types do
        let obj = Activator.CreateInstance(t)
        printfn "%s" t.Name
        for m in getMethods t typeof<TestAttribute> do
            try 
                for setup in getMethods t typeof<SetUpAttribute> do
                    setup.Invoke(obj, null) |> ignore

                m.Invoke(obj, null) |> ignore

                for tearDown in getMethods t typeof<TearDownAttribute> do
                    tearDown.Invoke(obj, null) |> ignore
            with | :? TargetInvocationException as e -> raise (e.GetBaseException())
                 | e -> raise e
            printfn "\t%s : pass" m.Name

let runBenchmarks () = 
    let types = 
        (Assembly.GetCallingAssembly().GetTypes() 
         |> Array.filter (fun t -> t.GetCustomAttribute<TestFixtureAttribute>() <> null))

    for t in types do
        let methods = 
            (t.GetMethods(BindingFlags.Instance ||| BindingFlags.NonPublic ||| BindingFlags.Public) 
             |> Array.filter (fun m -> m.Name.StartsWith("Bench")))

        let obj = Activator.CreateInstance(t)
        for m in methods do
            m.Invoke(obj, null) |> ignore

let private sRandom = new System.Random()
let genRandoms count minVal maxVal =
    seq { for i in 1..count -> sRandom.Next(minVal, maxVal) }

let randomShuffle (array : IList<'a>) = 
    for i in 1..array.Count do
        let pos = sRandom.Next(0, i-1)
        let a, b = array.[i-1], array.[pos]
        array.[i-1] <- b
        array.[pos] <- a
    array

let lowerBound<'a, 'b> (list : IList<'a>) (v : 'b) (compare : 'a -> 'b -> int) : int =
    let mutable lo, hi = 0, list.Count - 1
    if list.Count = 0 then
        0
    elif compare list.[lo] v >= 0 then
        0
    elif compare list.[hi] v < 0 then
        list.Count
    else
        while lo + 1 < hi do
            let mid = (hi - lo) / 2 + lo
            if compare list.[mid] v < 0 then
                lo <- mid
            else
                hi <- mid
        hi

let serialize (v : 'a) : byte[] = 
    let formatter = new BinaryFormatter()
    use ms = new MemoryStream()
    formatter.Serialize(ms, v)
    ms.ToArray()

let deserialize(bytes : byte[]) : 'a =
    let formatter = new BinaryFormatter()
    use ms = new MemoryStream(bytes)
    formatter.Deserialize(ms) :?> 'a