module Utility

open System
open System.Reflection
open NUnit.Framework


let timeit name times f =
    if times > 1 then f()
    
    let gcCounts = [for i in 0..GC.MaxGeneration -> GC.CollectionCount(i)]

    let stopwatch = System.Diagnostics.Stopwatch.StartNew()
    for _ in 1..times do f()
    stopwatch.Stop()

    let deltaCounts = gcCounts |> List.mapi (fun i c -> GC.CollectionCount(i) - c)
    printfn "%s : Latency=%fs GC=%A" name (stopwatch.Elapsed.TotalSeconds / float times) deltaCounts


let runUnitTests() =
    let types = (Assembly.GetCallingAssembly().GetTypes() 
        |> Array.filter (fun t -> t.GetCustomAttribute<TestFixtureAttribute>() <> null))

    for t in types do
        let methods = (t.GetMethods(BindingFlags.Instance ||| BindingFlags.NonPublic ||| BindingFlags.Public) 
            |> Array.filter (fun m -> m.GetCustomAttribute<TestAttribute>() <> null))

        let obj = Activator.CreateInstance(t)
        printfn "%s" t.Name
        for m in methods do
            try
                m.Invoke(obj, null) |> ignore
            with | :? TargetInvocationException as e -> raise (e.GetBaseException())
                 | e -> raise e
            printfn "\t%s : pass" m.Name