open System
open System.IO
open System.Net
open System.Threading.Tasks

module Utiltiy =
    let timeit name f =
        let stopwatch = System.Diagnostics.Stopwatch.StartNew()
        f()
        printfn "%s : %f" name stopwatch.Elapsed.TotalSeconds

module AsyncWorkflow =
    type AsyncBuilder() = 

        member this.Zero() = Task.FromResult(Unchecked.defaultof<'a>)

        member this.Return(v) = Task.FromResult(v)

        member this.ReturnFrom(v) = v

        member this.Using(v : 'a, k : 'a -> Task<'b>) : Task<'b> when 'a :> IDisposable = 
            (k v).ContinueWith(new Func<Task<'b>, 'b>(fun t -> 
                v.Dispose()
                t.Result))

        member this.Delay(k) = k()

        member this.Bind(t : Task<'a>,  k : 'a -> Task<'b>) : Task<'b> = 
            let source = new TaskCompletionSource<'b>()
            t.ContinueWith(new Action<Task<'a>>(fun t -> 
                (k t.Result).ContinueWith(new Action<Task<'b>>(fun t2 -> 
                    source.SetResult(t2.Result)
                    ())) |> ignore
            )) |> ignore
            source.Task

    let async = new AsyncBuilder()

module Program =
    let sleep (seconds:int) = 
        Task.Delay(seconds).ContinueWith(new Func<Task, unit>(fun t -> ()))

    [<EntryPoint>]
    let main argv = 

        do 
            let t = (
                [|
                    "http://www.baidu.com";
                    "http://www.qq.com";
                    "http://www.taobao.com";
                    "http://www.zhihu.com";
                    "http://www.baidu.com";
                |] 
                |> Array.map (fun (url:string) -> AsyncWorkflow.async {
                    let req = WebRequest.Create(url)
                    use! resp = req.GetResponseAsync()
                    use reader = new StreamReader(resp.GetResponseStream())
                    return! reader.ReadToEndAsync()
                })
                |> Task.WhenAll)

            Utiltiy.timeit "download async" (fun ()-> t.Wait())

            printfn "%A" (t.Result |> Array.map (fun s->s.Length))

        do 
            let t = (
                [|0 .. 10|] 
                |> Array.map (fun _-> AsyncWorkflow.async { 
                    do! sleep 1000
                    do! sleep 1000
                    do! sleep 1000
                }) 
                |> Task.WhenAll)

            Utiltiy.timeit "sleep async" (fun ()-> t.Wait())
    
        0
