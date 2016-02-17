module Program

[<EntryPoint>]
let main argv = 

    #if DEBUG
    Utility.runUnitTests()
    #else
    Utility.runBenchmarks()
    #endif
    
    0