module TimeOff.TestsRunner

open Expecto

[<EntryPoint>]
let main args =
  Tests.runTestsInAssembly { defaultConfig with ``parallel`` = false } args