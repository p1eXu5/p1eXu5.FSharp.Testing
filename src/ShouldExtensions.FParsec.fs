namespace p1eXu5.FSharp.Testing.ShouldExtensions

open System.Diagnostics
open NUnit.Framework
open FsUnit
open FParsec

[<AutoOpen>]
module FParsec =

    [<DebuggerStepThrough>]
    let shouldSuccessEqual res = function
    | Success (s, _, _) -> s |> FsUnit.shouldL equal res ""
    | Failure (t, _, _) -> raise (AssertionException($"Should be %A{res} but was %A{t}"))


    // [<DebuggerStepThrough>]
    // let shouldSuccess ``constraint`` res = function
    // | Success (s, _, _) -> s |> should ``constraint`` res
    // | Failure (t, _, _) -> raise (AssertionException($"Should be %A{res} but was %A{t}"))

    [<DebuggerStepThrough>]
    let shouldSuccess = function
    | Success (s, _, _) -> Assert.Pass()
    | Failure (t, _, _) -> raise (AssertionException($"Result is not Success. ${t}"))

    [<DebuggerStepThrough>]
    let runResult p input =
        runParserOnString p () "test" input
        |> function
            | Success (ok,_,_) -> Result.Ok ok
            | Failure (err,_,_) -> Result.Error err