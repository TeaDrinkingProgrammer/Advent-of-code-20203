// Use FSharpPlus to extend the standard library
#r @"nuget: FSharpPlus"

open System

open FSharpPlus

// Because I am too lazy to care about types in printfunctions in Advent Of Code
let printany any =
    printf "%A \n" any