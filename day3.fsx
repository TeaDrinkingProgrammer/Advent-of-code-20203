#r @"nuget: FSharpPlus"
#r @"nuget: FSharpPlus.TypeLevel"

open System

open FSharpPlus
open FSharpPlus.Data
open System.Text.RegularExpressions

let printany any =
    printf "%A \n \n" any

let raw_input = 
    IO.File.ReadAllLines("day3_input1.txt")

let input = raw_input |> Array.map (Array.ofSeq) |> array2D

let check_symbol =
    let rx = Regex("[^\w.]", RegexOptions.Compiled)
    fun (symbol: char) -> (
            let rx_match = rx.IsMatch (string symbol)
            if rx_match then printany symbol
            rx_match
        )

let foldi (folder: int -> int -> 'S -> 'T -> 'S) (state: 'S) (array: 'T[,]) =
    let mutable state = state
    for x in 0 .. Array2D.length1 array - 1 do
        for y in 0 .. Array2D.length2 array - 1 do
            state <- folder x y state (array.[x, y])
    state

let check_for_symbols (input: char[,]) =
    foldi (fun x y (acc: list<int * int>) (value: char) -> 
        if check_symbol value then (x, y) :: acc else acc
    ) [] input
let string_is_empty = Option.toObj >> String.IsNullOrEmpty
let is_digit_or_sign = 
    let rx = Regex("[0-9+-]", RegexOptions.Compiled)
    fun (symbol: char) -> (
            rx.IsMatch (string symbol)
        )
let get_number_chords (input: char[,]) =
    foldi (fun x y (acc: ((string * (int * int)) * list<(int * int) * (int * int)>)) (value: char) -> 
        let ((number, start_chord), list) = acc
        let new_number = 
            if start_chord = (0,0) then
                match value with
                    | value when is_digit_or_sign value -> (string value, (x,y))
                    | _ -> ("", (0,0))
            else
                match value with
                    | value when not (is_digit_or_sign value) -> (string number, (x-1,y-1))
                    | _ -> ("", (0,0))
        (new_number, list)
    ) (0, []) input

input |> check_for_symbols |> List.rev |> List.iter printany