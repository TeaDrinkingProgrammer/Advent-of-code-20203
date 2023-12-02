#r @"nuget: FSharpPlus"

open System

open FSharpPlus

let input = 
    IO.File.ReadAllLines("day2_input1.txt")

type Cube =
    | Red of amount: int
    | Blue of amount: int
    | Green of amount: int
type Game =
 {
    id: int
    sets: list<list<Cube>>
 }

let printany any =
    printf "%A \n" any

let cubes_of_set set =
    set |>
        String.split [|","|] |>
        Seq.map (fun cube ->
            let number::color = cube |> String.trimWhiteSpaces |> String.split [|" "|] |> List.ofSeq
            let color = color.Head
            match color with
            | "red" -> Red(int number)
            | "blue" -> Blue(int number)
            | "green" -> Green(int number)
            | x -> failwithf "Not a valid color: %A" x
        ) |> List.ofSeq
let to_encoded input =
    (String.split [|":"|] input) |> List.ofSeq |> (fun (game: list<string>) -> 
        let id: string list = game.Head |> String.split [|" "|] |> List.ofSeq
        let id = int id.Tail.Head
        let sets = 
            game.Tail.Head |> String.split [|";"|] |>
            List.ofSeq |> 
            List.map cubes_of_set |> List.ofSeq
        {
            id = id;
            sets = sets;
        }
    )

// Part 1
let encoded_input = Array.map to_encoded input

let fold_bools acc curr =
    match acc with
        | true -> curr
        | false -> acc
let get_answer_part_1 (input: seq<Game>) =
    input |> Seq.map (fun game -> 
    let {id = id; sets = sets} = game
    let sets_are_possible = 
        sets |> List.map (fun set ->
            set |> List.map (fun cube ->
                match cube with
                    | Red(x) -> x <= 12
                    | Green(x) -> x <= 13
                    | Blue(x) -> x <= 14
            ) |> List.fold fold_bools true
        ) |>
        List.fold fold_bools true
    if sets_are_possible then
        id
    else 0
    )
encoded_input |> get_answer_part_1 |> Seq.sum |> printany

    
// Part 2

let get_answer_part_2 (input: seq<Game>) =
    input |> Seq.map (fun game -> 
    let {id = id; sets = sets} = game
    let amount_of_each_color = 
        sets |> List.concat |> List.fold (fun acc curr ->
            let (red,green,blue) = acc
            match curr with
                | Red(x) -> if x > red then (x, green, blue) else acc
                | Green(x) -> if x > green then (red, x, blue) else acc
                | Blue(x) -> if x > blue then (red, green, x) else acc
        ) (0,0,0)
    let (red,green,blue) = amount_of_each_color
    red * green * blue
    ) |> Seq.sum 
encoded_input |> get_answer_part_2 |> printany