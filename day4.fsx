#r @"nuget: FSharpPlus"

open System

open FSharpPlus

let input = 
    IO.File.ReadAllLines("day4_input1.txt")

let printany any =
    printf "%A \n" any

let intersection (list1: int list) (list2: int list) =
    List.filter (fun x -> List.contains x list2) list1

let rec last = function
    | hd :: [] -> hd
    | hd :: tl -> last tl
    | _ -> failwith "Empty list."

let to_encoded input =
    (String.split [|":"|] input) |> List.ofSeq |> (fun (game: list<string>) ->
        let id: string list = game.Head |> String.split [|" "|] |> List.ofSeq
        let id = int (last id)
        
        let games = 
            game.Tail.Head |> 
            String.split [|"|"|] |> 
            List.ofSeq |> 
            List.map String.trimWhiteSpaces |> 
            List.map (
                String.split [|" "|] >> 
                Seq.filter (String.IsNullOrEmpty >> not) >>
                Seq.map int >>
                List.ofSeq
            ) |> 
            List.ofSeq
        let numbers = games.Head
        let winning_numbers = games.Tail.Head
        (id, numbers, winning_numbers)
    )

// Part 1
let to_part_1 encoded_input =
    let id, numbers, winning_numbers = encoded_input
    let points = intersection numbers winning_numbers |> (fun ls ->
        let length = List.length ls
        let fib n = 
            let rec inner a n =
                match n with
                    | 0 -> 0
                    | 1 -> a
                    | _ -> inner (a+a) (n-1)
            inner 1 n
        fib length
    )
    points

input |> Array.map (to_encoded >> to_part_1) |> Array.sum |> printany 

// Part 2
let to_part_2 (encoded_input: int * list<int> * list<int>) =
        let id, numbers, winning_numbers = encoded_input
        let points = intersection numbers winning_numbers |> List.length
        Map [id, points]

let map_of_array_map = Array.fold (fun acc mp -> Map.union acc mp) Map.empty
let game_list = input |> Array.map (to_encoded >> to_part_2) |> map_of_array_map
let add_x_to_index (mp: Map<int, int>) index value =
    if mp.ContainsKey index then
        let curr_value = mp[index]
        mp.Add (index, (value+curr_value))
    else
        mp.Add (index, (value))

let create_index_map n =
    [1..n] |> List.map (fun index -> index, 1) |> Map.ofList


let id_counts = (Map.fold (fun (acc: Map<int, int>) key value -> 
    let amount_of_cards = acc[key]
    let mutable acc =  acc
    for i in key+1 .. key+value do
         acc <- (add_x_to_index acc i amount_of_cards)
    acc
    ) (create_index_map game_list.Count) game_list)

id_counts |> Map.values |> Seq.sum |> printany