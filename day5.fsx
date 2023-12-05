#r @"nuget: FSharpPlus"

open System

open FSharpPlus

let input = 
    IO.File.ReadAllLines("day5_input1.txt")

let printany any =
    printf "%A \n" any

let intersection (list1: int list) (list2: int list) =
    List.filter (fun x -> List.contains x list2) list1

let rec last = function
    | hd :: [] -> hd
    | hd :: tl -> last tl
    | _ -> failwith "Empty list."

type Game =
 {
    seeds: seq<int>
    maps: seq<string * seq<(list<int> * list<int>)>>
 }

// let returnseeds input =

let list_string_split seperators =
    String.split seperators >> List.ofSeq
let process_numbers =
    String.trimWhiteSpaces >> list_string_split [" "] >>  List.map int
let to_encoded(input: seq<string>) =
    let input = List.ofSeq input
    let seeds = input.Head |> list_string_split [":"] |> (List.tail >> List.head) |> process_numbers
    let maps = input.Tail |> List.split [[""]] |> Seq.tail |>  Seq.map (fun x ->
        let id = x.Head
        let ranges = x.Tail |> Seq.map (fun x ->
            let numbers = process_numbers x
            let starta::startb::range::[] = numbers
            ([starta..starta+range], [startb..startb+range])
        )
        (id, ranges)
    )
    {seeds = seeds; maps = maps}

let map_to (ids: list<int>) (map: list<(list<int> * list<int>)>) =
    let range_in_range rangea rangeb =
        List.filter (fun x -> List.contains x rangeb) rangea
    
    Seq.fold (fun acc (curr: list<int> * list<int>) -> 
        let (rangea, rangeb) = curr
        if (range_in_range ids rangea).Length > 0 then
            rangeb::acc
        else
            acc
    ) List.Empty map |> List.concat

input |> to_encoded |> 
    (fun game -> 
        let seeds = game.seeds
        let map_list = game.maps

        let result = map_list |> List.fold (fun acc curr -> 
                let (name, ls_from, ls_to) = curr
                ((map_to seeds (ls_from, ls_to))::acc)
            ) []
        result
    )

    (List.empty)