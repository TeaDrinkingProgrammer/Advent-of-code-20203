open System
let test_input = [|"1abc2nine";"pqr3seventu8vwx";"a1b2c3d4e5f";"treb7uchet"|]
let input = 
    IO.File.ReadAllLines("day1_input1.txt")
let get_last_int acc x =
    if Char.IsDigit(x) then Some(int (string x)) else acc 

let get_first_int acc x =
    match acc with 
    | Some(x) -> Some(x)
    | None -> get_last_int acc x

let number_tuple_to_number tuple =
    match tuple with
        | (Some(a), Some(b)) -> int ((string a) + (string b))
        | (None, Some(a))
        | (Some(a), None) -> int ((string a) + (string a))
        | (None, None) -> failwith "whatdis"
let row_to_number (row: string) = row |> Seq.toList |> (fun x -> (Seq.fold get_first_int None x, Seq.fold get_last_int None x)) |> number_tuple_to_number

// Part 1
(input |> Array.map row_to_number ) |> Array.sum |> (fun x -> printf "%A " x)

// Part 2
// I might have gotten a few hints of Reddit...
let replace_word_with_int (input: string) =
    input
        .Replace("seven", "se7en")
        .Replace("eight", "ei8ht")
        .Replace("three", "th3ee")
        .Replace("four", "fo4r")
        .Replace("five", "f5ve")
        .Replace("nine", "n9ne")
        .Replace("one", "o1e")
        .Replace("two", "t2o")
        .Replace("six", "s6x")

input |> Array.map (fun x -> x |> replace_word_with_int |> row_to_number) |> Array.sum |> (fun x -> printf "%A " x)