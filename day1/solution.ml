let reversed x =
  let len = String.length x in
  String.init len (fun n -> String.get x (len - n - 1)) ;;

(* not sure if these exist elsewhere but they're not hard to write *)
let char_to_int = function
    | '0' -> Some 0
    | '1' -> Some 1
    | '2' -> Some 2
    | '3' -> Some 3
    | '4' -> Some 4
    | '5' -> Some 5
    | '6' -> Some 6
    | '7' -> Some 7
    | '8' -> Some 8
    | '9' -> Some 9
    | _ -> None

let is_digit : char -> bool = function '0' .. '9' -> true | _ -> false

(* return the 1-indexed position of the first matching word in words *)
let leading_word words s =
    let rec loop i = function
        | [] -> None
        | w :: ws ->
            if String.starts_with ~prefix:w s then Some i
            else loop (i+1) ws in
    loop 1 words;;

let leading_digit (words: string list) (s: string): int option =
    (* iterate through and find the first char digit or word digit *)
    let rec loop substr =
        if substr = "" then None
        else
            match leading_word words substr with
            | None ->
                let c = String.get substr 0 in
                if is_digit c then char_to_int c
                else loop (String.sub substr 1 ((String.length substr) - 1))
            | some -> some
    in
    loop s

let unwrap_or default = function
    | Some v -> v
    | None -> default ;;

let rec solve ~words ~lines =
    let revwords = List.map reversed words in
    let rec loop total = function
        | [] -> total
        | line :: rest ->
            match leading_digit words line with
            | Some first  ->
                let revline = reversed line in
                let last = unwrap_or first (leading_digit revwords revline) in
                let amount = first * 10 + last in
                loop (total + amount) rest
            | None -> loop total rest
    in
    loop 0 lines ;;

let read_lines file =
    let contents = In_channel.with_open_bin file In_channel.input_all in
    String.split_on_char '\n' contents ;;

let lines = read_lines "input" ;;

(* part one *)
Printf.printf "part one: %d\n" (solve ~words:[] ~lines:lines) ;;

(* part two *)
let words = ["one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"] in
Printf.printf "part two: %d\n" (solve ~words:words ~lines:lines);;
