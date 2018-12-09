open Core
open Utils

(* PART 1 *)

let getOperator str = String.get str 0

let getNumber str = Pervasives.int_of_string (BatString.lchop str) 

let checkSymbolOperation (acc: int) (line: string): int = 
  let operator = getOperator line in 
  match operator with 
  | '+' -> acc + (getNumber line)
  | '-' -> acc - (getNumber line)
  | operator -> acc

let getFrequecyTotal (l: string list): int =
  List.fold_left ~f:checkSymbolOperation ~init:0 l

(* PART 2 *)

module SS = Set.Make(String)
module SI = Set.Make(Int)

let first list = match list with 
  | x :: xs -> x
  | [] -> (Failure "Empty list.")

let rec foldUntil f acc set = function 
  | x :: xs when (SI.mem set acc) -> acc
  | x :: xs -> foldUntil f (f acc x) (SI.add set acc) xs
  | [] -> 
    let restart = readFile "../files/day1-numbers.txt" in
    match restart with 
    | x :: xs -> foldUntil f (f acc x) (SI.add set acc) xs
    | [] -> acc

let frequencyUntilRepeat set list = foldUntil checkSymbolOperation 0 set list