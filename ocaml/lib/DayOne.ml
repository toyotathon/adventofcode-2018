open Core

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
