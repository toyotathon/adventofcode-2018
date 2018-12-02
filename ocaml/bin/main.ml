open Core
open Lib.DayOne
open Lib.Utils

let file = "../files/day1-numbers.txt"

let () =
  let lines = readFile file in
  let sum: int = getFrequecyTotal lines in
  printf "Resulting frequency: %s\n" (Pervasives.string_of_int sum)