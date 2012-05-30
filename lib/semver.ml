open List
open Printf
open Scanf

type semver = Semver of int * int * int

type query = QueryPatch of int * int * int
           | QueryMinor of int * int
           | QueryMajor of int

let compare_version l r = match l, r with
    Semver (l1, l2, l3), Semver (r1, r2, r3) ->
      match compare l1 r1, compare l2 r2, compare l3 r3 with
          0, 0, res -> res
        | 0, res, _ -> res
        | res, _, _ -> res

let increment_version p v = match p, v with
    `Major, Semver (l1, _, _) -> Semver (l1 + 1, 0, 0)
  | `Minor, Semver (l1, l2, _) -> Semver (l1, l2 + 1, 0)
  | `Patch, Semver (l1, l2, l3) -> Semver (l1, l2, l3 + 1)

let decrement_version p v = match p, v with
    `Major, Semver (l1, _, _) -> Semver (l1 - 1, 0, 0)
  | `Minor, Semver (l1, l2, _) -> Semver (l1, l2 - 1, 0)
  | `Patch, Semver (l1, l2, l3) -> Semver (l1, l2, l3 - 1)

let parse_version input = sscanf input "%d.%d.%d" (fun v1 v2 v3 -> Semver (v1, v2, v3))

let print_version (Semver (v1, v2, v3)) = sprintf "%d.%d.%d" v1 v2 v3

(* Should this just expect a sorted list instead of sorting it itself? *)
let query_version query versions =
  let last ls = nth ls (length ls - 1) in
  let compareQuery q v = match q, v with
      QueryPatch (maj, min, patch), v' -> Semver (maj, min, patch) == v'
    | QueryMinor (maj, min), Semver (v1, v2, _) -> maj == v1 && min == v2
    | QueryMajor maj, Semver (v1, _, _) -> maj == v1 in
  let res = (sort compare_version (filter (compareQuery query) versions)) in
  if res == [] then None else Some (last res)

let parse_query input =
  try sscanf input "%d.%d.%d" (fun v1 v2 v3 -> QueryPatch (v1, v2, v3))
  with End_of_file ->
    try sscanf input "%d.%d" (fun v1 v2 -> QueryMinor (v1, v2))
    with End_of_file -> sscanf input "%d" (fun v1 -> QueryMajor v1)

let print_query = function
    QueryPatch (maj, min, patch) -> sprintf "%d.%d.%d" maj min patch
  | QueryMinor (maj, min) -> sprintf "%d.%d" maj min
  | QueryMajor maj -> sprintf "%d" maj
   
let query q vs = query_version (parse_query q) vs
