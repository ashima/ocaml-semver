open Scanf
open Printf

module Semver = struct

  type semver = Semver of int option * int option * int option
  type semver_part = Major | Minor | Patch

  exception Invalid_version

  let cmp_option = fun a b -> match a, b with
      None, None -> 0
    | None, _ -> 1
    | _, None -> -1
    | Some a, Some b when a > b -> 1
    | Some a, Some b when b > a -> -1
    | _ -> 0

  let compare_version = fun l r -> match l, r with
      Semver (l1, l2, l3), Semver (r1, r2, r3) ->
        match cmp_option l1 r1, cmp_option l2 r2, cmp_option l3 r3 with
            0, 0, res -> res
          | 0, res, _ -> res
          | res, _, _ -> res

  let inc_option = function None -> None | Some n -> Some (n + 1)

  let increment_version = fun p v -> match p, v with
      Major, Semver (l1, _, _) -> Semver (inc_option l1, Some 0, Some 0)
    | Minor, Semver (l1, l2, _) -> Semver (l1, inc_option l2, Some 0)
    | Patch, Semver (l1, l2, l3) -> Semver (l1, l2, inc_option l3)

  let dec_option = function None -> None | Some n -> Some (n - 1)

  let decrement_version = fun p v -> match p, v with
      Major, Semver (l1, _, _) -> Semver (dec_option l1, Some 0, Some 0)
    | Minor, Semver (l1, l2, _) -> Semver (l1, dec_option l2, Some 0)
    | Patch, Semver (l1, l2, l3) -> Semver (l1, l2, dec_option l3)

  let parse_version = fun input ->
    try
      sscanf input "%d.%d.%d" (fun v1 v2 v3 -> Semver (Some v1, Some v2, Some v3))
    with End_of_file -> try
      sscanf input "%d.%d" (fun v1 v2 -> Semver (Some v1, Some v2, None))
    with End_of_file -> try
      sscanf input "%d" (fun v1 -> Semver (Some v1, None, None))
    with End_of_file -> Semver (None, None, None)

  let print_version = fun (Semver (v1, v2, v3)) -> match v1, v2, v3 with
      None, None, None -> ""
    | Some v1, None, None -> sprintf "%d" v1
    | Some v1, Some v2, None -> sprintf "%d.%d" v1 v2
    | Some v1, Some v2, Some v3 -> sprintf "%d.%d.%d" v1 v2 v3
    | _ -> raise Invalid_version
end
