(* For now, simplifying assumption that vars are chars *)
type var = char [@@deriving show]
type exp = int [@@deriving show]

(* By convention, the empty list will represent the constant term  *)
type t = (var * exp) list [@@deriving show]

let equal (m1 : t) (m2 : t) = CCList.equal ( = ) m1 m2

open Stdlib

(* By convention, we sort individual vars in lex ordering,
   i.e. x < y < z. We maintain the invariant that all monomials are sorted. *)
let sort_mon (m : t) =
  CCList.sort (fun (v1, _exp) (v2, _exp) -> compare v1 v2) m

let of_string str =
  (* print_endline "here is the string...."; *)
  (* print_endline str; *)
  let rec parse_chars = function
    | [] -> []
    | chars ->
        let var, rest = CCList.hd_tl chars in
        let exp_str, rest = CCList.take_drop_while Util.is_digit rest in
        let exp =
          if exp_str = [] then 1 else int_of_string (CCString.of_list exp_str)
        in
        (var, exp) :: parse_chars rest
  in
  (* We sort the monomial, and this is the only place monomials can be constructed *)
  (* Q: Do we need to collapse? Only if users input double vars... *)
  str |> CCString.to_list |> parse_chars |> sort_mon

let%test "sort" = equal (of_string "z4y2x") [ ('x', 1); ('y', 2); ('z', 4) ]

let%test "of_string" =
  equal (of_string "x2y3z4") [ ('x', 2); ('y', 3); ('z', 4) ]

let rec collapse = function
  | [] -> []
  | (x, e) :: rest ->
      let matches_id (y, _e) = x = y in
      let matches, others = CCList.partition matches_id rest in
      let sum_exp = CCList.fold_left (fun sum (_x, e) -> sum + e) e matches in
      if sum_exp = 0 then collapse others else (x, sum_exp) :: collapse others

let%test "collapse" =
  equal
    [ ('x', 2); ('y', 3) ]
    (collapse [ ('x', 1); ('y', 2); ('x', 1); ('y', 1) ])

let to_string m =
  let to_str_ls (v, e) =
    if e > 1 then [ CCChar.to_string v; string_of_int e ]
    else [ CCChar.to_string v ]
  in
  CCString.concat "" @@ CCList.flatten @@ CCList.map to_str_ls m

let%test "to_string" =
  let m = [ ('a', 1); ('x', 2); ('y', 3); ('z', 1) ] in
  let s = to_string m in
  s = "ax2y3z"

(* The monomial remains sorted in lex as long as m1 is sorted *)
let ( * ) m1 m2 = sort_mon @@ collapse @@ m1 @ m2

let%test "mult" =
  equal
    ([ ('x', 2); ('y', 3) ] * [ ('z', 1); ('x', 1) ])
    [ ('x', 3); ('y', 3); ('z', 1) ]

let%test "mult_order_challenge" =
  (* This test shows why we need to sort again after multiplication *)
  equal
    ([ ('a', 2); ('c', 3) ] * [ ('b', 1); ('z', 1) ])
    [ ('a', 2); ('b', 1); ('c', 3); ('z', 1) ]

let ( / ) m1 m2 =
  (* Here, no need to sort again as long as m1 is sorted.
     If the second term contains new vars, we get None anyways *)
  let negate m = CCList.map (fun (v, e) -> (v, -e)) m in
  let result = collapse @@ m1 @ negate m2 in
  if CCList.exists (fun (_v, e) -> e < 0) result then None else Some result

let%test "bad_div" = [ ("x", 2); ("y", 3) ] / [ ("z", 1); ("x", 1) ] = None

let%test "div" =
  [ ("x", 2); ("y", 3) ] / [ ("y", 1); ("x", 1) ] = Some [ ("x", 1); ("y", 2) ]

let ord m = CCList.fold_left (fun o (_v, e) -> o + e) 0 m
let const = []

module Order = struct
  type mon_compare = t -> t -> int

  (* Remember that here, we can assume each mon is sorted already *)
  let rec lex (m1 : t) (m2 : t) =
    match (m1, m2) with
    | [], [] -> 0
    | _, [] -> 1
    | [], _ -> -1
    | (v1, e1) :: rest1, (v2, e2) :: rest2
      when CCChar.equal v1 v2 && CCInt.equal e1 e2 ->
        lex rest1 rest2
    | (v1, e1) :: _, (v2, e2) :: _ when CCChar.equal v1 v2 -> compare e1 e2
    | (v1, _e1) :: _, (v2, _e2) :: _ -> compare v2 v1

  let%test "lex_same_var" =
    let m1 = of_string "x2y3" in
    let m2 = of_string "x3y3" in
    lex m1 m2 < 0

  let%test "lex_same_var_same_exp" =
    let m1 = of_string "x3y3" in
    let m2 = of_string "x3y2" in
    lex m1 m2 > 0

  let%test "lex_diff_var" =
    let m1 = of_string "x3y3" in
    let m2 = of_string "y2z" in
    lex m1 m2 > 0

  let%test "lex_diff_length" =
    let m1 = of_string "x3y3" in
    let m2 = of_string "x3" in
    lex m1 m2 > 0

  let grlex m1 m2 =
    let o1 = ord m1 in
    let o2 = ord m2 in
    if o1 < o2 then -1 else if o1 > o2 then 1 else lex m1 m2

  let%test "grlex" =
    let m1 = of_string "n2m3o4" in
    let m2 = of_string "x100" in
    grlex m1 m2 < 0

  let%test "grlex_same_ord" =
    let m1 = of_string "a2b3" in
    let m2 = of_string "a3b2" in
    grlex m1 m2 < 0

  let grevlex m1 m2 =
    let o1 = ord m1 in
    let o2 = ord m2 in
    if o1 < o2 then -1
    else if o1 > o2 then 1
    else CCInt.neg @@ grlex (CCList.rev m1) (CCList.rev m2)

  let%test "grevlex_diff_ord" =
    let m1 = of_string "xy4z2" in
    let m2 = of_string "x4yz3" in
    grevlex m1 m2 < 0

  let%test "grevlex_same_ord" =
    let m1 = of_string "xy5z2" in
    let m2 = of_string "x4yz3" in
    grevlex m1 m2 > 0
end
