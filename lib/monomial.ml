(* For now, simplifying assumption that vars are chars *)
type var = char [@@deriving show]
type exp = int [@@deriving show]
type t = (var * exp) list [@@deriving show]

(** By convention, we sort individual vars in lex ordering,
 i.e. x < y < z. We maintain the invariant that all monomials are sorted. *)
let sort_mon (m : t) =
  CCList.sort (fun (v1, _exp) (v2, _exp) -> compare v1 v2) m

let of_string str =
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

let%test "sort" =
  CCList.equal ( = ) (of_string "z4y2x") [ ('x', 1); ('y', 2); ('z', 4) ]

let%test "of_string" =
  CCList.equal ( = ) (of_string "x2y3z4") [ ('x', 2); ('y', 3); ('z', 4) ]

let rec collapse = function
  | [] -> []
  | (x, e) :: rest ->
      let matches_id (y, _e) = x = y in
      let matches, others = CCList.partition matches_id rest in
      let sum_exp = CCList.fold_left (fun sum (_x, e) -> sum + e) e matches in
      (x, sum_exp) :: collapse others

let%test "collapse" =
  CCList.equal ( = )
    [ ("x", 2); ("y", 3) ]
    (collapse [ ("x", 1); ("y", 2); ("x", 1); ("y", 1) ])

(* The monomial remains sorted in lex as long as m1 is sorted *)
let ( * ) m1 m2 = sort_mon @@ collapse @@ m1 @ m2

let%test "mult" =
  CCList.equal ( = )
    ([ ('x', 2); ('y', 3) ] * [ ('z', 1); ('x', 1) ])
    [ ('x', 3); ('y', 3); ('z', 1) ]

let%test "mult_order_challenge" =
  (* This test shows why we need to sort again after multiplication *)
  CCList.equal ( = )
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

(* module Order = struct *)
(*   let sort_mon = CCList.sort *)
(*   let lex m1 m2 =  *)

(* end *)
