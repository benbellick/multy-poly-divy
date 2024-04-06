(* From https://stackoverflow.com/a/49184157 *)
let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
let is_digit = function '0' .. '9' -> true | _ -> false

module Monomial = struct
  (* For now, simplifying assumption that vars are chars *)
  type var = char
  type exp = int
  type t = (var * exp) list

  let of_string str =
    let rec parse_chars = function
      | [] -> []
      | chars ->
          let var, rest = CCList.hd_tl chars in
          let exp_str, rest = CCList.take_drop_while is_digit rest in
          let exp =
            if exp_str = [] then 1 else int_of_string (CCString.of_list exp_str)
          in
          (var, exp) :: parse_chars rest
    in
    parse_chars @@ CCString.to_list str

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

  let ( * ) m1 m2 = collapse @@ m1 @ m2

  let%test "mult" =
    CCList.equal ( = )
      ([ ("x", 2); ("y", 3) ] * [ ("z", 1); ("x", 1) ])
      [ ("x", 3); ("y", 3); ("z", 1) ]

  let ( / ) m1 m2 =
    let negate m = CCList.map (fun (v, e) -> (v, -e)) m in
    let result = collapse @@ m1 @ negate m2 in
    if CCList.exists (fun (_v, e) -> e < 0) result then None else Some result

  let%test "bad_div" = [ ("x", 2); ("y", 3) ] / [ ("z", 1); ("x", 1) ] = None

  let%test "div" =
    [ ("x", 2); ("y", 3) ] / [ ("y", 1); ("x", 1) ]
    = Some [ ("x", 1); ("y", 2) ]
end

type coeff = Q.t
type t = (coeff * Monomial.t) list

let parse_term is_neg str =
  let chars = CCString.to_list str in
  let coeff_str, mon_str =
    CCList.take_drop_while (fun c -> is_digit c || c = '/') chars
  in
  let coeff =
    if coeff_str = [] then Q.one
    else coeff_str |> CCString.of_list |> Q.of_string
  in
  let coeff = if is_neg then Q.neg coeff else coeff in
  (coeff, mon_str |> CCString.of_list |> Monomial.of_string)

let of_string str : t =
  let str = CCString.trim str in
  let chunks = CCString.split_on_char '+' str in
  let chunks = CCList.map (CCString.split_on_char '-') chunks in
  let handle_chunk = function
    | [] -> []
    | term :: [] -> [ parse_term false term ]
    | pos_term :: negs ->
        parse_term false pos_term
        :: CCList.map (fun neg_term -> parse_term true neg_term) negs
  in
  CCList.flatten @@ CCList.map handle_chunk chunks

let%test "of_string" =
  let p = of_string "2xy3z+1/2x-z" in
  CCList.equal ( = ) p
    [
      (Q.of_int 2, [ ('x', 1); ('y', 3); ('z', 1) ]);
      (Q.of_ints 1 2, [ ('x', 1) ]);
      (Q.of_int (-1), [ ('z', 1) ]);
    ]

let rec collapse = function
  | [] -> []
  | (a, mon) :: rest ->
      let matches_ms (_b, mon_other) = mon = mon_other in
      let matches, others = CCList.partition matches_ms rest in
      let sum_coeff =
        CCList.fold_left (fun sum (b, _mon) -> Q.(sum + b)) a matches
      in
      (sum_coeff, mon) :: collapse others

let%test "collapse" =
  let p1 = of_string "2xyz+z" in
  let p2 = collapse @@ of_string "3xyz+z-xyz" in
  CCList.equal ( = ) p1 p2
