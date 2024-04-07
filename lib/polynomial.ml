type coeff = Q.t

let pp_coeff fmt = CCFormat.fprintf fmt "%a" Q.pp_print

type term = coeff * Monomial.t [@@deriving show]
type t = term list [@@deriving show]

let parse_term is_neg str =
  let chars = CCString.to_list str in
  let coeff_str, mon_str =
    CCList.take_drop_while (fun c -> Util.is_digit c || c = '/') chars
  in
  let coeff =
    if coeff_str = [] then Q.one
    else coeff_str |> CCString.of_list |> Q.of_string
  in
  let coeff = if is_neg then Q.neg coeff else coeff in
  (coeff, mon_str |> CCString.of_list |> Monomial.of_string)

let of_string str =
  let strip s = CCString.replace ~sub:" " ~by:"" s in
  let str = strip str in
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
      (Q.of_int 2, Monomial.of_string "xy3z");
      (Q.of_ints 1 2, Monomial.of_string "x1");
      (Q.of_int (-1), Monomial.of_string "z");
    ]

let%test "of_string_w_const_term" =
  let p = of_string "2xyz + x2 + 7" in
  CCList.equal ( = ) p
    [
      (Q.of_int 2, Monomial.of_string "xyz");
      (Q.of_int 1, Monomial.of_string "x2");
      (Q.of_int 7, Monomial.of_string "");
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
  let p1 = of_string "2xyz+2z" in
  let p2 = collapse @@ of_string "3xyz+z-xyz+z" in
  CCList.equal ( = ) p1 p2

let ( + ) p1 p2 = collapse @@ p1 @ p2
let neg p = CCList.map (fun (c, m) -> (Q.neg c, m)) p
let ( - ) p1 p2 = p1 + neg p2

let%test "add" =
  let p1 = of_string "2ab3c+b2c" in
  let p2 = of_string "3abc + 4ab3c" in
  let expect = of_string "6ab3c+b2c+3abc" in
  CCList.equal ( = ) (p1 + p2) expect

let mon_mult m p =
  collapse @@ CCList.map (fun (coeff, mo) -> (coeff, Monomial.(m * mo))) p

let term_mult (coeff, m) p =
  let p_times_coeff =
    CCList.map (fun (coeffo, mo) -> (Q.(coeff * coeffo), mo)) p
  in
  mon_mult m p_times_coeff

let ( * ) p1 p2 =
  collapse @@ CCList.fold_left (fun p t -> term_mult t p2 + p) [] p1

let%test "times" =
  let p1 = of_string "2x3y2z + 3xyz" in
  let p2 = of_string "xy2 + 9z4" in
  let expect = of_string "3x2y3z + 27xyz5 + 2x4y4z + 18x3y2z5" in
  CCList.equal ( = ) (p1 * p2) expect

let%test "times_w_const_terms" =
  let p1 = of_string "xy + 1" in
  let p2 = of_string "x2 + 2" in
  let expect = of_string "x2 + 2 + x3y + 2xy" in
  CCList.equal ( = ) (p1 * p2) expect
(* TODO: equality should be irrespective of order, but don't really want to commit to one representation...*)

let sort_by_ord ~order p =
  (* We negate the compare function bc we want the biggest first *)
  let term_compare (_c, m1) (_c, m2) = CCInt.neg @@ order m1 m2 in
  CCList.sort term_compare p

let leading_term ~order p = p |> sort_by_ord ~order |> CCList.hd

let%test "leading_term" =
  let open Monomial.Order in
  let p = of_string "x3 + xy + z + 7" in
  let lt = leading_term ~order:grlex p in
  lt = (Q.one, Monomial.of_string "x3")

let leading_coeff ~order p =
  let coeff, _mon = leading_term ~order p in
  coeff

let leading_mon ~order p =
  let _coeff, mon = leading_term ~order p in
  mon
