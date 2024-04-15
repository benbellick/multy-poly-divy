type coeff = Q.t

(* let pp_coeff fmt = Format.fprintf fmt "%a" Q.pp_print *)

type term = coeff * Monomial.t [@@deriving show]
type t = term list [@@deriving show]

let rec collapse = function
  | [] -> []
  | (a, mon) :: rest ->
      let matches_ms (_b, mon_other) = mon = mon_other in
      let matches, others = List.partition matches_ms rest in
      let sum_coeff =
        List.fold_left (fun sum (b, _mon) -> Q.(sum + b)) a matches
      in
      if Q.(eq sum_coeff zero) then collapse others
      else (sum_coeff, mon) :: collapse others

let parse_term is_neg str =
  let chars = Util.String.to_list str in
  let coeff_str, mon_str =
    Util.List.take_drop_while (fun c -> Util.String.is_digit c || c = '/') chars
  in
  let coeff =
    if coeff_str = [] then Q.one
    else coeff_str |> Util.String.of_list |> Q.of_string
  in
  let coeff = if is_neg then Q.neg coeff else coeff in
  (coeff, mon_str |> Util.String.of_list |> Monomial.of_string)

let of_string str =
  let strip s = Util.String.strip s in
  let str = strip str in
  let chunks = String.split_on_char '+' str in
  let chunks = List.map (String.split_on_char '-') chunks in
  let handle_chunk = function
    | [] -> []
    | term :: [] -> [ parse_term false term ]
    | "" :: negs -> List.map (fun neg_term -> parse_term true neg_term) negs
    | pos_term :: negs ->
        parse_term false pos_term
        :: List.map (fun neg_term -> parse_term true neg_term) negs
  in
  collapse @@ List.flatten @@ List.map handle_chunk chunks

let sort_by_ord ~order p =
  (* We negate the compare function bc we want the biggest first *)
  let term_compare (_c, m1) (_c, m2) = Int.neg @@ order m1 m2 in
  List.sort term_compare p

let equal p1 p2 =
  (* Can later think about if this is really the most efficient way to compare.
     For example, if we know both are sorted already by diff order,
     don't have to resort *)
  let p1_sorted = sort_by_ord ~order:Monomial.Order.lex p1 in
  let p2_sorted = sort_by_ord ~order:Monomial.Order.lex p2 in
  List.equal ( = ) p1_sorted p2_sorted

(* let%test "equal" = *)
(*   let p1 = of_string "2xy3z4 + 1/2yz + 2" in *)
(*   let p2 = of_string "2 + 2xz4y3 + 1/2yz" in *)
(*   let p3 = of_string "2 + 1/2zy + 2y3xz4" in *)
(*   equal p1 p2 && equal p2 p3 *)

(* let%test "of_string" = *)
(*   let p = of_string "2xy3z+1/2x-z" in *)
(*   equal p *)
(*     [ *)
(*       (Q.of_int 2, Monomial.of_string "xy3z"); *)
(*       (Q.of_ints 1 2, Monomial.of_string "x1"); *)
(*       (Q.of_int (-1), Monomial.of_string "z"); *)
(*     ] *)

(* let%test "of_string_w_const_term" = *)
(*   let p = of_string "2xyz + x2 + 7" in *)
(*   equal p *)
(*     [ *)
(*       (Q.of_int 2, Monomial.of_string "xyz"); *)
(*       (Q.of_int 1, Monomial.of_string "x2"); *)
(*       (Q.of_int 7, Monomial.of_string ""); *)
(*     ] *)

(* let%test "of_string_leading_neg" = *)
(*   let p = of_string "-y3 + x" in *)
(*   let p_expect = *)
(*     [ (Q.(neg one), Monomial.of_string "y3"); (Q.one, Monomial.of_string "x") ] *)
(*   in *)
(*   equal p p_expect *)

(* let%test "of_string_leading_neg" = *)
(*   let p = of_string "-y3" in *)
(*   let p_expect = [ (Q.(neg one), Monomial.of_string "y3") ] in *)
(*   equal p p_expect *)

(* let%test "collapse" = *)
(*   let p1 = of_string "2xyz+2z" in *)
(*   let p2 = collapse @@ of_string "3xyz+z-xyz+z" in *)
(*   equal p1 p2 *)

let ( + ) p1 p2 = collapse @@ p1 @ p2
let neg p = List.map (fun (c, m) -> (Q.neg c, m)) p
let ( - ) p1 p2 = p1 + neg p2

(* let%test "add" = *)
(*   let p1 = of_string "2ab3c+b2c" in *)
(*   let p2 = of_string "3abc + 4ab3c" in *)
(*   let expect = of_string "6ab3c+b2c+3abc" in *)
(*   equal (p1 + p2) expect *)

let mon_mult m p =
  collapse @@ List.map (fun (coeff, mo) -> (coeff, Monomial.(m * mo))) p

let term_mult (coeff, m) p =
  let p_times_coeff =
    List.map (fun (coeffo, mo) -> (Q.(coeff * coeffo), mo)) p
  in
  mon_mult m p_times_coeff

let ( * ) p1 p2 =
  collapse @@ List.fold_left (fun p t -> term_mult t p2 + p) [] p1

(* let%test "times" = *)
(*   let p1 = of_string "2x3y2z + 3xyz" in *)
(*   let p2 = of_string "xy2 + 9z4" in *)
(*   let expect = of_string "3x2y3z + 27xyz5 + 2x4y4z + 18x3y2z5" in *)
(*   equal (p1 * p2) expect *)

(* let%test "times_w_const_terms" = *)
(*   let p1 = of_string "xy + 1" in *)
(*   let p2 = of_string "x2 + 2" in *)
(*   let expect = of_string "x2 + 2 + x3y + 2xy" in *)
(*   equal (p1 * p2) expect *)

let leading_term ~order p = p |> sort_by_ord ~order |> List.hd

(* let%test "leading_term" = *)
(*   let open Monomial.Order in *)
(*   let p = of_string "x3 + xy + z + 7" in *)
(*   let lt = leading_term ~order:grlex p in *)
(*   lt = (Q.one, Monomial.of_string "x3") *)

let div (c1, m1) (c2, m2) =
  Option.map (fun m -> (Q.(c1 / c2), m)) Monomial.(m1 / m2)

let poly_of_term t = [ t ]

let leading_coeff ~order p =
  let coeff, _mon = leading_term ~order p in
  coeff

let leading_mon ~order p =
  let _coeff, mon = leading_term ~order p in
  mon

let zero = []

(* let%test "zero" = *)
(*   let p = of_string "k3 + d4 + 1" in *)
(*   equal (p * zero) zero && CCList.equal ( = ) (p + zero) p *)

let one = [ (Q.one, Monomial.const) ]

(* let%test "one" = *)
(*   let p = of_string "4a5 + 2/3d2 + 7" in *)
(*   equal (p * one) p *)
