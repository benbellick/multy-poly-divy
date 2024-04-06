(* For now, simplifying assumption that vars are chars *)
type var = char [@@deriving show]
type exp = int [@@deriving show]
type t = (var * exp) list [@@deriving show]

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
  [ ("x", 2); ("y", 3) ] / [ ("y", 1); ("x", 1) ] = Some [ ("x", 1); ("y", 2) ]
