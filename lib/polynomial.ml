module Monomial = struct
  type var = string
  type exp = int
  type t = (var * exp) list

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

type t = (Q.t * Monomial.t) list
