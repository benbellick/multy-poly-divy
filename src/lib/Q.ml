type t = int * int (*numerator x denominator*)

(*  *)
let rec gcd a b =
  if a = b then a else if a > b then gcd (a - b) b else gcd a (b - a)

let reduce (n, d) =
  (* Always reduce and maintain invariant that denom is pos *)
  if n = 0 && d <> 0 then (0, 1)
  else
    let c = gcd n d in
    let n', d' = (n / c, d / c) in
    if d' < 0 then (-1 * n', -1 * d') else (n', d')

let recip (n, d) = (d, n)
let neg (n, d) = (-1 * n, d)
let mk n d = reduce (n, d)
let add (n1, d1) (n2, d2) = reduce @@ ((n1 * d2) + (n2 * d1), d1 * d2)
let sub r1 r2 = add r1 (neg r2)
let mult (n1, d1) (n2, d2) = reduce @@ (n1 * n2, d1 * d2)

let div r1 r2 =
  let r2_inv = recip r2 in
  reduce @@ mult r1 r2_inv

let eq (n1, d1) (n2, d2) = n1 * d2 = n2 * d1
let ( + ) = add
let ( - ) = sub
let ( * ) = mult
let ( / ) = div
let zero = (0, 1)
let one = (1, 1)

let of_string s =
  match String.split_on_char '/' s with
  | [ num ] -> reduce (int_of_string num, 1)
  | [ num; denom ] -> reduce (int_of_string num, int_of_string denom)
  | _ -> failwith "Error"
