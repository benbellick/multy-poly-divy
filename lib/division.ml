let lex = Monomial.Order.lex

let attempt_div ~fs ~qs ~p =
  (* Returns Some(updated_qs, updated_p) if something happened
     None otherwise *)
  let open CCOption in
  let p_lt = Polynomial.leading_term ~order:lex p in
  let find_div i f_i =
    let f_i_lt = Polynomial.leading_term ~order:lex f_i in
    let+ res = Polynomial.div p_lt f_i_lt in
    (Polynomial.poly_of_term res, i)
  in
  let+ quotient, i = CCList.find_mapi find_div fs in
  let q_i = CCList.nth qs i in
  let f_i = CCList.nth fs i in
  Polynomial.(CCList.set_at_idx i (q_i + quotient) qs, p - (quotient * f_i))

let step ~fs ~qs ~r ~p =
  match attempt_div ~fs ~qs ~p with
  | Some (qs_updated, p_updated) -> (fs, qs_updated, r, p_updated)
  | None ->
      let lt_p = Polynomial.(poly_of_term @@ leading_term ~order:lex p) in
      let r_updated = Polynomial.(r + lt_p) in
      let p_updated = Polynomial.(p - lt_p) in
      (fs, qs, r_updated, p_updated)

let rec loop ~fs ~qs ~r ~p =
  if Polynomial.(equal p zero) then (qs, r)
  else
    let fs', qs', r', p' = step ~fs ~qs ~r ~p in
    loop ~fs:fs' ~qs:qs' ~r:r' ~p:p'

let top f fs =
  let s = CCList.length fs in
  let qs = CCList.init s (fun _ -> Polynomial.zero) in
  let r = Polynomial.zero in
  let p = f in
  loop ~fs ~qs ~r ~p

let%test "div" =
  let f = Polynomial.of_string "x2y + xy2 + y2" in
  let fs = CCList.map Polynomial.of_string [ "xy-1"; "y2-1" ] in
  let qs, r = top f fs in
  let qs_expected = CCList.map Polynomial.of_string [ "x + y"; "1" ] in
  let r_expected = Polynomial.of_string "x + y + 1" in
  CCList.equal Polynomial.equal qs qs_expected && Polynomial.equal r r_expected
