let lex = Monomial.Order.lex

let attempt_div ~order ~fs ~qs ~p =
  (* Returns Some(updated_qs, updated_p) if something happened
     None otherwise *)
  let open CCOption in
  let p_lt = Polynomial.leading_term ~order p in
  let find_div i f_i =
    let f_i_lt = Polynomial.leading_term ~order f_i in
    let+ res = Polynomial.div p_lt f_i_lt in
    (Polynomial.poly_of_term res, i)
  in
  let+ quotient, i = CCList.find_mapi find_div fs in
  let q_i = CCList.nth qs i in
  let f_i = CCList.nth fs i in
  Polynomial.(CCList.set_at_idx i (q_i + quotient) qs, p - (quotient * f_i))

let step ~order ~fs ~qs ~r ~p =
  match attempt_div ~order ~fs ~qs ~p with
  | Some (qs_updated, p_updated) -> (fs, qs_updated, r, p_updated)
  | None ->
      let lt_p = Polynomial.(poly_of_term @@ leading_term ~order p) in
      let r_updated = Polynomial.(r + lt_p) in
      let p_updated = Polynomial.(p - lt_p) in
      (fs, qs, r_updated, p_updated)

let top ~order f fs =
  (* returns (quotient results, remainders) *)
  let rec loop ~fs ~qs ~r ~p =
    if Polynomial.(equal p zero) then (qs, r)
    else
      let fs', qs', r', p' = step ~order ~fs ~qs ~r ~p in
      loop ~fs:fs' ~qs:qs' ~r:r' ~p:p'
  in
  let s = CCList.length fs in
  let qs = CCList.init s (fun _ -> Polynomial.zero) in
  let r = Polynomial.zero in
  let p = f in
  loop ~fs ~qs ~r ~p

let%test "div_lex1" =
  let f = Polynomial.of_string "x2y + xy2 + y2" in
  let fs = CCList.map Polynomial.of_string [ "xy-1"; "y2-1" ] in
  let qs, r = top ~order:Monomial.Order.lex f fs in
  let qs_expected = CCList.map Polynomial.of_string [ "x + y"; "1" ] in
  let r_expected = Polynomial.of_string "x + y + 1" in
  CCList.equal Polynomial.equal qs qs_expected && Polynomial.equal r r_expected

let%test "div_lex1_swap_order" =
  let f = Polynomial.of_string "x2y + xy2 + y2" in
  let fs = CCList.map Polynomial.of_string [ "y2-1"; "xy-1" ] in
  let qs, r = top ~order:Monomial.Order.lex f fs in
  let qs_expected = CCList.map Polynomial.of_string [ "x + 1"; "x" ] in
  let r_expected = Polynomial.of_string "2x + 1" in
  CCList.equal Polynomial.equal qs qs_expected && Polynomial.equal r r_expected

let%test "div_grlex1_swap_order" =
  let f = Polynomial.of_string "x2y + xy2 + y2" in
  let fs = CCList.map Polynomial.of_string [ "y2-1"; "xy-1" ] in
  let qs, r = top ~order:Monomial.Order.grlex f fs in
  let qs_expected = CCList.map Polynomial.of_string [ "x + 1"; "x" ] in
  let r_expected = Polynomial.of_string "2x+1" in
  CCList.equal Polynomial.equal qs qs_expected && Polynomial.equal r r_expected

let%test "div_grlex2" =
  let f = Polynomial.of_string "x7y2 + x3y2 - y + 1" in
  let fs = CCList.map Polynomial.of_string [ "xy2 - x"; "-y3 + x" ] in
  let qs, r = top ~order:Monomial.Order.grlex f fs in
  let qs_expected = CCList.map Polynomial.of_string [ "x6 + x2"; "0" ] in
  let r_expected = Polynomial.of_string "x7 + x3 - y + 1" in
  CCList.equal Polynomial.equal qs qs_expected && Polynomial.equal r r_expected
