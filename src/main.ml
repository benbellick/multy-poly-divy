open React.Dom.Dsl
open Html
open Lib

external to_input_element :
  Ojs.t -> Js_of_ocaml.Dom_html.inputElement Js_of_ocaml.Js.t = "%identity"

external to_form_element :
  Ojs.t -> Js_of_ocaml.Dom_html.formElement Js_of_ocaml.Js.t = "%identity"

let value_of_event event =
  (React.Event.Form.target event |> to_input_element)##.value
  |> Js_of_ocaml.Js.to_string

(* let value_of_form_submit event = *)
(*   event |> React.Event.Form.target |> to_form_element *)
(*   |> Js_of_ocaml.Form.get_form_contents *)

let value_of_form_submit event =
  event |> React.Event.Form.target |> React.Dom.dom_element_of_js
(* |> Js_of_ocaml.Form.get_form_contents *)

module AlgoInputs = struct
  let%component make ~divisor_count ~set_divisors ~set_dividend () =
    let mk_label i = string @@ "q_" ^ string_of_int i ^ ": " in
    let q_inputs =
      CCList.init divisor_count (fun i ->
          div [||] [ label [||] [ mk_label i; input [||] [] ] ])
    in
    let submit_button = button [||] [ string "Submit" ] in
    let handle_submit e =
      let () = React.Event.Form.prevent_default e in
      let t = React.Event.Form.current_target e in
      let elms = Getters.elements t in
      let vals = CCList.map Getters.value elms in
      (* The last one is empy from submit button  *)
      let vals = CCList.take (CCList.length vals - 1) vals in
      let dividend_s, divisors_s = CCList.hd_tl vals in
      (* CCList.iter *)
      (*   (fun v -> Js_of_ocaml.Firebug.console##log (string v)) *)
      (*   divisors_s; *)
      let divisors = CCList.map Polynomial.of_string divisors_s in
      let dividend = Polynomial.of_string dividend_s in
      set_divisors (fun _ -> divisors);
      set_dividend (fun _ -> dividend)
    in

    let f_input = label [||] [ string "f: "; input [||] [] ] in
    form
      [| onSubmit handle_submit |]
      [ div [||] @@ (f_input :: q_inputs) @ [ submit_button ] ]
end

type ord_selection = Lex | Grlex | Grevlex
[@@deriving enum, show { with_path = false }]

module OrderSelection = struct
  let str_to_ord_selection_enum s =
    ord_selection_to_enum
      (match s with
      | "Lex" -> Lex
      | "Grlex" -> Grlex
      | "Grevlex" -> Grevlex
      | _ -> failwith "Unexpected ord_selection")

  let ord_selection_enum_to_str e =
    match ord_selection_of_enum e with
    | Some os -> os
    | None -> failwith "Incorrect value of ord_selection enum"

  let%component make ~mon_order_enum ~set_mon_order_enum () =
    let mk_option os =
      option
        [| os |> ord_selection_to_enum |> string_of_int |> value |]
        [ os |> show_ord_selection |> string ]
    in
    select
      [|
        onChange (fun e ->
            e |> React.Event.Form.target |> Getters.value |> int_of_string
            |> CCFun.(set_mon_order_enum % const));
        value (string_of_int mon_order_enum);
      |]
      (CCList.map mk_option [ Lex; Grlex; Grevlex ])
end

module DisplayResult = struct
  let mk_quotient_display idx p =
    div [||]
      [
        string ("q_" ^ string_of_int idx ^ ": ");
        string (Polynomial.to_string p);
      ]

  let%component make ~dividend ~divisors ~mon_order_enum () =
    let mon_compare =
      let open Monomial.Order in
      let ord_selection =
        CCOption.get_exn_or "Bad enum val"
          (ord_selection_of_enum mon_order_enum)
      in
      match ord_selection with
      | Lex -> lex
      | Grlex -> grlex
      | Grevlex -> grevlex
    in
    print_endline "Beginning division...";
    let quotients, remainder =
      Division.top ~order:mon_compare dividend divisors
    in
    print_endline "Division complete";
    let remainder_display =
      div [||] [ string "r: "; string (Polynomial.to_string remainder) ]
    in
    CCList.iter
      (fun v -> Js_of_ocaml.Firebug.console##log (Polynomial.show v))
      quotients;
    div [||] (CCList.mapi mk_quotient_display quotients @ [ remainder_display ])
end

let%component make () =
  let divisor_count, set_divisor_count = React.use_state CCFun.(const 1) in
  let divisors, set_divisors = React.use_state CCFun.(const []) in
  let dividend, set_dividend = React.use_state CCFun.(const Polynomial.one) in
  let mon_order_enum, set_mon_order_enum =
    React.use_state CCFun.(const (ord_selection_to_enum Lex))
  in
  div [||]
    [
      OrderSelection.make ~mon_order_enum ~set_mon_order_enum ();
      label [||]
        [
          string "# of Divisors:";
          input
            [|
              type_ "number";
              value (string_of_int divisor_count);
              onChange (fun e ->
                  e |> React.Event.Form.target |> Getters.value
                  |> int_of_string_opt
                  |> function
                  | Some i -> set_divisor_count (fun _ -> i)
                  | None -> ());
              min "1";
            |]
            [];
        ];
      AlgoInputs.make ~divisor_count ~set_divisors ~set_dividend ();
      DisplayResult.make ~dividend ~divisors ~mon_order_enum ();
    ]
