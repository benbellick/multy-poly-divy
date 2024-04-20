open React.Dom.Dsl
open Html

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
  let%component make ~quo_count ~set_quotients ~set_dividend () =
    let _ = set_dividend in
    let mk_label i = string @@ "q_" ^ string_of_int i ^ ": " in
    let q_inputs =
      CCList.init quo_count (fun i ->
          div [||] [ label [||] [ mk_label i; input [||] [] ] ])
    in
    let submit_button = button [||] [ string "Submit" ] in
    let handle_submit e =
      let () = React.Event.Form.prevent_default e in
      let t = React.Event.Form.current_target e in
      let elms = Getters.elements t in
      let vals = CCList.map Getters.value elms in
      (* CCList.iter (fun v -> Firebug.console##log (string v)) vals; *)
      let polys = CCList.map Lib.Polynomial.of_string vals in
      set_quotients (fun _ -> polys)
    in
    let f_input = label [||] [ string "f: "; input [||] [] ] in
    form
      [| onSubmit handle_submit |]
      [ div [||] @@ (f_input :: q_inputs) @ [ submit_button ] ]
end

let%component make () =
  let quo_count, set_quo_count = React.use_state CCFun.(const 1) in
  let _quotients, set_quotients = React.use_state CCFun.(const []) in
  let _dividend, set_dividend = React.use_state CCFun.(const None) in
  div [||]
    [
      label [||]
        [
          string "# of Divisors:";
          input
            [|
              type_ "number";
              value (string_of_int quo_count);
              onChange (fun e ->
                  e |> React.Event.Form.target |> Getters.value
                  |> int_of_string_opt
                  |> function
                  | Some i -> set_quo_count (fun _ -> i)
                  | None -> ());
              min "0";
            |]
            [];
        ];
      AlgoInputs.make ~quo_count ~set_quotients ~set_dividend ();
    ]
