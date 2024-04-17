open React.Dom.Dsl
open Html

module Quotients = struct
  let%component make ~quo_count () =
    let mk_label i = string @@ "q_" ^ string_of_int i ^ ": " in
    let q_inputs =
      CCList.init quo_count (fun i -> label [||] [ mk_label i; input [||] [] ])
    in
    form [||] [ div [||] @@ q_inputs ]
end

external to_input_element :
  Ojs.t -> Js_of_ocaml.Dom_html.inputElement Js_of_ocaml.Js.t = "%identity"

let%component make () =
  let quo_count, set_quo_count = React.use_state CCFun.(const 1) in
  div [||]
    [
      input
        [|
          type_ "number";
          value (string_of_int quo_count);
          onChange (fun e ->
              set_quo_count (fun _ ->
                  (React.Event.Form.target e |> to_input_element)##.value
                  |> Js_of_ocaml.Js.parseInt));
        |]
        [];
      Quotients.make ~quo_count ();
    ]
