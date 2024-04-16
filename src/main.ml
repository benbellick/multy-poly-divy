open React.Dom.Dsl
open Html

module Quotients = struct
  let%component make ~quotients () =
    let mk_label i = string @@ "q_" ^ string_of_int i ^ ": " in
    let mk_q_input i _q = label [||] [ mk_label i; input [||] [] ] in
    form [||] [ div [||] @@ CCList.mapi mk_q_input quotients ]
end

let%component make () = div [||] [ Quotients.make ~quotients:[ "a"; "b" ] () ]
