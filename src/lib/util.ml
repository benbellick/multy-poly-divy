module String = struct
  (* From https://stackoverflow.com/a/49184157 *)
  let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
  let is_digit = function '0' .. '9' -> true | _ -> false

  let of_list l =
    let buf = Buffer.create (List.length l) in
    List.iter (Buffer.add_char buf) l;
    Buffer.contents buf

  let rec _to_list s acc i len =
    if len = 0 then List.rev acc else _to_list s (s.[i] :: acc) (i + 1) (len - 1)

  let to_list s = _to_list s [] 0 (String.length s)

  let strip s =
    let chars = to_list s in
    let is_not_space c = c <> ' ' in
    let stripped_chars = List.filter is_not_space chars in
    of_list stripped_chars
end

module List = struct
  let take_drop_while p l =
    let rec sub t p = function
      | [] -> (t, [])
      | l :: ls -> if p l then sub (t @ [ l ]) p ls else (t, l :: ls)
    in
    sub [] p l

  let set_at_idx i x ls = List.mapi (fun i' x' -> if i = i' then x else x') ls
end

module Option = struct
  let ( >|= ) x f = Option.map f x
  let ( let+ ) = ( >|= )
end
