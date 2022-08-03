let to_topic substrings =
  let s = Pcre.get_substring substrings 1 in
  let strings = Pcre.split ~pat:"\\|" ~max:0 s in
  List.hd strings

let examine_text i =
  match Xmlm.input i with
  | `Data data ->
    let rex = Pcre.regexp {|\[\[([^[{]+)\]\]|} in
    let rec loop topics (substrings: Pcre.substrings option) : string list =
      try
        let substrings = match substrings with
          | None -> (Pcre.exec ~rex data)
          | Some substrings -> Pcre.next_match ~rex substrings
        in
        loop ((to_topic substrings) :: topics) (Some substrings)
      with Not_found ->
        topics
    in
    loop [] None
  | _ -> assert false

let find_links i =
  let rec process topics i depth =

    match Xmlm.input i with
    | `El_start ((_namespace, tag), _attributes) when tag = "text" ->
      process (List.rev_append topics (examine_text i)) i (depth + 1)
    | `El_start _ -> process topics i (depth + 1)
    | `El_end -> if depth = 0 then topics else process topics i (depth - 1)
    | `Data _ -> process topics i depth
    | `Dtd _ -> assert false
  in
  let _dtd = Xmlm.input i in
  let _root = Xmlm.input i in
  process [] i 0

let run_with_string s =
  let i = Xmlm.make_input (`String (0, s)) in
  find_links i

let run filename =
  let ic = In_channel.open_bin filename in
  let i = Xmlm.make_input (`Channel ic) in
  ignore (find_links i);
  In_channel.close ic;
  ()