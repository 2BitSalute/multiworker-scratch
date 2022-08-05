module Int64Set = Set.Make(Int64)

(* Type? redirect: bool? *)
type article = {
  topic: string;
  hash: int64;
  resolved_deps: int64 list;
  unresolved_deps: string list;
}

let to_topic substrings =
  let s = Pcre.get_substring substrings 1 in
  let strings = Pcre.split ~pat:"\\|" ~max:0 s in
  List.hd strings

let extract_data i =
  match Xmlm.input i with
  | `Data data -> data
  | _ -> assert false

let examine_text data : (int64 * string list) =
  let rex = Pcre.regexp {|\[\[([^[{]+?)\]\]|} in
  let rec loop topics (substrings: Pcre.substrings option) : (int64 * string list) =
    try
      let substrings = match substrings with
        | None -> (Pcre.exec ~rex data)
        | Some substrings -> Pcre.next_match ~rex substrings
      in
      loop ((to_topic substrings) :: topics) (Some substrings)
    with Not_found ->
      (Murmur3.hash64 data, topics)
  in
  loop [] None

let examine_article i =
  let rec process articles title i depth =
    match Xmlm.input i with
    | `El_start ((_namespace, tag), _attributes) when tag = "title" ->
      process articles (Some (extract_data i)) i (depth + 1)
    | `El_start ((_namespace, tag), _attributes) when tag = "text" ->
      begin
        match title with
        | Some topic ->
          let (hash, unresolved_deps) = extract_data i |> examine_text in
          let article = { topic; hash; unresolved_deps; resolved_deps = [] } in
          process (article :: articles) None i (depth + 1)
        | None -> assert false
      end
    | `El_start _ -> process articles title i (depth + 1)
    | `El_end -> if depth = 0 then articles else process articles title i (depth - 1)
    | `Data _ -> process articles title i depth
    | `Dtd _ -> assert false
  in
  let _dtd = Xmlm.input i in
  let _root = Xmlm.input i in
  process [] None i 0

let run_with_string s : article list =
  let i = Xmlm.make_input (`String (0, s)) in
  examine_article i

let run filename =
  let ic = In_channel.open_bin filename in
  let i = Xmlm.make_input (`Channel ic) in
  ignore (examine_article i);
  In_channel.close ic;
  ()