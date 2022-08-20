module Int64Set = Set.Make(Int64)

(* Type? redirect: bool? *)
type article = {
  topic: string;
  hash: int64;
  resolved_deps: int64 list;
  deps: string list;
}

let to_topic substrings =
  let s = Pcre.get_substring substrings 1 in
  try
    let strings = Pcre.split ~pat:"\\|" ~max:0 s in
    let topic = List.hd (Pcre.split ~pat:"#" ~max:0 (List.hd strings)) in
    if topic = "" then failwith "Empty topic - likely a reference to the section"
    else topic
  with _ ->
    (* If there's something wrong with the title, it'll be reported as not found *)
    (* Example of an invalid title: [[#|see section]] in the CBS Morning News article *)
    s

let extract_data i =
  match Xmlm.input i with
  | `Data data -> Some data
  | `El_end -> None
  | _ ->
    assert false

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

let examine_article ~log i =
  let rec process articles title i depth =
    match Xmlm.input i with
    | `El_start ((_namespace, tag), _attributes) when tag = "title" ->
      begin
        match extract_data i with
        | Some title ->
          if log then
            Printf.printf "Title: %s\n" title;
          process articles (Some title) i (depth + 1)
        | None ->
          failwith "No title!"
      end
    | `El_start ((_namespace, tag), _attributes) when tag = "text" ->
      begin
        match title with
        | Some topic ->
          begin
            match extract_data i with
            | Some text ->
              let (hash, deps) = examine_text text in
              let article = { topic; hash; deps; resolved_deps = [] } in
              process (article :: articles) None i (depth + 1)
            | None ->
              Printf.printf "No text in %s\n" topic;
              process articles title i depth
          end
        | None ->
          failwith "Text precedes title"
      end
    | `El_start _ -> process articles title i (depth + 1)
    | `El_end -> if depth = 0 then articles else process articles title i (depth - 1)
    | `Data _ -> process articles title i depth
    | `Dtd _ -> assert false
  in
  let _dtd = Xmlm.input i in
  let _root = Xmlm.input i in
  process [] None i 0

let run_with_string ~log s : article list =
  let i = Xmlm.make_input (`String (0, s)) in
  (* Printf.printf "%s" s; *)
  try
    examine_article ~log i
  with _ ->
    Printf.printf "%s" s;
    failwith "Printed article"

let run filename =
  let ic = In_channel.open_bin filename in
  let i = Xmlm.make_input (`Channel ic) in
  ignore (examine_article ~log:false i);
  In_channel.close ic;
  ()