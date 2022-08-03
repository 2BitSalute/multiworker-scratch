let to_topic substrings =
  let s = Pcre.get_substring !substrings 1 in
  let strings = Pcre.split ~pat:"\\|" ~max:0 s in
  List.hd strings
  |> Pcre.replace ~templ:"_"
  |> String.lowercase_ascii

let examine_text i =
  let () = match Xmlm.input i with
    | `Data data ->
      begin
        let rex = Pcre.regexp {|\[\[([^[{]+)\]\]|} in
        try
          let (substrings: Pcre.substrings ref) = ref (Pcre.exec ~rex data) in
          while true do
            let topic = to_topic substrings in
            Printf.printf "%s\n" topic;
            substrings := Pcre.next_match ~rex !substrings;
          done;
        with Not_found ->
          Printf.printf "No links matched!\n%!"
      end
    | _ -> assert false
  in
  ()

let find_links i =
  let rec process i depth =
    match Xmlm.input i with
    | `El_start ((_namespace, tag), _attributes) when tag = "text" ->
      examine_text i;
      process i (depth + 1)
    | `El_start _ -> process i (depth + 1)
    | `El_end -> if depth = 0 then () else process i (depth - 1)
    | `Data _ -> process i depth
    | `Dtd _ -> assert false
  in
  let _dtd = Xmlm.input i in
  let _root = Xmlm.input i in
  process i 0

let run_with_string s =
  let i = Xmlm.make_input (`String (0, s)) in
  find_links i;
  ()

let run filename =
  let ic = In_channel.open_bin filename in
  let i = Xmlm.make_input (`Channel ic) in
  find_links i;
  In_channel.close ic;
  ()