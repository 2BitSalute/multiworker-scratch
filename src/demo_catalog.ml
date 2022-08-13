module type CATALOG = sig
  val get_article : string -> int64 -> Demo_xmlm.article list
end

module TestCatalog = struct
  let (parsed_articles: (string, Demo_xmlm.article) Hashtbl.t) = Hashtbl.create 10

  let add topic deps =
    let article = Demo_xmlm.{
        topic;
        hash = Murmur3.hash64 topic;
        resolved_deps = [];
        unresolved_deps = deps;
      }
    in
    Hashtbl.add parsed_articles topic article

  let get_article topic offset : Demo_xmlm.article list =
    ignore offset;
    [ Hashtbl.find parsed_articles topic ]
end

module XmlCatalog : CATALOG = struct
  module I64S = Demo_xmlm.Int64Set

  let seen_offsets = I64S.empty
  let (parsed_articles: (string, Demo_xmlm.article list) Hashtbl.t) = Hashtbl.create 1000

  let add_parsed_article topic article =
    let articles = match Hashtbl.find_opt parsed_articles topic with
      | None -> []
      | Some articles -> articles
    in
    Hashtbl.add parsed_articles topic (article :: articles)

  let parse_with_retry buffer : Demo_xmlm.article list =
    try
      Demo_xmlm.run_with_string ~log:false (Buffer.contents buffer)
    with _ ->
      Demo_xmlm.run_with_string ~log:true (Buffer.contents buffer)

  let read_catalog_at_offset topic offset =
    (* Printf.printf "Parsing articles at offset %s\n" (Int64.to_string offset); *)
    try
      Demo_bz2.read_catalog_at_offset
        ("../wikipedia/" ^ "enwiki-20211020-pages-articles-multistream.xml.bz2")
        offset
    with Bz2.Data_error as e ->
      Printf.eprintf "Data error reading topic `%s` at offset %s\n%!" topic (Int64.to_string offset);
      raise e

  let bad_offsets = [ 1960883272L; 439999741L ]

  let get_article topic offset : Demo_xmlm.article list =
    (* Printf.printf "Look up topic %s at offset %s\n%!" topic (Int64.to_string offset); *)
    (* Look up article *)
    if List.mem offset bad_offsets then
      begin
        Printf.printf "Tried looking up `%s` at bad offset %s\n%!" topic (Int64.to_string offset);
        []
      end
    else if not (I64S.mem offset seen_offsets) then
      begin
        let buffer = read_catalog_at_offset topic offset in
        let (articles_at_offset: Demo_xmlm.article list) = parse_with_retry buffer in
        (* Printf.printf "Found %d articles at offset %s\n%!" (List.length articles_at_offset) (Int64.to_string offset); *)
        (* Cache articles and return the ones that match the topic *)
        List.fold_left
          Demo_xmlm.(fun (found: Demo_xmlm.article list) (article: Demo_xmlm.article) ->
              add_parsed_article article.topic article;
              if (String.lowercase_ascii topic) = (String.lowercase_ascii article.topic) then
                begin
                  (* TODO: should look up first by the exact casing, then by lowercase/normalized *)
                  (* Printf.printf "%s == %s\n" topic article.topic; *)
                  article :: found
                end
              else
                begin
                  (* Printf.printf "%s <> %s\n" topic article.topic; *)
                  found
                end)
          []
          articles_at_offset
      end
    else
      begin
        match Hashtbl.find_opt parsed_articles topic with
        | None ->
          failwith (Printf.sprintf "No parsed articles under topic '%s' at offset %s\n" topic (Int64.to_string offset));
        | Some articles -> articles
      end
end