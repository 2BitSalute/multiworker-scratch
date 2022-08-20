type catalog_error = {
  topic: string;
  offset: int64;
  message: string;
  debug_detail: string;
}

module type CATALOG = sig
  val get_article : string -> int64 -> (Demo_xmlm.article list, catalog_error) result
  val collect_offset_cache_hits : unit -> int
end

module TestCatalog = struct
  let (parsed_articles: (string, Demo_xmlm.article) Hashtbl.t) = Hashtbl.create 10

  let add topic deps =
    let article = Demo_xmlm.{
        topic;
        hash = Murmur3.hash64 topic;
        resolved_deps = [];
        deps;
      }
    in
    Hashtbl.add parsed_articles topic article

  let get_article topic offset : Demo_xmlm.article list =
    ignore offset;
    [ Hashtbl.find parsed_articles topic ]

  let collect_offset_cache_hits () = 0
end

module type OFFSET_CACHE = sig
  val add : int64 -> unit
  val mem : int64 -> bool
end

module HashTableOffsetCache : OFFSET_CACHE = struct
  module I64S = Demo_xmlm.Int64Set
  let seen_offsets = ref I64S.empty
  let add offset = seen_offsets := I64S.add offset !seen_offsets
  let mem offset = I64S.mem offset !seen_offsets
end

module type PARSED_ARTICLE_CACHE = sig
  val add : string -> Demo_xmlm.article -> unit
  val get : string -> Demo_xmlm.article list option
end

module HashTableParsedArticleCache : PARSED_ARTICLE_CACHE = struct
  let (cache: (string, Demo_xmlm.article list) Hashtbl.t) = Hashtbl.create 1000

  let add topic article =
    let articles = match Hashtbl.find_opt cache topic with
      | None -> []
      | Some articles -> articles
    in
    Hashtbl.add cache topic (article :: articles)

  let get topic =
    Hashtbl.find_opt cache topic
end

module type LOGGER = sig
  val error : ('a, unit, string, unit) format4 -> 'a
  val verbose : ('a, unit, string, unit) format4 -> 'a
end

module NullLogger : LOGGER = struct
  let eat fmt =
    let print_raw s =
      ignore (Printf.sprintf "%s\n%!" s)
    in
    Printf.ksprintf print_raw fmt

  let error = eat
  let verbose = eat
end

module StdIoLogger = struct
  let error fmt =
    let print_raw s =
      Printf.eprintf "%s\n%!" s
    in
    Printf.ksprintf print_raw fmt

  let verbose fmt =
    let print_raw s =
      Printf.printf "%s\n%!" s
    in
    Printf.ksprintf print_raw fmt
end

module MakeXmlCatalog
    (OffsetCache: OFFSET_CACHE)
    (ParsedArticleCache: PARSED_ARTICLE_CACHE)
    (Logger: LOGGER) : CATALOG = struct
  let offset_cache_hits = ref 0

  let collect_offset_cache_hits () =
    let result = !offset_cache_hits in
    offset_cache_hits := 0;
    result

  let parse_with_retry buffer : Demo_xmlm.article list =
    try
      Demo_xmlm.run_with_string ~log:false (Buffer.contents buffer)
    with _ ->
      Demo_xmlm.run_with_string ~log:true (Buffer.contents buffer)

  let data_error topic offset e =
    let message =
      Printf.sprintf "Data corruption error reading topic `%s` at offset %s" topic (Int64.to_string offset)
    in
    Logger.error "%s" message;
    Error {
      topic;
      offset;
      message;
      debug_detail = Printexc.to_string e;
    }

  let matching_error topic offset =
    let message =
      Printf.sprintf "No parsed articles under topic '%s' at offset %s" topic (Int64.to_string offset)
    in
    Logger.error "%s" message;
    Error {
      topic;
      offset;
      message;
      debug_detail = ""; (* TODO: get backtrace *)
    }

  let read_catalog_at_offset topic offset =
    Logger.verbose "Parsing articles at offset %s\n" (Int64.to_string offset);
    try
      let buffer = Demo_bz2.read_catalog_at_offset
          ("../wikipedia/" ^ "enwiki-20211020-pages-articles-multistream.xml.bz2")
          offset
      in
      Ok buffer
    with Bz2.Data_error as e ->
      data_error topic offset e

  (* Cache articles and return the ones that match the topic *)
  let match_article (topic: string) (found: Demo_xmlm.article list) (article: Demo_xmlm.article) =
    let open Demo_xmlm in
    ParsedArticleCache.add article.topic article;
    if (String.lowercase_ascii topic) = (String.lowercase_ascii article.topic) then
      begin
        (* TODO: should look up first by the exact casing, then by lowercase/normalized *)
        Logger.verbose "%s == %s\n" topic article.topic;
        article :: found
      end
    else
      begin
        Logger.verbose "%s <> %s\n" topic article.topic;
        found
      end

  let get_article topic offset : (Demo_xmlm.article list, catalog_error) result =
    Logger.verbose "Look up topic %s at offset %s\n%!" topic (Int64.to_string offset);

    if not (OffsetCache.mem offset) then
      begin
        OffsetCache.add offset;
        match read_catalog_at_offset topic offset with
        | Ok buffer ->
          begin
            let (articles_at_offset: Demo_xmlm.article list) = parse_with_retry buffer in
            let articles = List.fold_left (match_article topic) [] articles_at_offset in
            match articles with
            | [] -> matching_error topic offset
            | articles -> Ok articles
          end
        | Error _ as e -> e
      end
    else
      begin
        offset_cache_hits := !offset_cache_hits + 1;
        let result = match ParsedArticleCache.get topic with
          | None ->
            matching_error topic offset
          | Some articles -> Ok articles
        in
        result
      end
end

module HashTableXmlCatalog = MakeXmlCatalog (HashTableOffsetCache) (HashTableParsedArticleCache) (NullLogger)