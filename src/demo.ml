(*
 * Copyright (c) Tatiana Racheva
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Demo_setup

module Daemon = MultiWorker.MultiThreadedCall.WorkerController.Worker.Daemon
module SharedMem = MultiWorker.MultiThreadedCall.WorkerController.SharedMem
module WorkerController = MultiWorker.MultiThreadedCall.WorkerController

(* Data model for Wikipedia jobs *)

type article_result =
  | Hash of int64
  | Deps of int64 * string list
  | NotFound of string

module Global_state = struct
  type state = {
    dummy: int
  }

  let worker_id_str ~(worker_id : int) =
    if worker_id = 0 then
      "demo master"
    else
      Printf.sprintf "demo worker-%d" worker_id

  let restore
      ({
        dummy = _
      } : state)
      ~(worker_id : int) : unit =
    PidLog.log
      (Printf.sprintf
         "%s - Hello from restore"
         (worker_id_str ~worker_id))

  let save ~(trace : bool) : state =
    ignore trace;
    {
      dummy = 144
    }
end

(* This should stay at toplevel in order to be executed before [Daemon.check_entry_point]. *)
let entry =
  WorkerControllerEntryPoint.register ~restore:Global_state.restore

let catch_and_classify_exceptions : 'x 'b. ('x -> 'b) -> 'x -> 'b =
  fun f x ->
  try f x with
  | Not_found ->
    Exit.exit Exit_status.Worker_not_found_exception

let init_state () = Global_state.save ~trace:true

let init () =
  let nbr_procs =
    (* MacBook Air 2017 *)
    2
    (* Sys_utils.nbr_procs *)
  in
  (* 2 is too few. I need to buy a new MacBook Air *)
  let num_workers = nbr_procs * 5 in

  let heap_handle = SharedMem.init ~num_workers SharedMem.default_config in
  let gc_control = Gc.get () in
  let state = init_state () in
  let workers =
    MultiWorker.make
      ~call_wrapper:{ WorkerController.wrap = catch_and_classify_exceptions }
      ~longlived_workers:false
      ~saved_state:state
      ~entry
      nbr_procs
      ~gc_control
      ~heap_handle
  in
  workers

let next_list =
  let rec aux acc n =
    if n >= 0 then
      aux (n :: acc) (n - 1)
    else
      acc
  in
  aux [] 200000

let () =
  (* Required to prevent the spawned subprocesses from executing this function again.
     If you forget to check entry point here, the program is going to enter an infinite
     loop, spawning more and more subprocesses until the OS process limit is hit.
     At that point, you will begin seeing failures having to do with being unable to load
     libraries (e.g., zstdlib), or some other unrelated-looking errors.
     This MatLab discussion contains a clue:
     https://www.mathworks.com/matlabcentral/answers/341979-matlab-r2017a-9-2-0-538062-crashes-when-calling-functions-from-the-parallel-computing-toolbox-mac#answer_268898
     "The error may be a result of the ulimit settings being too low."

     I can't tell you how many times I got caught by this.
     It's a design flaw in the approach.
  *)
  (* PidLog.enable (); *)
  Daemon.check_entry_point ();
  Exception.record_backtrace true;

  let workers = init () in
  let workers = Some workers in
  let job (c: int list) (a: int list) =
    (* Is c the accumulator (seems to be inited by neutral), and a the input? *)
    ignore (c, a);
    PidLog.log (Printf.sprintf "Job! %d %d\n" (List.length c) (List.hd a));
    List.fold_left (fun acc el -> acc + el) 0 a;
  in
  let merge (b: int) (acc: int list) : int list =
    ignore b;
    PidLog.log (Printf.sprintf "Merge: %d %d\n" b (List.length acc));
    b :: acc
  in

  let (c: int list) = MultiWorker.call
      workers
      ~job
      ~neutral:[1; 2; 3; 4]
      ~merge
      ~next:(MultiWorker.next workers next_list)
  in

  Demo_domains.run ();
  (* Index file:
     enwiki-20211020-pages-articles-multistream-index.txt.bz2
     Multistream:
     enwiki-20211020-pages-articles-multistream.xml.bz2

     The index format is:
     offset:article_id:title

     The offset is the number of bytes into the COMPRESSED stream, NOT the number of bytes to skip reading using BZ2 reader.
  *)

  (* Demo_bz2.index ("../wikipedia/" ^ "enwiki-20211020-pages-articles-multistream-index.txt.bz2"); *)

  let db_name = "wikipedia_reverse_index" in

  let should_generate_reverse_index = false in
  let should_check_article = true in

  if should_generate_reverse_index then
    begin
      let next_seq =
        Demo_bz2.index2
          ("../wikipedia/" ^ "enwiki-20211020-pages-articles-multistream-index.txt.bz2")
          ~n_pages:(-1)
      in
      let files = Sys.readdir "." |> Array.to_list in
      List.iter (fun file -> if String.starts_with ~prefix:db_name file then Sys.remove file) files;

      let _result = Reverse_index.save_names db_name next_seq in
      ()
    end;

  if false then
    begin
      let buffer =
        Demo_bz2.read_catalog_at_offset
          ("../wikipedia/" ^ "enwiki-20211020-pages-articles-multistream.xml.bz2")
          597L
      in
      let articles =
        Demo_xmlm.run_with_string (Buffer.contents buffer)
      in
      List.iter
        Demo_xmlm.(fun article ->
            Printf.printf "%s %s %d\n"
              article.topic
              (Int64.to_string article.hash)
              (List.length article.unresolved_deps))
        articles;
    end;

  let query_db_cache = Reverse_index.QueryDbCache.make db_name in

  begin
    match Reverse_index.get query_db_cache "Andorra" with
    | [] ->
      Printf.printf "Not found!\n";
    | entries  ->
      List.iter
        Reverse_index.(fun { entry = { name; id; offset; }; hash; canon_hash } ->
            Printf.printf
              "Name: %s, ID: %s, Offset: %s Hash: %s Canonical hash: %s\n"
              name
              (Int64.to_string id)
              (Int64.to_string offset)
              (Int64.to_string hash)
              (Int64.to_string canon_hash))
        entries
  end;

  (* Get all transitive dependencies' hashes *)
  (* TODO: How to resolve circular dependencies? Maybe if a circular dependency is encountered, the
     dependency is broken based on article ID (only a dep if ID < current) *)

  (* Create a hash cache, name->hash *)
  let hashed_articles = Hashtbl.create 1000 in
  let (parsed_articles: (string, Demo_xmlm.article list) Hashtbl.t) = Hashtbl.create 1000 in
  let module I64S = Demo_xmlm.Int64Set in
  let seen_offsets = I64S.empty in

  let add_parsed_article topic article =
    let articles = match Hashtbl.find_opt parsed_articles topic with
      | None -> []
      | Some articles -> articles
    in
    Hashtbl.add parsed_articles topic (article :: articles)
  in

  let get_article_from_catalog topic offset : Demo_xmlm.article list =
    Printf.printf "Look up topic %s at offset %s\n%!" topic (Int64.to_string offset);
    (* Look up article *)
    if not (I64S.mem offset seen_offsets) then
      begin
        Printf.printf "Parsing articles at offset %s\n" (Int64.to_string offset);
        let buffer =
          Demo_bz2.read_catalog_at_offset
            ("../wikipedia/" ^ "enwiki-20211020-pages-articles-multistream.xml.bz2")
            offset
        in
        let (articles_at_offset: Demo_xmlm.article list) =
          Demo_xmlm.run_with_string (Buffer.contents buffer)
        in
        (* Cache articles and return the ones that match the topic *)
        List.fold_left
          Demo_xmlm.(fun (found: Demo_xmlm.article list) (article: Demo_xmlm.article) ->
              add_parsed_article article.topic article;
              if (String.lowercase_ascii topic) = (String.lowercase_ascii article.topic) then
                begin
                  (* TODO: should look up first by the exact casing, then by lowercase/normalized *)
                  Printf.printf "%s == %s\n" topic article.topic;
                  article :: found
                end
              else found)
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
  in

  let get_deps deps : (int64 list * string list) =
    List.fold_left
      (fun (resolved, unresolved) dep ->
         match Hashtbl.find_opt hashed_articles dep with
         | Some hash -> (hash :: resolved, unresolved)
         | None -> (resolved, dep :: unresolved))
      ([], [])
      deps
  in

  let compute_article_hash (hash: int64) deps =
    List.fold_left
      (fun hash (dep: int64) -> Int64.logxor hash dep)
      hash
      deps
  in

  let check_indexed_article acc (index_entry: Reverse_index.retrieval_entry) =
    let name = index_entry.Reverse_index.entry.name in
    let offset = index_entry.Reverse_index.entry.offset in
    match get_article_from_catalog name offset with
    | [] ->
      (* Hash collision - the article is not actually in the stream *)
      Printf.printf "Hash collision\n";
      acc
    | articles ->
      let f article =
        (* Filter to the topics that aren't in the cache *)
        let (resolved, unresolved) = get_deps article.Demo_xmlm.unresolved_deps in
        (* If all the topics are in the cache, then get their hashes, compute *)
        if List.length unresolved = 0 then
          let article_hash = compute_article_hash article.Demo_xmlm.hash resolved in
          Hashtbl.add hashed_articles article.Demo_xmlm.topic article_hash;
          Hash article_hash
        else
          Deps (article.Demo_xmlm.hash, unresolved)
      in
      List.rev_append acc (List.map f articles)
  in

  let check_article topic : article_result list =
    (* Get topic offsets *)
    match Reverse_index.get query_db_cache topic with
    | [] ->
      Printf.printf "%s not found!\n" topic;
      [ NotFound topic ]
    | index_entries -> (* TODO: how to resolve duplicates? *)
      Printf.printf "Found %d matches for topic '%s'\n" (List.length index_entries) topic;
      List.fold_left check_indexed_article [] index_entries
  in

  let rec check_article_transitively topic : int64 list =
    let process_result acc result =
      match result with
      | Hash hash -> hash :: acc
      | NotFound topic ->
        if String.starts_with topic ~prefix:"Image:" then acc
        else failwith (Printf.sprintf "Did not find %s\n" topic)
      | Deps (hash, deps) ->
        let resolved = List.map check_article_transitively deps in
        compute_article_hash hash (List.flatten resolved) :: acc
    in
    match check_article topic with
    | [] -> failwith (Printf.sprintf "This should not happen; no results for %s?\n" topic)
    | results -> List.fold_left process_result [] results
  in

  if should_check_article then
    begin
      match check_article_transitively "Capital city" with
      | [] -> failwith "This should not happen; no results?"
      | hashes -> List.iter (fun hash -> Printf.printf "Final hash is %s\n" (Int64.to_string hash)) hashes
    end;

  Printf.printf "*** DONE: %d ***\n\n%!" (List.length c)