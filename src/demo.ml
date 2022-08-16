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

let skipped_prefixes = [
  "image:";
  "file:";
  ":file:";
  "commons:";
  ":template:";
  "wikipedia:";
  "wikt:";
  "wp:";
]

module type ARTICLE_CACHE = sig
  val mem : string -> bool
end

module SharedMemArticleCache : ARTICLE_CACHE = struct
  let mem _topic = failwith "TODO: Not imlemented"
end

module HashTableArticleCache : ARTICLE_CACHE = struct
  let cache = Hashtbl.create 1000

  let mem = Hashtbl.mem cache
end

module ShallowChecker
    (Index : Reverse_index.REVERSE_INDEX)
    (Catalog : Demo_catalog.CATALOG)
    (ArticleCache : ARTICLE_CACHE) = struct

  type error = 
    | Catalog_error of string * Demo_catalog.catalog_error
    | Index_error of string * Reverse_index.retrieval_error

  type result = {
    unchecked: Reverse_index.retrieval_entry list;
    errors: error list;
  }

  let empty_result = {
    unchecked = [];
    errors = [];
  }

  let get_unresolved_deps deps : string list =
    List.fold_left
      (fun unresolved dep -> if ArticleCache.mem dep then unresolved else dep :: unresolved)
      []
      deps

  let get_index_entries name unresolved : result =
    List.fold_left
      (fun result dep ->
         match Index.get_entries dep with
         | Error e -> { result with errors = Index_error (name, e) :: result.errors }
         | Ok index_entries -> { result with unchecked = List.rev_append index_entries result.unchecked }
      )
      empty_result
      unresolved

  let check_indexed_article (acc: result) (index_entry: Reverse_index.retrieval_entry) =
    let name = index_entry.Reverse_index.entry.name in
    let offset = index_entry.Reverse_index.entry.offset in
    match Catalog.get_article name offset with
    | Error e ->
      (* Hash collision or some other error - the article is not actually in the stream, or the stream is corrupted *)
      {
        acc with
        errors = Catalog_error (name, e) :: acc.errors
      }
    | Ok articles ->
      let loop acc article =
        (* Filter to the topics that aren't in the cache *)
        let unresolved = get_unresolved_deps article.Demo_xmlm.deps in
        let result = get_index_entries name unresolved in
        {
          errors = List.rev_append result.errors acc.errors;
          unchecked = List.rev_append result.unchecked acc.unchecked
        }
      in
      List.fold_left loop acc articles

  let check_article_deps acc topic =
    match Index.get_entries topic with
    | Error e ->
      { acc with errors = Index_error (topic, e) :: acc.errors }
    | Ok index_entries ->
      List.fold_left check_indexed_article acc index_entries

  let check_article_deps topics : result =
    (* TODO: this function should:
       1. retrieve the topic from the index
       2. retrieve topic from catalog
       3. retrieve from index its dependencies *)
    (* It should produce errors as well as the unresolved dependencies in the form of index records *)
    (* It should also store checked articles in the article cache *)
    List.fold_left check_article_deps empty_result topics
end

module TransitiveChecker (Index : Reverse_index.REVERSE_INDEX) (Catalog : Demo_catalog.CATALOG) = struct
  let hashed_articles = Hashtbl.create 1000

  let get_deps deps : (int64 list * string list) =
    List.fold_left
      (fun (resolved, unresolved) dep ->
         match Hashtbl.find_opt hashed_articles dep with
         | Some hash -> (hash :: resolved, unresolved)
         | None -> (resolved, dep :: unresolved))
      ([], [])
      deps

  let compute_article_hash (hash: int64) deps =
    List.fold_left
      (fun hash (dep: int64) -> Int64.logxor hash dep)
      hash
      deps

  let check_indexed_article acc (index_entry: Reverse_index.retrieval_entry) : article_result list =
    let name = index_entry.Reverse_index.entry.name in
    let offset = index_entry.Reverse_index.entry.offset in
    match Catalog.get_article name offset with
    | Error _ ->
      (* Hash collision or some other error - the article is not actually in the stream, or the stream is corrupted *)
      acc
    | Ok articles ->
      let f article =
        (* Filter to the topics that aren't in the cache *)
        let (resolved, unresolved) = get_deps article.Demo_xmlm.deps in
        (* If all the topics are in the cache, then get their hashes, compute *)
        if List.length unresolved = 0 then
          let article_hash = compute_article_hash article.Demo_xmlm.hash resolved in
          Hashtbl.add hashed_articles article.Demo_xmlm.topic article_hash;
          Hash article_hash
        else
          Deps (article.Demo_xmlm.hash, unresolved)
      in
      List.rev_append acc (List.map f articles)

  let check_article topic =
    (* Printf.printf "Checking topic %s\n%!" topic; *)
    (* Get topic offsets *)
    match Index.get_entries topic with
    | Error _ ->
      (* Printf.printf "%s not found!\n" topic; *)
      [ NotFound topic ]
    | Ok index_entries -> (* TODO: how to resolve duplicates? *)
      (* Printf.printf "Found %d matches for topic '%s'\n" (List.length index_entries) topic; *)
      List.fold_left check_indexed_article [] index_entries

  let rec check_article_transitively stack topic : int64 list =
    let process_result acc result : int64 list =
      match result with
      | Hash hash -> hash :: acc
      | NotFound topic ->
        let topic = String.lowercase_ascii topic in
        if List.exists (fun prefix -> String.starts_with topic ~prefix) skipped_prefixes then
          acc
        else
          begin
            Printf.printf "Did not find %s\n%!" topic;
            []
          end
      | Deps (hash, deps) ->
        let deps = List.filter
            (fun dep -> if List.mem dep stack || String.length dep == 0 then false else true)
            deps
        in
        (* This call causes the stack to grow to an arbitrary depth; rewrite *)
        let resolved = List.map (check_article_transitively (topic :: stack)) deps in
        compute_article_hash hash (List.flatten resolved) :: acc
    in
    match check_article topic with
    | [] -> failwith (Printf.sprintf "This should not happen; no results for %s?\n" topic)
    | results ->
      List.fold_left process_result [] results

  let check_article_transitively topic : int64 list =
    check_article_transitively [] topic
end

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
        Demo_xmlm.run_with_string ~log:false (Buffer.contents buffer)
      in
      List.iter
        Demo_xmlm.(fun article ->
            Printf.printf "%s %s %d\n"
              article.topic
              (Int64.to_string article.hash)
              (List.length article.deps))
        articles;
    end;

  (* Get all transitive dependencies' hashes *)

  let module TC = Demo_catalog.TestCatalog in

  TC.add "Capital city" [ "Capital City" ];
  TC.add "Capital City" [ "Capital city" ];

  let module TI = Reverse_index.TestIndex in

  TI.add "Capital city";
  TI.add "Capital City";

  let query_db_cache = Reverse_index.QueryDbCache.make db_name in
  Reverse_index.SqliteIndex.init query_db_cache;

  let module TransitiveChecker = TransitiveChecker (Reverse_index.SqliteIndex) (Demo_catalog.XmlCatalog) in

  if should_check_article then
    begin
      match TransitiveChecker.check_article_transitively "continent" with
      | [] -> failwith "This should not happen; no results?"
      | hashes -> List.iter (fun hash -> Printf.printf "Final hash is %s\n" (Int64.to_string hash)) hashes
    end;

  Printf.printf "*** DONE: %d ***\n\n%!" (List.length c)