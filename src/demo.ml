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

module WorkItem = struct
  type t =
    | Topic of string
    | IndexEntry of Reverse_index.retrieval_entry

  let to_string: t -> string = function
    | Topic s -> s
    | IndexEntry entry -> entry.entry.name

  let compare item1 item2 : int =
    match (item1, item2) with
    | (Topic topic1, Topic topic2) ->
      String.compare topic1 topic2
    | (IndexEntry { entry = { name = topic2; _ }; _ }, Topic topic1)
    | (Topic topic1, IndexEntry { entry = { name = topic2; _ }; _ }) ->
      let comparison = String.compare topic1 topic2 in
      (* If names are the same, then always consider the Topic to be greater than the IndexEntry *)
      if comparison = 0 then 1 else comparison
    | (IndexEntry { entry = { name = topic1; id = id1; offset = offset1 }; _ },
       IndexEntry { entry = { name = topic2; id = id2; offset = offset2 }; _ }) ->
      let comparison = String.compare topic1 topic2 in
      let comparison = if comparison = 0 then Int64.compare id1 id2 else comparison in
      let comparison = if comparison = 0 then Int64.compare offset1 offset2 else comparison in
      comparison
end

module WorkItemSet = Set.Make(WorkItem)

type telemetry = {
  index_duration: float;
  catalog_duration: float;
  catalog_cache_hits: int;
  cache_duration: float;
  cache_hits: int;
  cache_misses: int;
  other_duration: float;
}

let empty_telemetry = {
  index_duration = 0.0;
  catalog_duration = 0.0;
  catalog_cache_hits = 0;
  cache_duration = 0.0;
  cache_hits = 0;
  cache_misses = 0;
  other_duration = 0.0;
}

let db_name = "wikipedia_reverse_index"

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
  val add : string -> (Reverse_index.retrieval_entry list, Reverse_index.retrieval_error) result -> unit
end

module StringKey = struct
  type t = string
  let to_string t = t
  let compare = String.compare
end
module StringKeyHasher = SM.MakeKeyHasher (StringKey)

module SharedMemArticleCache : ARTICLE_CACHE = struct
  module Value = struct
    type t = (Reverse_index.retrieval_entry list, Reverse_index.retrieval_error) result
    let description = "Index retrieval result"
  end
  module SharedMemHashTable = Demo_setup.SharedMemHashTable (StringKeyHasher) (Value)

  let mem (topic: string) : bool =
    SharedMemHashTable.mem (StringKeyHasher.hash topic)

  let add
      (topic: string)
      (result: (Reverse_index.retrieval_entry list, Reverse_index.retrieval_error) result)
    : unit =
    SharedMemHashTable.add (StringKeyHasher.hash topic) result
end

module HashTableArticleCache : ARTICLE_CACHE = struct
  let cache = Hashtbl.create 1000
  let mem = Hashtbl.mem cache
  let add = Hashtbl.add cache
end

module MakeShallowChecker
    (Index : Reverse_index.REVERSE_INDEX)
    (Catalog : Demo_catalog.CATALOG)
    (ArticleCache : ARTICLE_CACHE) = struct

  type error =
    | Catalog_error of string * Demo_catalog.catalog_error
    | Index_error of string * Reverse_index.retrieval_error

  type result = {
    completed: WorkItem.t list;
    unchecked: WorkItem.t list;
    errors: error list;
    telemetry: telemetry;
  }

  let index_time = ref 0.0
  let catalog_time = ref 0.0
  let cache_time = ref 0.0
  let hits = ref 0
  let misses = ref 0

  let collect_telemetry ~total =
    let other_duration = total -. !index_time -. !catalog_time -. !cache_time in
    let telemetry = {
      index_duration = !index_time;
      catalog_duration = !catalog_time;
      catalog_cache_hits = Catalog.collect_offset_cache_hits ();
      cache_duration = !cache_time;
      cache_hits = !hits;
      cache_misses = !misses;
      other_duration;
    }
    in
    index_time := 0.0;
    catalog_time := 0.0;
    cache_time := 0.0;
    hits := 0;
    misses := 0;
    telemetry

  let cache_time t =
    cache_time := !cache_time +. t

  let catalog_time t =
    catalog_time := !catalog_time +. t

  let index_time t =
    index_time := !index_time +. t

  let hits () = hits := !hits + 1
  let misses () = misses := !misses + 1

  let empty_result = {
    completed = [];
    unchecked = [];
    errors = [];
    telemetry = empty_telemetry;
  }

  let get_unresolved_topics deps : string list =
    let t = Unix.gettimeofday () in
    let unresolved =
      List.fold_left
        (fun unresolved dep ->
           if ArticleCache.mem dep then begin
             hits ();
             unresolved
           end
           else begin
             misses ();
             dep :: unresolved
           end)
        []
        deps
    in
    cache_time (Unix.gettimeofday () -. t);
    unresolved

  let get_index_entries name unresolved : result =
    Printexc.record_backtrace true;
    List.fold_left
      (fun result dep ->
         let index_result = Index.get_entries dep in
         ArticleCache.add dep index_result;
         match index_result with
         | Error e -> { result with errors = Index_error (name, e) :: result.errors }
         | Ok index_entries -> {
             result with
             unchecked = List.rev_append
                 (List.map (fun index_entry -> WorkItem.IndexEntry index_entry) index_entries)
                 result.unchecked
           }
      )
      empty_result
      unresolved

  let get_index_entries name unresolved =
    let t = Unix.gettimeofday () in
    let result = (get_index_entries name unresolved) in
    index_time (Unix.gettimeofday () -. t);
    result

  let get_valid_topics unresolved : string list =
    List.filter
      (fun topic ->
         (* Topics with colons include special page links such as `Category:...` , `de:...`, `iarchive:...`, etc.s *)
         not (String.contains topic ':'))
      unresolved

  let get_article name offset =
    let t = Unix.gettimeofday () in
    let result = (Catalog.get_article name offset) in
    catalog_time (Unix.gettimeofday () -. t);
    result

  let check_indexed_article (acc: result) (index_entry: Reverse_index.retrieval_entry) =
    (* let t = Unix.gettimeofday () in *)
    let name = index_entry.Reverse_index.entry.name in
    let offset = index_entry.Reverse_index.entry.offset in
    match get_article name offset with
    | Error e ->
      (* Hash collision or some other error - the article is not actually in the stream, or the stream is corrupted *)
      {
        acc with
        errors = Catalog_error (name, e) :: acc.errors
      }
    | Ok articles ->
      let loop acc article =
        let unresolved =
          let unresolved = get_unresolved_topics article.Demo_xmlm.deps in
          get_valid_topics unresolved
        in
        let result = get_index_entries name unresolved in
        let result = {
          result with
          errors = List.rev_append result.errors acc.errors;
          unchecked = List.rev_append result.unchecked acc.unchecked;
        }
        in
        result
      in
      List.fold_left loop acc articles

  let check_article_deps acc work_item =
    match work_item with
    | WorkItem.Topic topic ->
      begin
        let t = Unix.gettimeofday () in
        let retrieval_result = Index.get_entries topic in
        index_time (Unix.gettimeofday () -. t);
        let t = Unix.gettimeofday () in
        ArticleCache.add topic retrieval_result;
        cache_time (Unix.gettimeofday () -. t);
        match retrieval_result with
        | Error e ->
          { acc with errors = Index_error (topic, e) :: acc.errors }
        | Ok index_entries ->
          List.fold_left check_indexed_article acc index_entries
      end
    | WorkItem.IndexEntry entry ->
      check_indexed_article acc entry

  let check_article_deps work_items : result =
    (* This function does:
       1. retrieve the topic from the index
       2. retrieve topic from catalog
       3. retrieve from index its dependencies *)
    (* It should produce errors as well as the unresolved dependencies in the form of index records *)
    (* It should also store checked articles in the article cache *)
    let t = Unix.gettimeofday () in
    let result = List.fold_left check_article_deps empty_result work_items in
    (* The article on `Bolivia` has 1318945 topics! *)
    if List.length result.unchecked > 10_000 then begin
      List.iter
        (fun work_item ->
           Printf.printf "Item %s\n%!" (WorkItem.to_string work_item))
        work_items;
    end;
    let telemetry = collect_telemetry ~total:(Unix.gettimeofday () -. t) in
    {
      result with
      completed = work_items;
      telemetry;
    }
end

module SharedMemOffsetCache : Demo_catalog.OFFSET_CACHE = struct
  module Key = struct
    type t = int64
    let to_string t = Int64.to_string t
    let compare = Int64.compare
  end
  module Value = struct
    type t = bool
    let description = "Offset result"
  end
  module KeyHasher = SM.MakeKeyHasher (Key)
  module SharedMemHashTable = Demo_setup.SharedMemHashTable (KeyHasher) (Value)

  let add (offset: int64) =
    SharedMemHashTable.add (KeyHasher.hash offset) true

  let mem (offset: int64) =
    SharedMemHashTable.mem (KeyHasher.hash offset)
end

module SharedMemParsedArticleCache : Demo_catalog.PARSED_ARTICLE_CACHE = struct
  module Value = struct
    type t = Demo_xmlm.article list
    let description = "Parsed articles"
  end

  module SharedMemHashTable = Demo_setup.SharedMemHashTable (StringKeyHasher) (Value)

  let get (topic: string) : Demo_xmlm.article list option =
    SharedMemHashTable.get (StringKeyHasher.hash ("parsed" ^ topic))

  let add
      (topic: string)
      (article: Demo_xmlm.article)
    : unit =
    begin
      match get topic with
      | Some articles -> article :: articles
      | None -> [ article ]
    end
    |> SharedMemHashTable.add (StringKeyHasher.hash ("parsed" ^ topic))
end

module SharedMemXmlCatalog = Demo_catalog.MakeXmlCatalog
    (SharedMemOffsetCache)
    (SharedMemParsedArticleCache)
    (Demo_catalog.NullLogger)

module SerialShallowChecker = MakeShallowChecker
    (Reverse_index.SqliteIndex)
    (Demo_catalog.HashTableXmlCatalog)
    (SharedMemArticleCache)

module ParallerShallowChecker = MakeShallowChecker
    (Reverse_index.SqliteIndex)
    (* (Demo_catalog.HashTableXmlCatalog) *)
    (SharedMemXmlCatalog)
    (SharedMemArticleCache)

(* The [job] lambda is marshalled, sent to the worker process, unmarshalled there, and executed.
     It is marshalled immediately before being executed. *)
let job
    _
    (input : WorkItem.t list)
  : ParallerShallowChecker.result =
  ParallerShallowChecker.check_article_deps input

let rec split_n acc l n =
  match (n, l) with
  | 0, _ | _, [] -> (acc, l)
  | n, head :: rest ->
    split_n (head :: acc) rest (n - 1)

let split_n l n = split_n [] l n

let merge
    ~(max_work_items: int)
    ~(work_items_count: int ref)
    ~(work_items_to_process : WorkItem.t list ref)
    ~(work_items_in_progress : WorkItemSet.t ref)
    (job_result: ParallerShallowChecker.result)
    (acc_errors, acc_telemetry)
  : ParallerShallowChecker.error list * telemetry =
  work_items_in_progress :=
    List.fold_left
      (fun acc item -> WorkItemSet.remove item acc)
      !work_items_in_progress
      job_result.completed;

  (* Printf.printf
     "MERGE: total work items count: %d errors: %d completed in job: %d unchecked: %d>>\n%!"
     !work_items_count
     (List.length job_result.errors)
     (List.length job_result.completed)
     (List.length job_result.unchecked); *)

  let num_new_to_process = if !work_items_count < max_work_items then
      begin
        let num_new_to_process =
          min
            max_work_items
            (!work_items_count + List.length job_result.unchecked)
        in
        work_items_count := !work_items_count + num_new_to_process;
        num_new_to_process
      end
    else 0
  in
  (* Printf.printf "Num New To Process %d\n%!" num_new_to_process; *)
  let (new_to_process, _) =
    split_n job_result.unchecked num_new_to_process
  in
  work_items_to_process := List.rev_append new_to_process !work_items_to_process;

  let acc_errors = List.rev_append job_result.errors acc_errors in
  let acc_telemetry = {
    catalog_duration = acc_telemetry.catalog_duration +. job_result.telemetry.catalog_duration;
    catalog_cache_hits = acc_telemetry.catalog_cache_hits + job_result.telemetry.catalog_cache_hits;
    index_duration = acc_telemetry.index_duration +. job_result.telemetry.index_duration;
    cache_duration = acc_telemetry.cache_duration +. job_result.telemetry.cache_duration;
    cache_hits = acc_telemetry.cache_hits + job_result.telemetry.cache_hits;
    cache_misses = acc_telemetry.cache_misses + job_result.telemetry.cache_misses;
    other_duration = acc_telemetry.other_duration +. job_result.telemetry.other_duration;
  }
  in
  (acc_errors, acc_telemetry)

module Bucket = Demo_setup.MultiWorker.MultiThreadedCall.Bucket

let next
    ~(workers : MultiWorker.worker list option)
    ~(work_items_to_process : WorkItem.t list ref)
    ~(work_items_in_progress : WorkItemSet.t ref) =
  ignore workers;
  (* let max_size = Bucket.max_size () in *)
  (* let num_workers =
     match workers with
     | Some w -> List.length w
     | None -> 1
     in *)
  let return_bucket_job ~current_bucket ~remaining_jobs =
    (* Update our shared mutable state, because hey: it's not like we're
       writing OCaml or anything. *)
    (* Printf.printf "work_items_to_process length in return_bucket_job: %d\n%!" (List.length !work_items_to_process);
       Printf.printf
       "NEXT: current_bucket: %d remaining_jobs: %d>>\n%!"
       (List.length current_bucket)
       (List.length remaining_jobs); *)

    work_items_to_process := remaining_jobs;
    work_items_in_progress :=
      List.fold_left
        (fun item acc -> WorkItemSet.add acc item)
        !work_items_in_progress
        current_bucket;
    Bucket.Job current_bucket
  in
  fun () ->
    (* Printf.printf "Num workers: %d\n%!" num_workers; *)
    if (List.length !work_items_to_process) <> 0 then begin
      (* Printf.printf "BUCKET\n%!"; *)
      (* let bucket_size =
         Bucket.calculate_bucket_size
          ~num_jobs:(List.length !work_items_to_process)
          ~num_workers
          ~max_size
         in *)
      let bucket_size = 10 in
      let (current_bucket, remaining_jobs) =
        split_n !work_items_to_process bucket_size
      in
      return_bucket_job ~current_bucket ~remaining_jobs
    end
    else if (WorkItemSet.cardinal !work_items_in_progress) <> 0 then begin
      (* Printf.printf "WAIT\n%!"; *)
      Bucket.Wait
    end
    else
      Bucket.Done

type article_result =
  | Hash of int64
  | Deps of int64 * string list
  | NotFound of string

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
    Exception.record_backtrace true;
    PidLog.log
      (Printf.sprintf
         "%s - Hello from restore"
         (worker_id_str ~worker_id));
    let query_db_cache = Reverse_index.QueryDbCache.make db_name in
    Reverse_index.SqliteIndex.init query_db_cache

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
  let _nbr_procs =
    (* MacBook Air 2017 *)
    2
    (* Sys_utils.nbr_procs *)
  in
  (* 2 is too few. I need to buy a new MacBook Air *)
  let num_workers =
    10
    (* nbr_procs * 5  *)
  in

  let heap_handle = SharedMem.init ~num_workers SharedMem.default_config in
  let gc_control = Gc.get () in
  let state = init_state () in
  let workers =
    MultiWorker.make
      ~call_wrapper:{ WorkerController.wrap = catch_and_classify_exceptions }
      ~longlived_workers:false
      ~saved_state:state
      ~entry
      num_workers
      ~gc_control
      ~heap_handle
  in
  workers

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
  let job_simple (acc: int list) (input: int list) =
    (* Is c the accumulator (seems to be inited by neutral), and a the input? *)
    ignore (acc, input);
    PidLog.log (Printf.sprintf "Job! %d %d\n" (List.length acc) (List.hd input));
    List.fold_left (fun acc el -> acc + el) 0 input;
  in
  let merge_simple (job_result: int) (acc: int list) : int list =
    ignore job_result;
    PidLog.log (Printf.sprintf "Merge: %d %d\n" job_result (List.length acc));
    job_result :: acc
  in
  let next_simple =
    let rec aux acc n =
      if n >= 0 then
        aux (n :: acc) (n - 1)
      else
        acc
    in
    aux [] 200000
  in

  let (c: int list) = MultiWorker.call
      workers
      ~job:job_simple
      ~neutral:[1; 2; 3; 4]
      ~merge:merge_simple
      ~next:(MultiWorker.next workers next_simple)
  in

  let work_items_to_process = ref [WorkItem.Topic "Capital city"] in
  let work_items_in_progress = ref WorkItemSet.empty in
  let work_items_count = ref (List.length !work_items_to_process) in

  let merge =
    merge
      ~max_work_items:500
      ~work_items_count
      ~work_items_to_process
      ~work_items_in_progress
  in
  let next =
    next
      ~workers
      ~work_items_to_process
      ~work_items_in_progress
  in
  let t = Unix.gettimeofday () in
  let (errors, telemetry) = MultiWorker.call
      workers
      ~job
      ~neutral:([], empty_telemetry)
      ~merge
      ~next
  in
  let total_duration = Unix.gettimeofday () -. t in
  Printf.printf "Total errors (I hope): %d\n%!" (List.length errors);
  (* List.iter
     (fun error ->
       match error with
       | ParallerShallowChecker.Index_error (source_name, retrieval_error) ->
         Printf.printf
           "Index error in `%s`:\n    %s\n"
           source_name
           retrieval_error.message
       | ParallerShallowChecker.Catalog_error (source_name, catalog_error) ->
         Printf.printf
           "Catalog error in `%s`:\n    %s\n"
           source_name
           catalog_error.message
     )
     errors; *)

  Printf.printf
    "Telemetry:\n    Total time: %0.10f\n    Time in catalog: %0.10f\n    Time in index: %0.10f\n    Cache time: %0.10f\n    Other time: %0.10f\n    Catalog cache hits: %d\n    Cache hits: %d\n    Cache misses: %d\n"
    total_duration
    telemetry.catalog_duration
    telemetry.index_duration
    telemetry.cache_duration
    telemetry.other_duration
    telemetry.catalog_cache_hits
    telemetry.cache_hits
    telemetry.cache_misses;

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

  let should_generate_reverse_index = false in
  let should_check_article = false in

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

  let module TransitiveChecker = TransitiveChecker (Reverse_index.SqliteIndex) (Demo_catalog.HashTableXmlCatalog) in

  if should_check_article then
    begin
      match TransitiveChecker.check_article_transitively "continent" with
      | [] -> failwith "This should not happen; no results?"
      | hashes -> List.iter (fun hash -> Printf.printf "Final hash is %s\n" (Int64.to_string hash)) hashes
    end;

  Printf.printf "*** DONE: %d ***\n\n%!" (List.length c)