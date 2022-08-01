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

type article = {
  text_hash: int;
  length: int;
  dependencies: string list;
}

type payload =
  (* An article was explicitly asked to be checked by title *)
  | Title of string
  (* A stream contains 100 unrelated articles *)
  | Stream of int64
  (* Since we know an article's dependencies have already been queued, we just need to check that they're all finished *)
  | Article of article

(* We found dependencies, and they need to be checked first, articles next *)
type result = {
  articles: article list;
  streams: int64 list;
}

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

  Demo_bz2.index ("../wikipedia/" ^ "enwiki-20211020-pages-articles-multistream-index.txt.bz2");

  if false then begin
    let filename =
      Demo_bz2.catalog ("../wikipedia/" ^ "enwiki-20211020-pages-articles-multistream.xml.bz2") 597L
    in

    Demo_xmlm.run filename;
  end;

  Printf.printf "*** DONE: %d ***\n\n%!" (List.length c)