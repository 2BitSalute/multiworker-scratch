(*
 * Copyright (c) 2022, Tatiana Racheva
 * Copyright (c) 2015, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module CallFunctor (Caller : Procs_sig.CALLER) = struct
  module Bucket = Caller.Bucket

  let next ?progress_fn ?max_size workers =
    Bucket.make
      ~num_workers:
        (match workers with
         | Some w -> List.length w
         | None -> 1)
      ?progress_fn
      ?max_size

  let single_threaded_call_with_worker_id job merge neutral next =
    let x = ref (next ()) in
    let acc = ref neutral in
    (* This is a just a sanity check that the job is serializable and so
     * that the same code will work both in single threaded and parallel
     * mode.
    *)
    let _ = Marshal.to_string job [Marshal.Closures] in
    while not (Bucket.is_done !x) do
      match !x with
      | Bucket.Wait ->
        (* May waiting for remote worker to finish *)
        x := next ()
      | Bucket.Job l ->
        let res = job (0, neutral) l in
        acc := merge (0, res) !acc;
        x := next ()
      | Bucket.Done -> ()
    done;
    !acc

  let single_threaded_call job merge neutral next =
    let job (_worker_id, a) b = job a b in
    let merge (_worker_id, a) b = merge a b in
    single_threaded_call_with_worker_id job merge neutral next

  let call workers ~job ~merge ~neutral ~next =
    match workers with
    | None ->
      Caller.return (single_threaded_call_with_worker_id job merge neutral next)
    | Some workers -> Caller.multi_threaded_call workers job merge neutral next
end

module MakeMultiWorker
    (MultiThreadedCall : Procs_sig.MULTITHREADEDCALL)
  : Procs_sig.MULTIWORKER = struct
  module Bucket = MultiThreadedCall.Bucket
  module MultiThreadedCall = MultiThreadedCall
  module SharedMem = MultiThreadedCall.WorkerController.SharedMem
  module WorkerController = MultiThreadedCall.WorkerController

  module Call = CallFunctor (struct
      module Bucket = Bucket
      module WorkerController = WorkerController
      type 'a result = 'a

      let return x = x

      let multi_threaded_call = MultiThreadedCall.call_with_worker_id
    end)

  let next = Call.next

  (* Hide the worker type from our users *)
  type worker = WorkerController.worker

  type 'a interrupt_config = 'a MultiThreadedCall.interrupt_config

  let call_with_worker_id = Call.call

  let call workers ~job ~merge ~neutral ~next =
    let job (_worker_id, a) b = job a b in
    let merge (_worker_id, a) b = merge a b in
    Call.call workers ~job ~merge ~neutral ~next

  (* If we ever want this in MultiWorkerLwt then move this into CallFunctor *)
  let call_with_interrupt
      ?on_cancelled workers ~job ~merge ~neutral ~next ~interrupt =
    match workers with
    | Some workers when List.length workers <> 0 ->
      Hh_logger.log
        "MultiThreadedCall.call_with_interrupt called with %d workers"
        (List.length workers);
      MultiThreadedCall.call_with_interrupt
        ?on_cancelled
        workers
        job
        merge
        neutral
        next
        interrupt
    | _ ->
      Hh_logger.log "single_threaded_call called with zero workers";
      ( Call.single_threaded_call job merge neutral next,
        interrupt.MultiThreadedCall.env,
        [] )



  let make = WorkerController.make

  type call_wrapper = {
    f:
      'a 'b 'c.
        worker list option ->
      job:('c -> 'a -> 'b) ->
      merge:('b -> 'c -> 'c) ->
      neutral:'c ->
      next:'a Bucket.next ->
      'c;
  }

  let wrapper = { f = call }
end