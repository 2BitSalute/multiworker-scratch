(*
 * Copyright (c) 2022, Tatiana Racheva
 * Copyright (c) 2015, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* The protocol for a next function is to return a list of elements.
 * It will be called repeatedly until it returns an empty list.
*)

(* TODO: rename into MakeCall *)
module CallFunctor (Caller : Procs_sig.CALLER) : sig
  val call :
    Caller.WorkerController.worker list option ->
    job:(Caller.WorkerController.worker_id * 'c -> 'a -> 'b) ->
    merge:(Caller.WorkerController.worker_id * 'b -> 'c -> 'c) ->
    neutral:'c ->
    next:'a Caller.Bucket.next ->
    'c Caller.result
end

module MakeMultiWorker
    (MultiThreadedCall : Procs_sig.MULTITHREADEDCALL)
  : Procs_sig.MULTIWORKER