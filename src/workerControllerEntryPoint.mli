(*
 * Copyright (c) 2021, Tatiana Racheva
 * Copyright (c) 2021, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module MakeWorkerControllerEntryPoint
    (* TODO: remove *)
    (Worker: Procs_sig.WORKER)
    (WorkerController: Procs_sig.WORKERCONTROLLER) : sig
  val register :
    restore:('a -> worker_id:int -> unit) ->
    ('a WorkerController.worker_params, Worker.request, 'b) Worker.Daemon.entry
end
