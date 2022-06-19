(*
 * Copyright (c) 2022, Tatiana Racheva
 * Copyright (c) 2021, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module MakeWorkerControllerEntryPoint
    (Worker: Procs_sig.WORKER)
    (WorkerController: Procs_sig.WORKERCONTROLLER) = struct

  open Worker
  open WorkerController

  let entry_counter = ref 0

  let win32_worker ~restore p =
    (* TODO: install signal handlers *)
    win32_worker_main restore (p.entry_state, p.controller_fd)

  let unix_worker ~restore { longlived_workers; entry_state; controller_fd } =
    (* TODO: install signal handlers *)
    if longlived_workers then
      unix_worker_main_no_clone restore (entry_state, controller_fd)
    else
      unix_worker_main restore (entry_state, controller_fd)

  let register ~restore =
    incr entry_counter;
    let restore (st, gc_control, heap_handle, worker_id) =
      restore st ~worker_id;
      SharedMem.connect heap_handle ~worker_id;
      Gc.set gc_control
    in
    let name = Printf.sprintf "subprocess_%d" !entry_counter in
    let worker_main =
      if Sys.win32 then
        win32_worker ~restore
      else
        unix_worker ~restore
    in
    Daemon.register_entry_point name worker_main
end