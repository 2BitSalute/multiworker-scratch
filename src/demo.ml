(*
 * Copyright (c) Tatiana Racheva
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

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
        dummy
      } : state)
      ~(worker_id : int) : unit =
    Printf.printf "%s - Hello from restore" (worker_id_str ~worker_id)

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
  | Decl_class.Decl_heap_elems_bug -> Exit.exit Exit_status.Decl_heap_elems_bug
  | File_provider.File_provider_stale ->
    Exit.exit Exit_status.File_provider_stale
  | Decl_defs.Decl_not_found x ->
    Hh_logger.log "Decl_not_found %s" x;
    Exit.exit Exit_status.Decl_not_found
  | Not_found_s _
  | Caml.Not_found ->
    Exit.exit Exit_status.Worker_not_found_exception

let init_state
    ~(root : Path.t)
    ~(popt : ParserOptions.t)
    ~(tcopt : TypecheckerOptions.t)
    ~(deps_mode : Typing_deps_mode.t) :
  Provider_context.t * Batch_global_state.batch_state =
  Relative_path.(set_path_prefix Root root);
  make_tmp_dir ();
  make_hhi_dir ();
  Typing_global_inference.set_path ();
  let ctx =
    Provider_context.empty_for_tool
      ~popt
      ~tcopt
      ~backend:Provider_backend.Shared_memory
      ~deps_mode
  in
  let batch_state = Batch_global_state.save ~trace:true in
  (ctx, batch_state)

let init () =
  let nbr_procs = Sys_utils.nbr_procs in
  let heap_handle = SharedMem.init ~num_workers:nbr_procs shmem_config in
  let gc_control = Core_kernel.Gc.get () in
  let (ctx, state) = init_state ~root ~popt ~tcopt ~deps_mode in
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

let () =
  let _workers = init () in
  ()