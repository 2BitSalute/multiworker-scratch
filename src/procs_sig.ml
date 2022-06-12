module type SHAREDMEM = sig
  (* TODO: MOVE *)
  type handle
end

module type WORKER = sig
  module Daemon: Sys_sig.DAEMON

  type request = Request of (serializer -> unit)
  and serializer = { send: 'a. 'a -> unit }

  type subprocess_job_status = Subprocess_terminated of Unix.process_status

  val win32_worker_main :
    ('a -> 'b) ->
    'a * Unix.file_descr option ->
    request Daemon.in_channel * 'c Daemon.out_channel ->
    'd

  val unix_worker_main :
    ('a -> 'b) ->
    'a * Unix.file_descr option ->
    request Daemon.in_channel * 'c Daemon.out_channel ->
    'd

  val unix_worker_main_no_clone :
    ('a -> 'b) ->
    'a * Unix.file_descr option ->
    request Daemon.in_channel * 'c Daemon.out_channel ->
    'd
end

module type WORKERCONTROLLER = sig
  module Daemon : Sys_sig.DAEMON
  module SharedMem : SHAREDMEM
  module Worker : WORKER

  (*****************************************************************************)
  (* Module building workers.
   * A worker is a subprocess executing an arbitrary function.
   * You should first create a fixed amount of workers and then use those
   * because the amount of workers is limited and to make the load-balancing
   * of tasks better (cf multiWorker.ml).
  *)
  (*****************************************************************************)

  type process_id = int

  type worker_id = int

  type worker_failure =
    (* Worker foce quit by Out Of Memory. *)
    | Worker_oomed
    | Worker_quit of Unix.process_status

  exception Worker_failed of (process_id * worker_failure)

  (* Raise this exception when sending work to a worker that is already busy.
   * We should never be doing that, and this is an assertion error. *)
  exception Worker_busy

  val failure_to_string : worker_failure -> string

  type send_job_failure =
    | Worker_already_exited of Unix.process_status
    | Other_send_job_failure of exn

  exception Worker_failed_to_send_job of send_job_failure

  (* The type of a worker visible to the outside world *)
  type worker

  (*****************************************************************************)
  (* The handle is what we get back when we start a job. It's a "future"
   * (sometimes called a "promise"). The scheduler uses the handle to retrieve
   * the result of the job when the task is done (cf multiWorker.ml).
  *)
  (*****************************************************************************)
  type ('job, 'result) handle

  (* An empty type *)
  type void

  (* Get the worker's id *)
  val worker_id : worker -> worker_id

  (* Has the worker been force quit *)
  val is_force_quit : worker -> bool

  (* Mark the worker as busy. Throw if it is already busy *)
  val mark_busy : worker -> unit

  (* If the worker is busy, what is it doing. Note that calling this is not
   * type safe: 'a and 'b are free type variables, and they depend on what is the
   * job being executed by worker. *)
  val get_handle_UNSAFE : worker -> ('a, 'b) handle option

  (* Mark the worker as free *)
  val mark_free : worker -> unit

  (* If the worker isn't prespawned, spawn the worker *)
  val spawn : worker -> (void, Worker.request) Daemon.handle

  (* If the worker isn't prespawned, close the worker *)
  val close : worker -> (void, Worker.request) Daemon.handle -> unit

  type call_wrapper = { wrap: 'x 'b. ('x -> 'b) -> 'x -> 'b }

  type 'a entry_state = 'a * Gc.control * SharedMem.handle * int

  (* The first bool parameter specifies whether to use worker clones
   * or not: for non-longlived-workers, we must clone. *)
  type 'a worker_params = {
    longlived_workers: bool;
    entry_state: 'a entry_state;
    controller_fd: Unix.file_descr option;
  }

  type 'a entry = ('a worker_params, Worker.request, void) Daemon.entry

  (* Creates a pool of workers. *)
  val make :
    ?call_wrapper:
      (* See docs in WorkerController.worker for call_wrapper. *)
      call_wrapper ->
    longlived_workers:bool ->
    saved_state:'a ->
    entry:'a entry ->
    int ->
    gc_control:Gc.control ->
    heap_handle:SharedMem.handle ->
    worker list

  (* Call in a sub-process (CAREFUL, GLOBALS ARE COPIED) *)
  val call : ?call_id:int -> worker -> ('a -> 'b) -> 'a -> ('a, 'b) handle

  (* See MultiThreadedCall.call_id *)
  val get_call_id : ('a, 'b) handle -> int

  (* Retrieves the job that the worker is currently processing *)
  val get_job : ('a, 'b) handle -> 'a

  (* Retrieves the result (once the worker is done) hangs otherwise *)
  val get_result : ('a, 'b) handle -> 'b

  (* Selects among multiple handles those which are ready. *)
  type ('a, 'b) selected = {
    readys: ('a, 'b) handle list;
    waiters: ('a, 'b) handle list;
    (* Additional (non worker) ready fds that we selected on. *)
    ready_fds: Unix.file_descr list;
  }

  val select : ('a, 'b) handle list -> Unix.file_descr list -> ('a, 'b) selected

  (* Returns the worker which produces this handle *)
  val get_worker : ('a, 'b) handle -> worker

  (* Force quit the workers *)
  val force_quit_all : unit -> unit

  val cancel : ('a, 'b) handle list -> unit

end

module type MULTITHREADEDCALL = sig
  module Bucket : Bucket.BUCKET
  module Exception : Sys_sig.EXCEPTION
  module WorkerController : WORKERCONTROLLER

  (** If a worker process fails, this is raised.
   *
   * Note: When one worker process fails, the remaining in-progress workers are checked
   * for completion/failure, and all their failures (non-zero exit code) are coalesced
   * together into one of these exceptions.
   *
   * No further buckets are distributed to workers.
   *
   * Still-in-progress workers are left to their own accord. *)
  exception Coalesced_failures of WorkerController.worker_failure list

  val coalesced_failures_to_string :
    WorkerController.worker_failure list -> string

  type interrupt_result =
    | Cancel
    | Continue

  type 'env interrupt_handler = 'env -> 'env * interrupt_result

  type 'env interrupt_config = {
    env: 'env;
    handlers: 'env -> (Unix.file_descr * 'env interrupt_handler) list;
  }

  type worker_id = int

  val no_interrupt : 'a -> 'a interrupt_config

  (** Can raise Coalesced_failures exception. *)
  val call :
    WorkerController.worker list ->
    ('c -> 'a -> 'b) ->
    ('b -> 'c -> 'c) ->
    'c ->
    'a Bucket.next ->
    'c

  (** Invokes merge with a unique worker id.
      Can raise Coalesced_failures exception. *)
  val call_with_worker_id :
    WorkerController.worker list ->
    (worker_id * 'c -> 'a -> 'b) ->
    (worker_id * 'b -> 'c -> 'c) ->
    'c ->
    'a Bucket.next ->
    'c

  (** The last element returned, a list of job inputs, are the job inputs which have not been
      processed fully or at all due to interrupts. *)
  val call_with_interrupt :
    WorkerController.worker list ->
    ('c -> 'a -> 'b) ->
    ('b -> 'c -> 'c) ->
    'c ->
    'a Bucket.next ->
    ?on_cancelled:
      ((* [on_cancelled] should be specified if your [next] function ever returns
          [Bucket.Wait], and it should return the list of all jobs that haven't
          finished or started yet. *)
        unit ->
        'a list) ->
    'd interrupt_config ->
    'c * 'd * 'a list

  val on_exception : (Exception.t -> unit) -> unit
end