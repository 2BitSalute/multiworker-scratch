(** Some telemetry utilities *)
module type SMTELEMETRY = sig
  module Telemetry : Sys_sig.TELEMETRY

  val get_telemetry_list : (Telemetry.t -> Telemetry.t) list ref

  (** Get some shared-memory telemetry. Even works when shared memory hasn't
      been initialized yet. *)
  val get_telemetry : unit -> Telemetry.t

  (** Return the number of bytes allocated in shared memory. This includes
      bytes that were free'd but are not yet available for reuse. *)
  val heap_size : unit -> int

  (** Returns the number of bytes not reachable fro hashtable entries. *)
  val wasted_heap_size : unit -> int

  (** The logging level for shared memory statistics:
      - 0 = nothing
      - 1 = log totals, averages, min, max bytes marshalled and unmarshalled
  *)
  val hh_log_level : unit -> int

  (** Get the sample rate for shared memory statistics. *)
  val hh_sample_rate : unit -> float

  (** Get the number of used slots in our hashtable. *)
  val hash_used_slots : unit -> int * int

  (** Get the number of total slots in our hashtable. *)
  val hash_slots : unit -> int

  (** Get the number of used slots in our dependency table. *)
  val dep_used_slots : unit -> int

  (** Get the total number of slots in our dependency table. *)
  val dep_slots : unit -> int

  type table_stats = {
    nonempty_slots: int;
    used_slots: int;
    slots: int;
  }

  (** Combine [dep_used_slots] and [dep_slots] *)
  val dep_stats : unit -> table_stats

  (** Combine [hash_used_slots] and [hash_slots] *)
  val hash_stats : unit -> table_stats

  (** Not sure. Return the removed number of entries? *)
  val hh_removed_count : unit -> int

  (** Did we overflow the heap? *)
  val is_heap_overflow : unit -> bool

  (** Compute the size of values in the garbage-collected heap. (???) *)
  val value_size : Obj.t -> int

  (** Log to our telemetry infra that we successfully initialized shared
      memory *)
  val init_done : unit -> unit
end

(** Interface to the garbage collector *)
module type GC = sig
  val should_collect : [ `aggressive | `always_TEST | `gentle ] -> bool

  val collect : [ `aggressive | `always_TEST | `gentle ] -> unit
end

module type SHAREDMEM = sig
  (** This is just a sentinel for self-documenting purposes which some
      parts of the codebase use. They take a parameter "uses_sharedmem : SharedMem.uses"
      as a way to indicate to their callers that they read/write sharedmem. *)
  type uses = Uses

  exception Out_of_shared_memory

  exception Hash_table_full

  exception Heap_full

  exception Sql_assertion_failure of int

  exception C_assertion_failure of string

  (** Configuration object that initializes shared memory. *)
  type config = {
    global_size: int;
    heap_size: int;
    dep_table_pow: int;
    hash_table_pow: int;
    shm_use_sharded_hashtbl: bool;
    shm_dirs: string list;
    shm_min_avail: int;
    log_level: int;
    sample_rate: float;
    (* 0 - lz4, others -- compression level for zstd*)
    compression: int;
  }
  [@@deriving show]

  (** Default configuration object *)
  val default_config : config

  (** Empty configuration object.

      There are places where we don't expect to write to shared memory, and doing
      so would be a memory leak. But since shared memory is global, it's very easy
      to accidentally call a function that will attempt such write. This config
      initializes shared memory with zero sizes. As such, attempting to write to
      shared memory that was initialized with this config, will make the program
      fail immediately. *)
  val empty_config : config

  (** A handle to initialized shared memory. Used to connect other workers to
      shared memory.

      NOTE: If you change the order, update hh_shared.c! *)
  type handle = private {
    h_fd: Unix.file_descr;
    h_global_size: int;
    h_heap_size: int;
    h_dep_table_pow_val: int;
    h_hash_table_pow_val: int;
    h_num_workers_val: int;
    h_shm_use_sharded_hashtbl: bool;
    h_sharded_hashtbl_fd: Unix.file_descr;
  }

  (** Initialize shared memory.

      Must be called before forking. *)
  val init : config -> num_workers:int -> handle

  (** Connect other workers to shared memory *)
  val connect : handle -> worker_id:int -> unit

  (** Get the handle to shared memory. Returns nonsense if the current
      process hasn't yet connected to shared memory *)
  val get_handle : unit -> handle

  (** Allow or disallow remove operations. *)
  val set_allow_removes : bool -> unit

  (** Allow or disallow shared memory writes for the current process. *)
  val set_allow_hashtable_writes_by_current_process : bool -> unit
end