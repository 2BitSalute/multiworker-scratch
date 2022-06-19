(*
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

module Hashtbl = Stdlib.Hashtbl
module Queue = Stdlib.Queue
module Set = Stdlib.Set

(* TODO: move out *)
module GlobalConfig = struct
  let shm_dir = "/dev/shm"
  let tmp_dir = "/tmp/shm"
end

module SharedMem : SharedMem_sig.SHAREDMEM = struct
  type uses = Uses

  let ref_has_done_init = ref false

  (* Don't change the ordering of this record without updating hh_shared_init in
   * hh_shared.c, which indexes into config objects *)
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

  let default_config =
    let gig = 1024 * 1024 * 1024 in
    {
      global_size = gig;
      heap_size = 20 * gig;
      dep_table_pow = 17;
      (* 1 << 17 *)
      hash_table_pow = 18;
      (* 1 << 18 *)
      shm_dirs = [GlobalConfig.shm_dir; GlobalConfig.tmp_dir];
      shm_use_sharded_hashtbl = false;
      shm_min_avail = gig / 2;
      (* Half a gig by default *)
      log_level = 0;
      sample_rate = 0.0;
      compression = 0;
    }

  let empty_config =
    {
      global_size = 0;
      heap_size = 0;
      dep_table_pow = 0;
      hash_table_pow = 0;
      shm_dirs = [];
      shm_use_sharded_hashtbl = false;
      shm_min_avail = 0;
      log_level = 0;
      sample_rate = 0.0;
      compression = 0;
    }

  (* Allocated in C only. *)
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

  exception Out_of_shared_memory

  exception Hash_table_full

  exception Dep_table_full

  exception Heap_full

  exception Revision_length_is_zero

  exception Sql_assertion_failure of int

  exception Failed_anonymous_memfd_init

  exception Less_than_minimum_available of int

  exception Failed_to_use_shm_dir of string

  exception C_assertion_failure of string

  let () =
    Callback.register_exception "out_of_shared_memory" Out_of_shared_memory;
    Callback.register_exception "hash_table_full" Hash_table_full;
    Callback.register_exception "dep_table_full" Dep_table_full;
    Callback.register_exception "heap_full" Heap_full;
    Callback.register_exception "revision_length_is_zero" Revision_length_is_zero;
    Callback.register_exception "sql_assertion_failure" (Sql_assertion_failure 0);
    Callback.register_exception
      "failed_anonymous_memfd_init"
      Failed_anonymous_memfd_init;
    Callback.register_exception
      "less_than_minimum_available"
      (Less_than_minimum_available 0);
    Callback.register_exception
      "c_assertion_failure"
      (C_assertion_failure "dummy string")

  external hh_shared_init :
    config:config -> shm_dir:string option -> num_workers:int -> handle
    = "hh_shared_init"

  let anonymous_init config ~num_workers =
    hh_shared_init ~config ~shm_dir:None ~num_workers

  let rec shm_dir_init config ~num_workers = function
    | [] ->
      Hh_logger.log "We've run out of filesystems to use for shared memory";
      raise Out_of_shared_memory
    | shm_dir :: shm_dirs ->
      (* For some reason statvfs is segfaulting when the directory doesn't
       * exist, instead of returning -1 and an errno *)
      begin
        try
          if not (Sys.file_exists shm_dir) then
            raise (Failed_to_use_shm_dir "shm_dir does not exist");
          hh_shared_init ~config ~shm_dir:(Some shm_dir) ~num_workers
        with
        | Less_than_minimum_available avail ->
          (* EventLogger.(
             log_if_initialized (fun () ->
                sharedmem_less_than_minimum_available
                  ~shm_dir
                  ~shm_min_avail
                  ~avail)); *)
          Hh_logger.log
            "Filesystem %s only has %d bytes available, which is less than the minimum %d bytes"
            shm_dir
            avail
            config.shm_min_avail;
          shm_dir_init config ~num_workers shm_dirs
        | Unix.Unix_error (e, fn, arg) ->
          let fn_string =
            if String.equal fn "" then
              ""
            else
              Printf.sprintf " thrown by %s(%s)" fn arg
          in
          let reason =
            Printf.sprintf "Unix error%s: %s" fn_string (Unix.error_message e)
          in
          (* EventLogger.(
             log_if_initialized (fun () ->
                sharedmem_failed_to_use_shm_dir ~shm_dir ~reason)); *)
          Hh_logger.log "Failed to use shm dir `%s`: %s" shm_dir reason;
          shm_dir_init config ~num_workers shm_dirs
        | Failed_to_use_shm_dir reason ->
          (* EventLogger.(
             log_if_initialized (fun () ->
                sharedmem_failed_to_use_shm_dir ~shm_dir ~reason)); *)
          Hh_logger.log "Failed to use shm dir `%s`: %s" shm_dir reason;
          shm_dir_init config ~num_workers shm_dirs
      end

  let init config ~num_workers =
    ref_has_done_init := true;
    try anonymous_init config ~num_workers with
    | Failed_anonymous_memfd_init ->
      (* EventLogger.(
         log_if_initialized (fun () -> sharedmem_failed_anonymous_memfd_init ())); *)
      Hh_logger.log "Failed to use anonymous memfd init";
      shm_dir_init config ~num_workers config.shm_dirs

  external connect : handle -> worker_id:int -> unit = "hh_connect"

  external get_handle : unit -> handle = "hh_get_handle"

  external set_allow_removes : bool -> unit = "hh_set_allow_removes"

  external set_allow_hashtable_writes_by_current_process : bool -> unit
    = "hh_set_allow_hashtable_writes_by_current_process"
end

module RawAccess = struct
  (* Allocated in C only. *)
  type serialized = private bytes

  external mem_raw : string -> bool = "hh_mem"

  external get_raw : string -> serialized option = "hh_get_raw"

  external add_raw : string -> serialized -> unit = "hh_add_raw"

  external deserialize_raw : serialized -> 'a = "hh_deserialize_raw"

  external serialize_raw : 'a -> serialized = "hh_serialize_raw"
end

module DepTable = struct
  external loaded_dep_table_filename_c : unit -> string
    = "hh_get_loaded_dep_table_filename"

  external get_in_memory_dep_table_entry_count : unit -> int
    = "hh_get_in_memory_dep_table_entry_count"

  let loaded_dep_table_filename () =
    let fn = loaded_dep_table_filename_c () in
    if String.equal "" fn then
      None
    else
      Some fn

  external save_dep_table_blob_c : string -> string -> bool -> int
    = "hh_save_dep_table_blob"

  external save_dep_table_sqlite_c : string -> string -> int
    = "hh_save_dep_table_sqlite"

  external update_dep_table_sqlite_c : string -> string -> int
    = "hh_update_dep_table_sqlite"

  let save_dep_table_sqlite ~fn ~build_revision =
    if Option.is_some (loaded_dep_table_filename ()) then
      failwith
        "save_dep_table_sqlite not supported when server is loaded from a saved state; use update_dep_table_sqlite";
    Hh_logger.log "Dumping a saved state deptable into a SQLite DB.";
    save_dep_table_sqlite_c fn build_revision

  let save_dep_table_blob ~fn ~build_revision ~reset_state_after_saving =
    if Option.is_some (loaded_dep_table_filename ()) then
      failwith
        "save_dep_table_blob not supported when the server is loaded from a saved state; use update_dep_table_sqlite";
    Hh_logger.log "Dumping a saved state deptable as a blob.";

    save_dep_table_blob_c fn build_revision reset_state_after_saving

  let update_dep_table_sqlite ~fn ~build_revision =
    Hh_logger.log "Updating given saved state deptable.";
    update_dep_table_sqlite_c fn build_revision

  external load_dep_table_blob_c : string -> bool -> int
    = "hh_load_dep_table_blob"

  external load_dep_table_sqlite_c : string -> bool -> unit
    = "hh_load_dep_table_sqlite"

  let load_dep_table_blob ~fn ~ignore_hh_version =
    load_dep_table_blob_c fn ignore_hh_version

  let load_dep_table_sqlite ~fn ~ignore_hh_version =
    load_dep_table_sqlite_c fn ignore_hh_version

  external cleanup_sqlite : unit -> unit = "hh_cleanup_sqlite"
end

module MakeSMTelemetry
    (Core: Core_sig.COREOPS)
    (Telemetry: Sys_sig.TELEMETRY) : SharedMem_sig.SMTELEMETRY = struct
  (*****************************************************************************)
  (* Each cache can write telemetry about its current occupancy.
   * - Immediate caches - only records its existence
   * - WithLocalChanges caches - they do Obj.reachable_words to count up the stack
   * - Local caches - they do Obj.reachable_words
   * In the case of compound caches, e.g. HeapWithLocalCache which includes all three,
   * it doesn't have to report telemetry since each of its constituents already
   * reports telemetry on its own.
   * Anyway, each cache registers in the global "get_telemetry_list" so that
   * callers can do SharedMem.get_telemetry and pick up from all caches.
   *
   * Caveats:
   * Note that Obj.reachable_words may double-count stuff if it's in both
   * Local and WithLocalChanges cache. It may also take time, up to ~300ms.
   * And it will be meaningless if the items in the Local cache have references
   * into other parts of the system. It's up to the reader to make sense of it.
   *
   * The "WithLocalChanges" doesn't have a straightforward count of elements.
   * Instead it counts how many "actions" there are across all change-stacks:
   * how many adds, removes, replaces.
  *)
  (*****************************************************************************)

  module List = Core.List
  module Telemetry = Telemetry

  let get_telemetry_list = ref []

  let get_telemetry () : Telemetry.t =
    (* This function gets called by compute_tast, even in places which
       deliberately don't initialize shared memory. In these places, no-op,
       since otherwise reading from hh_log_level would segfault. *)
    if not !ref_has_done_init then
      Telemetry.create ()
    else
      let start_time = Unix.gettimeofday () in
      let telemetry =
        List.fold
          !get_telemetry_list
          ~init:(Telemetry.create ())
          ~f:(fun acc get_telemetry -> get_telemetry acc)
      in
      telemetry |> Telemetry.duration ~start_time

  external heap_size : unit -> int = "hh_used_heap_size" [@@noalloc]

  external wasted_heap_size : unit -> int = "hh_wasted_heap_size" [@@noalloc]

  external hh_log_level : unit -> int = "hh_log_level" [@@noalloc]

  external hh_sample_rate : unit -> float = "hh_sample_rate"

  external hash_used_slots : unit -> int * int = "hh_hash_used_slots"

  external hash_slots : unit -> int = "hh_hash_slots"

  external dep_used_slots : unit -> int = "hh_dep_used_slots"

  external dep_slots : unit -> int = "hh_dep_slots"

  type table_stats = {
    nonempty_slots: int;
    used_slots: int;
    slots: int;
  }

  let dep_stats () =
    let used = dep_used_slots () in
    { nonempty_slots = used; used_slots = used; slots = dep_slots () }

  let hash_stats () =
    let (used_slots, nonempty_slots) = hash_used_slots () in
    { nonempty_slots; used_slots; slots = hash_slots () }

  external hh_removed_count : unit -> int = "hh_removed_count"

  external is_heap_overflow : unit -> bool = "hh_check_heap_overflow"

  let value_size r =
    let w = Obj.reachable_words r in
    w * (Sys.word_size / 8)

  let init_done () =
    ()
    (* EventLogger.sharedmem_init_done (heap_size ()) *)
end

module MakeGC (SMTelemetry: SharedMem_sig.SMTELEMETRY) = struct
  external hh_collect : unit -> unit = "hh_collect" [@@noalloc]

  let should_collect (effort : [ `gentle | `aggressive | `always_TEST ]) =
    let overhead =
      match effort with
      | `always_TEST -> 1.0
      | `aggressive -> 1.2
      | `gentle -> 2.0
    in
    let used = SMTelemetry.heap_size () in
    let wasted = SMTelemetry.wasted_heap_size () in
    let reachable = used - wasted in
    (* TODO: Originally, iround_towards_zero_exn *)
    used >= int_of_float (Float.round (float reachable *. overhead))

  let collect (effort : [ `gentle | `aggressive | `always_TEST ]) =
    let old_size = SMTelemetry.heap_size () in
    (* TODO: why? Do I care about Stats? *)
    (* Stats.update_max_heap_size old_size; *)
    let start_t = Unix.gettimeofday () in
    if should_collect effort then hh_collect ();
    let new_size = SMTelemetry.heap_size () in
    let time_taken = Unix.gettimeofday () -. start_t in
    if old_size <> new_size then (
      Hh_logger.log
        "Sharedmem GC: %d bytes before; %d bytes after; in %f seconds"
        old_size
        new_size
        time_taken;
      (* EventLogger.sharedmem_gc_ran effort old_size new_size time_taken *)
    )
end

module type Key = sig
  type t

  val to_string : t -> string

  val compare : t -> t -> int
end

module type KeyHasher = sig
  type key

  type hash

  val hash : key -> hash

  val hash_old : key -> hash

  val to_bytes : hash -> string
end

module MakeKeyHasher (Key : Key) : KeyHasher with type key = Key.t = struct
  type key = Key.t

  type hash = string

  let prefix = Prefix.make ()

  (* The prefix we use for old keys. The prefix guarantees that we never
   * mix old and new data, because a key can never start with the prefix
   * "old_", it always starts with a number (cf Prefix.make()).
  *)
  let old_prefix = "old_"

  let full_key (x : key) : string = Prefix.make_key prefix (Key.to_string x)

  let full_key_old (x : key) : string =
    old_prefix ^ Prefix.make_key prefix (Key.to_string x)

  let hash (key : key) : hash = Stdlib.Digest.string (full_key key)

  let hash_old (key : key) : hash = Stdlib.Digest.string (full_key_old key)

  let to_bytes (hash : hash) : string = hash
end

module type Value = sig
  type t

  val description : string
end

module type Backend = functor
  (Core: Core_sig.COREOPS)
  (Measure: Sys_sig.MEASURE)
  (SMTelemetry: SharedMem_sig.SMTELEMETRY)
  (KeyHasher : KeyHasher)
  (Value : Value) -> sig
  val add : KeyHasher.hash -> Value.t -> unit

  val mem : KeyHasher.hash -> bool

  val get : KeyHasher.hash -> Value.t option

  val remove : KeyHasher.hash -> unit

  val move : KeyHasher.hash -> KeyHasher.hash -> unit
end

module type Capacity = sig
  val capacity : int
end

module ImmediateBackend : Backend =
  functor
    (Core: Core_sig.COREOPS)
    (Measure: Sys_sig.MEASURE)
    (SMTelemetry: SharedMem_sig.SMTELEMETRY)
    (KeyHasher : KeyHasher)
    (Value : Value)
    ->
    struct
      module List = Core.List
      module Option = Core.Option
      module Telemetry = SMTelemetry.Telemetry

      (* Returns the number of bytes allocated in the heap, or a negative number
       * if no new memory was allocated *)
      external hh_add : KeyHasher.hash -> Value.t -> int * int * int = "hh_add"

      external hh_mem : KeyHasher.hash -> bool = "hh_mem"

      external hh_get_size : KeyHasher.hash -> int = "hh_get_size"

      external hh_get_and_deserialize : KeyHasher.hash -> Value.t option
        = "hh_get_and_deserialize"

      external hh_remove : KeyHasher.hash -> int = "hh_remove"

      external hh_move : KeyHasher.hash -> KeyHasher.hash -> unit = "hh_move"

      let measure_add = Value.description ^ " (bytes serialized into shared heap)"

      let measure_remove =
        Value.description ^ " (compressed bytes removed from shared heap)"

      let measure_get =
        Value.description ^ " (bytes deserialized from shared heap)"

      let log_serialize compressed original total =
        let compressed = float compressed in
        let original = float original in
        let total = float total in
        let saved = original -. compressed in
        let ratio = compressed /. original in
        Measure.sample
          (Value.description ^ " (total bytes including header and padding)")
          total;
        Measure.sample
          "ALL bytes (total bytes including header and padding)"
          total;
        Measure.sample measure_add compressed;
        Measure.sample "ALL bytes serialized into shared heap" compressed;
        Measure.sample
          (Value.description ^ " (bytes saved in shared heap due to compression)")
          saved;
        Measure.sample "ALL bytes saved in shared heap due to compression" saved;
        Measure.sample
          (Value.description ^ " (shared heap compression ratio)")
          ratio;
        Measure.sample "ALL bytes shared heap compression ratio" ratio

      let log_deserialize l r =
        let sharedheap = float l in
        Measure.sample measure_get sharedheap;
        Measure.sample "ALL bytes deserialized from shared heap" sharedheap;

        if SMTelemetry.hh_log_level () > 1 then (
          (* value_size is a bit expensive to call this often, so only run with log levels >= 2 *)
          let localheap = float (SMTelemetry.value_size r) in
          Measure.sample
            (Value.description ^ " (bytes allocated for deserialized value)")
            localheap;
          Measure.sample "ALL bytes allocated for deserialized value" localheap
        )

      let log_remove compressed =
        let compressed = float compressed in
        Measure.sample measure_remove compressed;
        Measure.sample "ALL compressed bytes removed from shared heap" compressed;
        ()

      let add key value =
        let (compressed_size, original_size, total_size) = hh_add key value in
        (* compressed_size is a negative number if nothing new was added *)
        if SMTelemetry.hh_log_level () > 0 && compressed_size > 0 then
          log_serialize compressed_size original_size total_size

      let mem key = hh_mem key

      let get (key : KeyHasher.hash) : Value.t option =
        let v = hh_get_and_deserialize key in
        if SMTelemetry.hh_log_level () > 0 then
          Option.iter
            ~f:(fun v -> log_deserialize (hh_get_size key) (Obj.repr v))
            v;
        v

      let remove key =
        let compressed_size = hh_remove key in
        (* hh_remove assumes the key is present *)
        if SMTelemetry.hh_log_level () > 0 then log_remove compressed_size;
        ()

      let move from_key to_key = hh_move from_key to_key

      let get_telemetry (telemetry : Telemetry.t) : Telemetry.t =
        let simple_metric name = (Measure.get_count name, Measure.get_sum name) in
        let diff_metric left_name right_name =
          let diff left right = Option.merge left ~f:( -. ) right in
          let (left_count, left_bytes) = simple_metric left_name in
          let (right_count, right_bytes) = simple_metric right_name in
          (diff left_count right_count, diff left_bytes right_bytes)
        in
        (* Gather counts and sums for these metrics *)
        let metrics =
          [
            ("get", simple_metric measure_get);
            ("add", simple_metric measure_add);
            ("remove", simple_metric measure_remove);
            ("entries", diff_metric measure_add measure_remove);
          ]
        in
        let is_none = function
          | (_, (None, None)) -> true
          | _ -> false
        in
        if List.for_all ~f:is_none metrics then
          telemetry
        else
          let make_obj t (key, (count, bytes)) =
            let count_val = Option.value_map ~default:0 ~f:int_of_float count in
            let bytes_val = Option.value_map ~default:0 ~f:int_of_float bytes in
            Telemetry.object_
              ~key
              ~value:
                (Telemetry.create ()
                 |> Telemetry.int_ ~key:"count" ~value:count_val
                 |> Telemetry.int_ ~key:"bytes" ~value:bytes_val)
              t
          in
          let value = List.fold ~f:make_obj ~init:(Telemetry.create ()) metrics in
          telemetry
          |> Telemetry.object_ ~key:(Value.description ^ "__shared") ~value

      let () =
        SMTelemetry.get_telemetry_list :=
          get_telemetry :: !SMTelemetry.get_telemetry_list;
        ()
    end

type 'a profiled_value =
  | RawValue of 'a
  | ProfiledValue of {
      entry: 'a;
      write_time: float;
    }

module ProfiledBackend : Backend =
  functor
    (Core: Core_sig.COREOPS)
    (Measure: Sys_sig.MEASURE)
    (SMTelemetry: SharedMem_sig.SMTELEMETRY)
    (KeyHasher : KeyHasher)
    (Value : Value)
    ->
    struct
      module Float = Core.Float
      module ProfiledValue = struct
        (** Tagging a value as Raw (the 99.9999% case) only increases its marshalled
            size by 1 byte, and does not change its unmarshalled memory
            representation provided Value.t is a record type containing at least one
            non-float member. *)
        type t = Value.t profiled_value

        let description = Value.description
      end

      module Immediate = ImmediateBackend
          (Core)
          (Measure)
          (SMTelemetry)
          (KeyHasher)
          (ProfiledValue)

      let add x y =
        let sample_rate = SMTelemetry.hh_sample_rate () in
        let entry =
          if
            SMTelemetry.hh_log_level () <> 0
            && Float.(Random.float 1.0 < sample_rate)
          then
            ProfiledValue { entry = y; write_time = Unix.gettimeofday () }
          else
            RawValue y
        in
        Immediate.add x entry

      let get x : Value.t option =
        match Immediate.get x with
        | None -> None
        | Some (RawValue y) -> Some y
        | Some (ProfiledValue { entry; write_time = _ }) ->
          (* EventLogger.(
             log_if_initialized @@ fun () ->
             sharedmem_access_sample
              ~heap_name:Value.description
              ~key:(KeyHasher.to_bytes x)
              ~write_time); *)
          Some entry

      let mem = Immediate.mem

      let remove = Immediate.remove

      let move = Immediate.move
    end

(** Heap that provides direct access to shared memory, but with a layer
    of local changes that allows us to decide whether or not to commit
    specific values. *)
module BackendWithLocalChanges : functor
  (Backend : Backend)
  (Core: Core_sig.COREOPS)
  (Measure: Sys_sig.MEASURE)
  (SMTelemetry: SharedMem_sig.SMTELEMETRY)
  (KeyHasher : KeyHasher)
  (Value : Value)
  -> sig
    include module type of Backend
        (Core)
        (Measure)
        (SMTelemetry)
        (KeyHasher)
        (Value)

    module LocalChanges : sig
      val has_local_changes : unit -> bool

      val push_stack : unit -> unit

      val pop_stack : unit -> unit

      val revert : KeyHasher.hash -> unit

      val commit : KeyHasher.hash -> unit

      val revert_all : unit -> unit

      val commit_all : unit -> unit
    end
  end =
  functor
    (Backend : Backend)
    (Core: Core_sig.COREOPS)
    (Measure: Sys_sig.MEASURE)
    (SMTelemetry: SharedMem_sig.SMTELEMETRY)
    (KeyHasher : KeyHasher)
    (Value : Value)
    ->
    struct
      module Backend = Backend
          (Core)
          (Measure)
          (SMTelemetry)
          (KeyHasher)
          (Value)

      module Option = Core.Option
      module Telemetry = SMTelemetry.Telemetry

      (**
         Represents a set of local changes to the view of the shared memory heap
         WITHOUT materializing to the changes in the actual heap. This allows us to
         make speculative changes to the view of the world that can be reverted
         quickly and correctly.

         A LocalChanges maintains the same invariants as the shared heap. Except
         add are allowed to overwrite filled keys. This is for convenience so we
         do not need to remove filled keys upfront.

         LocalChanges can be committed. This will apply the changes to the previous
         stack, or directly to shared memory if there are no other active stacks.
         Since changes are kept local to the process, this is NOT compatible with
         the parallelism provided by MultiWorker.ml
      *)
      module LocalChanges = struct
        type action =
          (* The value does not exist in the current stack. When committed this
           * action will invoke remove on the previous stack.
          *)
          | Remove
          (* The value is added to a previously empty slot. When committed this
           * action will invoke add on the previous stack.
          *)
          | Add of Value.t
          (* The value is replacing a value already associated with a key in the
           * previous stack. When committed this action will invoke remove then
           * add on the previous stack.
          *)
          | Replace of Value.t

        type t = {
          current: (KeyHasher.hash, action) Hashtbl.t;
          prev: t option;
        }

        let stack : t option ref = ref None

        let has_local_changes () = Option.is_some !stack

        let rec mem stack_opt key =
          match stack_opt with
          | None -> Backend.mem key
          | Some stack ->
            (match Hashtbl.find_opt stack.current key with
             | Some Remove -> false
             | Some _ -> true
             | None -> mem stack.prev key)

        let rec get stack_opt key =
          match stack_opt with
          | None -> Backend.get key
          | Some stack ->
            (match Hashtbl.find_opt stack.current key with
             | Some Remove -> None
             | Some (Replace value | Add value) -> Some value
             | None -> get stack.prev key)

       (*
        * For remove/add it is best to think of them in terms of a state machine.
        * A key can be in the following states:
        *
        *  Remove:
        *    Local changeset removes a key from the previous stack
        *  Replace:
        *    Local changeset replaces value of a key in previous stack
        *  Add:
        *    Local changeset associates a value with a key. The key is not
        *    present in the previous stacks
        *  Empty:
        *    No local changes and key is not present in previous stack
        *  Filled:
        *    No local changes and key has an associated value in previous stack
        *  *Error*:
        *    This means an exception will occur
        *)
       (*
        * Transitions table:
        *   Remove  -> *Error*
        *   Replace -> Remove
        *   Add     -> Empty
        *   Empty   -> *Error*
        *   Filled  -> Remove
        *)
        let remove stack_opt key =
          match stack_opt with
          | None -> Backend.remove key
          | Some stack ->
            (match Hashtbl.find_opt stack.current key with
             | Some Remove -> failwith "Trying to remove a non-existent value"
             | Some (Replace _) -> Hashtbl.replace stack.current key Remove
             | Some (Add _) -> Hashtbl.remove stack.current key
             | None ->
               if mem stack.prev key then
                 Hashtbl.replace stack.current key Remove
               else
                 failwith "Trying to remove a non-existent value")

       (*
        * Transitions table:
        *   Remove  -> Replace
        *   Replace -> Replace
        *   Add     -> Add
        *   Empty   -> Add
        *   Filled  -> Replace
        *)
        let add stack_opt key value =
          match stack_opt with
          | None -> Backend.add key value
          | Some stack ->
            (match Hashtbl.find_opt stack.current key with
             | Some (Remove | Replace _) ->
               Hashtbl.replace stack.current key (Replace value)
             | Some (Add _) -> Hashtbl.replace stack.current key (Add value)
             | None ->
               if mem stack.prev key then
                 Hashtbl.replace stack.current key (Replace value)
               else
                 Hashtbl.replace stack.current key (Add value))

        let move stack_opt from_key to_key =
          match stack_opt with
          | None -> Backend.move from_key to_key
          | Some _stack ->
            assert (mem stack_opt from_key);
            assert (not @@ mem stack_opt to_key);
            let value = Option.value_exn (get stack_opt from_key) in
            remove stack_opt from_key;
            add stack_opt to_key value

        let commit_action changeset key elem =
          match elem with
          | Remove -> remove changeset key
          | Add value -> add changeset key value
          | Replace value ->
            remove changeset key;
            add changeset key value

        (* Public API **)
        let push_stack () =
          stack := Some { current = Hashtbl.create 128; prev = !stack }

        let pop_stack () =
          match !stack with
          | None ->
            failwith "There are no active local change stacks. Nothing to pop!"
          | Some { prev; _ } -> stack := prev

        let revert key =
          match !stack with
          | None -> ()
          | Some changeset -> Hashtbl.remove changeset.current key

        let commit key =
          match !stack with
          | None -> ()
          | Some changeset ->
            (match Hashtbl.find_opt changeset.current key with
             | None -> ()
             | Some r -> commit_action changeset.prev key r)

        let revert_all () =
          match !stack with
          | None -> ()
          | Some changeset -> Hashtbl.clear changeset.current

        let commit_all () =
          match !stack with
          | None -> ()
          | Some changeset ->
            Hashtbl.iter (commit_action changeset.prev) changeset.current

        let get_telemetry (telemetry : Telemetry.t) : Telemetry.t =
          let rec rec_actions_and_depth acc_count acc_depth changeset_opt =
            match changeset_opt with
            | Some changeset ->
              rec_actions_and_depth
                (acc_count + Hashtbl.length changeset.current)
                (acc_depth + 1)
                changeset.prev
            | None -> (acc_count, acc_depth)
          in
          let (actions, depth) = rec_actions_and_depth 0 0 !stack in
          (* We count reachable words of the entire stack, to avoid double-
             counting in cases where a value appears in multiple stack frames.
             If instead we added up reachable words from each frame separately,
             then an item reachable from two frames would be double-counted. *)
          let bytes =
            if SMTelemetry.hh_log_level () > 0 then
              Some (Obj.reachable_words (Obj.repr !stack) * (Sys.word_size / 8))
            else
              None
          in
          if actions = 0 then
            telemetry
          else
            telemetry
            |> Telemetry.object_
              ~key:(Value.description ^ "__stack")
              ~value:
                (Telemetry.create ()
                 |> Telemetry.int_ ~key:"actions" ~value:actions
                 |> Telemetry.int_opt ~key:"bytes" ~value:bytes
                 |> Telemetry.int_ ~key:"depth" ~value:depth)

        let () =
          SMTelemetry.get_telemetry_list :=
            get_telemetry :: !SMTelemetry.get_telemetry_list;
          ()
      end

      let add key value = LocalChanges.(add !stack key value)

      let mem key = LocalChanges.(mem !stack key)

      let get key = LocalChanges.(get !stack key)

      let remove key = LocalChanges.(remove !stack key)

      let move from_key to_key = LocalChanges.(move !stack from_key to_key)
    end

module type Heap = sig
  type key

  type value

  module KeyHasher : KeyHasher with type key = key

  module KeySet : Set.S with type elt = key

  module KeyMap : WrappedMap.S with type key = key

  val add : key -> value -> unit

  val get : key -> value option

  val get_old : key -> value option

  val get_batch : KeySet.t -> value option KeyMap.t

  val get_old_batch : KeySet.t -> value option KeyMap.t

  val remove : key -> unit

  val remove_old : key -> unit

  val remove_batch : KeySet.t -> unit

  val remove_old_batch : KeySet.t -> unit

  val mem : key -> bool

  val mem_old : key -> bool

  val oldify_batch : KeySet.t -> unit

  val revive_batch : KeySet.t -> unit

  module LocalChanges : sig
    val has_local_changes : unit -> bool

    val push_stack : unit -> unit

    val pop_stack : unit -> unit

    val revert_batch : KeySet.t -> unit

    val commit_batch : KeySet.t -> unit

    val revert_all : unit -> unit

    val commit_all : unit -> unit
  end
end

module type LocalCacheLayer = sig
  type key

  type value

  val add : key -> value -> unit

  val get : key -> value option

  val remove : key -> unit

  val clear : unit -> unit

  val get_telemetry_items_and_keys : unit -> Obj.t * key Seq.t
end

module Heap
    (Backend : Backend)
    (Core: Core_sig.COREOPS)
    (Measure: Sys_sig.MEASURE)
    (SMTelemetry: SharedMem_sig.SMTELEMETRY)
    (Key : Key)
    (Value : Value)
  : Heap
    with type key = Key.t
     and type value = Value.t
     and module KeyHasher = MakeKeyHasher (Key)
     and module KeySet = Set.Make (Key)
     and module KeyMap = WrappedMap.Make (Core) (Key) = struct
  module KeyHasher = MakeKeyHasher (Key)
  module KeySet = Set.Make (Key)
  module KeyMap = WrappedMap.Make (Core) (Key)

  (** Stacks that keeps track of local, non-committed changes. If
      no stacks are active, changs will be committed immediately to
      the shared-memory backend *)
  module WithLocalChanges =
    BackendWithLocalChanges
      (Backend)
      (Core)
      (Measure)
      (SMTelemetry)
      (KeyHasher)
      (Value)

  type key = Key.t

  type value = Value.t

  let hash_of_key x = KeyHasher.hash x

  let old_hash_of_key x = KeyHasher.hash_old x

  let add x y = WithLocalChanges.add (hash_of_key x) y

  let get x =
    let hash = hash_of_key x in
    WithLocalChanges.get hash

  let get_old x =
    let old_hash = old_hash_of_key x in
    WithLocalChanges.get old_hash

  let get_batch xs =
    KeySet.fold
      begin
        fun key acc ->
          KeyMap.add key (get key) acc
      end
      xs
      KeyMap.empty

  let get_old_batch xs =
    KeySet.fold
      begin
        fun key acc ->
          KeyMap.add key (get_old key) acc
      end
      xs
      KeyMap.empty

  let remove x =
    let hash = hash_of_key x in
    if WithLocalChanges.mem hash then
      WithLocalChanges.remove hash
    else
      ()

  let remove_old x =
    let old_hash = old_hash_of_key x in
    if WithLocalChanges.mem old_hash then
      WithLocalChanges.remove old_hash
    else
      ()

  let remove_batch xs = KeySet.iter remove xs

  let remove_old_batch xs = KeySet.iter remove_old xs

  let mem x = WithLocalChanges.mem (hash_of_key x)

  let mem_old x = WithLocalChanges.mem (old_hash_of_key x)

  let oldify x =
    if mem x then
      WithLocalChanges.move (hash_of_key x) (old_hash_of_key x)
    else
      ()

  let revive x =
    if mem_old x then (
      remove x;
      WithLocalChanges.move (old_hash_of_key x) (hash_of_key x)
    )

  let oldify_batch xs =
    KeySet.iter
      begin
        fun key ->
          if mem key then
            oldify key
          else
            (* this is weird, semantics of `oldify x` and `oldify_batch {x}` are
               different for some mysterious reason *)
            remove_old key
      end
      xs

  let revive_batch xs =
    KeySet.iter
      begin
        fun key ->
          if mem_old key then
            revive key
          else
            (* this is weird, semantics of `revive x` and `revive {x}` are
                 different for some mysterious reason *)
            remove key
      end
      xs

  module LocalChanges = struct
    include WithLocalChanges.LocalChanges

    let revert_batch keys =
      KeySet.iter
        begin
          fun key ->
            revert (hash_of_key key)
        end
        keys

    let commit_batch keys =
      KeySet.iter
        begin
          fun key ->
            commit (hash_of_key key)
        end
        keys
  end
end

(** Every time a new worker-local cache is created, a clearing function is
    registered for it here. **)
let invalidate_local_caches_callback_list = ref []

let invalidate_local_caches () =
  List.iter (fun callback -> callback ()) !invalidate_local_caches_callback_list

module FreqCache
    (Core: Core_sig.COREOPS)
    (Key : Key)
    (Value : Value)
    (Capacity : Capacity)
  : LocalCacheLayer with type key = Key.t and type value = Value.t = struct
  module List = Core.List

  type key = Key.t

  type value = Value.t

  let (cache : (key, int ref * value) Hashtbl.t) =
    Hashtbl.create (2 * Capacity.capacity)

  let size = ref 0

  let get_telemetry_items_and_keys () =
    (Obj.repr cache, Hashtbl.to_seq_keys cache)

  let clear () =
    Hashtbl.clear cache;
    size := 0

  (** The collection function is called when we reach twice original
      capacity in size. When the collection is triggered, we only keep
      the most frequently used objects.
      So before collection: size = 2 * capacity
      After collection: size = capacity (with the most frequently
      used objects) *)
  let collect () =
    if !size < 2 * Capacity.capacity then
      ()
    else
      let l = ref [] in
      Hashtbl.iter
        begin
          fun key (freq, v) ->
            l := (key, !freq, v) :: !l
        end
        cache;
      Hashtbl.clear cache;
      l := List.sort ~compare:(fun (_, x, _) (_, y, _) -> y - x) !l;
      let i = ref 0 in
      while !i < Capacity.capacity do
        match !l with
        | [] -> i := Capacity.capacity
        | (k, _freq, v) :: rl ->
          Hashtbl.replace cache k (ref 0, v);
          l := rl;
          incr i
      done;
      size := Capacity.capacity;
      ()

  let add x y =
    collect ();
    match Hashtbl.find_opt cache x with
    | Some (freq, y') ->
      incr freq;
      if y' == y then
        ()
      else
        Hashtbl.replace cache x (freq, y)
    | None ->
      incr size;
      let elt = (ref 0, y) in
      Hashtbl.replace cache x elt;
      ()

  let get x =
    match Hashtbl.find_opt cache x with
    | None -> None
    | Some (freq, value) ->
      incr freq;
      Some value

  let remove x =
    if Hashtbl.mem cache x then decr size;
    Hashtbl.remove cache x
end

module OrderedCache (Key : Key) (Value : Value) (Capacity : Capacity) :
  LocalCacheLayer with type key = Key.t and type value = Value.t = struct
  type key = Key.t

  type value = Value.t

  let (cache : (key, value) Hashtbl.t) = Hashtbl.create Capacity.capacity

  let queue = Queue.create ()

  let size = ref 0

  let get_telemetry_items_and_keys () =
    (Obj.repr cache, Hashtbl.to_seq_keys cache)

  let clear () =
    Hashtbl.clear cache;
    size := 0;
    Queue.clear queue;
    ()

  let add x y =
    (if !size >= Capacity.capacity then
       (* Remove oldest element - if it's still around. *)
       let elt = Queue.pop queue in
       if Hashtbl.mem cache elt then (
         decr size;
         Hashtbl.remove cache elt
       ));

    (* Add the new element, but bump the size only if it's a new addition. *)
    Queue.push x queue;
    if not (Hashtbl.mem cache x) then incr size;
    Hashtbl.replace cache x y

  let get x = Hashtbl.find_opt cache x

  let remove x =
    if Hashtbl.mem cache x then begin
      decr size;
      Hashtbl.remove cache x
    end
end

module MultiCache
    (Core: Core_sig.COREOPS)
    (SMTelemetry: SharedMem_sig.SMTELEMETRY)
    (Key : Key)
    (Value : Value)
    (Capacity : Capacity) :
  LocalCacheLayer with type key = Key.t and type value = Value.t = struct
  module Telemetry = SMTelemetry.Telemetry
  type key = Key.t

  type value = Value.t

  (* Young values cache *)
  module L1 = OrderedCache (Key) (Value) (Capacity)

  (* Frequent values cache *)
  module L2 = FreqCache (Core) (Key) (Value) (Capacity)
  module KeySet = Set.Make (Key)

  let add x y =
    L1.add x y;
    L2.add x y

  let get x =
    match L1.get x with
    | None ->
      (match L2.get x with
       | None -> None
       | Some v as result ->
         L1.add x v;
         result)
    | Some v as result ->
      L2.add x v;
      result

  let remove x =
    L1.remove x;
    L2.remove x

  let clear () =
    L1.clear ();
    L2.clear ()

  let get_telemetry_items_and_keys () =
    (* Many items are stored in both L1 (ordered) and L2 (freq) caches.
       We don't want to double-count them.
       So: we'll figure out the reachable words of the (L1,L2) tuple,
       and we'll figure out the set union of keys in both of them. *)
    let (obj1, keys1) = L1.get_telemetry_items_and_keys () in
    let (obj2, keys2) = L2.get_telemetry_items_and_keys () in
    let combined =
      KeySet.empty
      |> KeySet.add_seq keys1
      |> KeySet.add_seq keys2
      |> KeySet.to_seq
    in
    (Obj.repr (obj1, obj2), combined)

  let get_telemetry (telemetry : Telemetry.t) : Telemetry.t =
    let (objs, keys) = get_telemetry_items_and_keys () in
    let count = Seq.fold_left (fun a _ -> a + 1) 0 keys in
    if count = 0 then
      telemetry
    else
      let bytes =
        if SMTelemetry.hh_log_level () > 0 then
          Some (Obj.reachable_words (Obj.repr objs) * Sys.word_size / 8)
        else
          None
      in
      telemetry
      |> Telemetry.object_
        ~key:(Value.description ^ "__local")
        ~value:
          (Telemetry.create ()
           |> Telemetry.int_ ~key:"count" ~value:count
           |> Telemetry.int_opt ~key:"bytes" ~value:bytes)

  let () =
    SMTelemetry.get_telemetry_list :=
      get_telemetry :: !SMTelemetry.get_telemetry_list;
    invalidate_local_caches_callback_list :=
      begin
        fun () ->
          L1.clear ();
          L2.clear ()
      end
      :: !invalidate_local_caches_callback_list
end

(** Create a new value, but append the "__cache" prefix to its description *)
module ValueForCache (Value : Value) = struct
  include Value

  let description = Value.description ^ "__cache"
end

module HeapWithLocalCache
    (Backend : Backend)
    (Core: Core_sig.COREOPS)
    (Measure: Sys_sig.MEASURE)
    (SMTelemetry: SharedMem_sig.SMTELEMETRY)
    (Key : Key)
    (Value : Value)
    (Capacity : Capacity) : sig
  include
    Heap
    with type key = Key.t
     and type value = Value.t
     and module KeyHasher = MakeKeyHasher (Key)
     and module KeySet = Set.Make(Key)
     and module KeyMap = WrappedMap.Make (Core) (Key)

  val write_around : key -> value -> unit

  val get_no_cache : key -> value option

  module Cache : LocalCacheLayer with type key = key and type value = value
end = struct
  module Direct = Heap (Backend) (Core) (Measure) (SMTelemetry) (Key) (Value)

  type key = Direct.key

  type value = Direct.value

  module KeyHasher = Direct.KeyHasher
  module KeySet = Direct.KeySet
  module KeyMap = Direct.KeyMap
  module Cache = MultiCache (Core) (SMTelemetry) (Key) (ValueForCache (Value)) (Capacity)

  let add x y =
    Direct.add x y;
    Cache.add x y

  let get_no_cache = Direct.get

  let write_around x y =
    (* Note that we do not need to do any cache invalidation here because
     * Direct.add is a no-op if the key already exists. *)
    Direct.add x y

  let log_hit_rate ~hit =
    Measure.sample
      (Value.description ^ " (cache hit rate)")
      (if hit then
         1.
       else
         0.);
    Measure.sample
      "(ALL cache hit rate)"
      (if hit then
         1.
       else
         0.)

  let get x =
    match Cache.get x with
    | None ->
      let result =
        match Direct.get x with
        | None -> None
        | Some v as result ->
          Cache.add x v;
          result
      in
      if SMTelemetry.hh_log_level () > 0 then log_hit_rate ~hit:false;
      result
    | Some _ as result ->
      if SMTelemetry.hh_log_level () > 0 then log_hit_rate ~hit:true;
      result

  (* We don't cache old objects, they are not accessed often enough. *)
  let get_old = Direct.get_old

  let get_old_batch = Direct.get_old_batch

  let mem_old = Direct.mem_old

  let mem x =
    match get x with
    | None -> false
    | Some _ -> true

  let get_batch keys =
    KeySet.fold
      begin
        fun key acc ->
          KeyMap.add key (get key) acc
      end
      keys
      KeyMap.empty

  let oldify_batch keys =
    Direct.oldify_batch keys;
    KeySet.iter Cache.remove keys

  let revive_batch keys =
    Direct.revive_batch keys;
    KeySet.iter Cache.remove keys

  let remove x =
    Direct.remove x;
    Cache.remove x

  let remove_old x : unit = Direct.remove_old x

  let remove_batch xs =
    Direct.remove_batch xs;
    KeySet.iter Cache.remove xs

  let remove_old_batch = Direct.remove_old_batch

  let () =
    invalidate_local_caches_callback_list :=
      begin
        fun () ->
          Cache.clear ()
      end
      :: !invalidate_local_caches_callback_list

  module LocalChanges = struct
    let push_stack () =
      Direct.LocalChanges.push_stack ();
      Cache.clear ()

    let pop_stack () =
      Direct.LocalChanges.pop_stack ();
      Cache.clear ()

    let revert_batch keys =
      Direct.LocalChanges.revert_batch keys;
      KeySet.iter Cache.remove keys

    let commit_batch keys =
      Direct.LocalChanges.commit_batch keys;
      KeySet.iter Cache.remove keys

    let revert_all () =
      Direct.LocalChanges.revert_all ();
      Cache.clear ()

    let commit_all () =
      Direct.LocalChanges.commit_all ();
      Cache.clear ()

    let has_local_changes () = Direct.LocalChanges.has_local_changes ()
  end
end