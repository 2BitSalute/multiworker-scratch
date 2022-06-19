(*
 * Copyright (c) 2022, Tatiana Racheva
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type TIMER = sig
  type t

  (* Will invoke callback () after interval seconds *)
  val set_timer : interval:float -> callback:(unit -> unit) -> t

  (* Will prevent a future timer from firing *)
  val cancel_timer : t -> unit
end

(* TODO: eliminate unused functions *)
module type TIMEOUT = sig
  type t

  (* The function `with_timeout` executes 'do_' for at most 'timeout'
     seconds. If the `timeout` is reached, the `on_timeout` is executed
     if available, otherwise the `Timeout` exception is raised.

     On Unix platform, this function is based on `SIGALRM`. On Windows
     platform, this is based on the equivalent of `select`. Hence, this
     module exports variant of basic input functions, adding them a
     `timeout` parameter. It should correspond to the parameter of the
     `do_` function.

     For `do_` function based only on computation (and not I/O), you
     should call the `check_timeout` function on a regular
     basis. Otherwise, on Windows, the timeout will never be detected.
     On Unix, the function `check_timeout` is no-op.

     On Unix, the type `in_channel` is in fact an alias for
     `Stdlib.in_channel`.

  *)
  val with_timeout : timeout:int -> on_timeout:(unit -> 'a) -> do_:(t -> 'a) -> 'a

  val check_timeout : t -> unit

  type in_channel

  val open_in : string -> in_channel

  val close_in : in_channel -> unit

  val close_in_noerr : in_channel -> unit

  val in_channel_of_descr : Unix.file_descr -> in_channel

  val descr_of_in_channel : in_channel -> Unix.file_descr

  val select :
    ?timeout:t ->
    Unix.file_descr list ->
    Unix.file_descr list ->
    Unix.file_descr list ->
    float ->
    Unix.file_descr list * Unix.file_descr list * Unix.file_descr list

  val input : ?timeout:t -> in_channel -> bytes -> int -> int -> int

  val really_input : ?timeout:t -> in_channel -> bytes -> int -> int -> unit

  val input_char : ?timeout:t -> in_channel -> char

  val input_line : ?timeout:t -> in_channel -> string

  val input_value : ?timeout:t -> in_channel -> 'a

  val open_process : string -> string array -> in_channel * out_channel

  val open_process_in : string -> string array -> in_channel

  val close_process_in : in_channel -> Unix.process_status

  val read_process :
    timeout:int ->
    on_timeout:(unit -> 'a) ->
    reader:(t -> in_channel -> out_channel -> 'a) ->
    string ->
    string array ->
    'a

  val open_connection : ?timeout:t -> Unix.sockaddr -> in_channel * out_channel

  val read_connection :
    timeout:int ->
    on_timeout:(unit -> 'a) ->
    reader:(t -> in_channel -> out_channel -> 'a) ->
    Unix.sockaddr ->
    'a

  val shutdown_connection : in_channel -> unit

  (* Some silly people like to catch all exceptions. This means they need to explicitly detect and
   * reraise the timeout exn. *)
  val is_timeout_exn : t -> exn -> bool
end

module type DAEMON = sig
  module Timeout : TIMEOUT

  (** Type-safe versions of the channels in Pervasives/Stdlib. *)

  type 'a in_channel

  type 'a out_channel

  type ('in_, 'out) channel_pair = 'in_ in_channel * 'out out_channel

  val to_channel : 'a out_channel -> ?flags:Marshal.extern_flags list -> ?flush:bool -> 'a -> unit

  val from_channel : ?timeout:Timeout.t -> 'a in_channel -> 'a

  val flush : 'a out_channel -> unit

  (* This breaks the type safety, but is necessary in order to allow select() *)
  val descr_of_in_channel : 'a in_channel -> Unix.file_descr

  val descr_of_out_channel : 'a out_channel -> Unix.file_descr

  val cast_in : 'a in_channel -> Timeout.in_channel

  val cast_out : 'a out_channel -> Stdlib.out_channel

  val close_out : 'a out_channel -> unit

  val output_string : 'a out_channel -> string -> unit

  val close_in : 'a in_channel -> unit

  val input_char : 'a in_channel -> char

  val input_value : 'a in_channel -> 'b

  (** Spawning new process *)

  (* In the absence of 'fork' on Windows, its usage must be restricted
     to Unix specifics parts.

     This module provides a mechanism to "spawn" new instance of the
     current program, but with a custom entry point (e.g. DfindServer,
     ...). Then, alternate entry points should not depend on global
     references that may not have been (re)initialised in the new
     process.

     All required data must be passed through the typed channels.
     associated to the spawned process.

  *)

  (* Alternate entry points *)
  type ('param, 'input, 'output) entry

  (* Alternate entry points must be registered at toplevel, i.e.
     every call to `Daemon.register_entry_point` must have been
     evaluated when `Daemon.check_entry_point` is called at the
     beginning of `ServerMain.start`. *)
  val register_entry_point :
    string -> ('param -> ('input, 'output) channel_pair -> unit) -> ('param, 'input, 'output) entry

  val name_of_entry : ('param, 'input, 'output) entry -> string

  (* Handler upon spawn and forked process. *)
  type ('in_, 'out) handle = {
    channels: ('in_, 'out) channel_pair;
    pid: int;
  }

  (* for unit tests *)
  val devnull : unit -> ('a, 'b) handle

  val fd_of_path : string -> Unix.file_descr

  val null_fd : unit -> Unix.file_descr

  (* Fork and run a function that communicates via the typed channels *)
  val fork :
    ?channel_mode:[ `pipe | `socket ] ->
    (* Where the daemon's output should go *)
    Unix.file_descr * Unix.file_descr ->
    ('param -> ('input, 'output) channel_pair -> unit) ->
    'param ->
    ('output, 'input) handle

  (* Spawn a new instance of the current process, and execute the
     alternate entry point. *)
  val spawn :
    ?channel_mode:[ `pipe | `socket ] ->
    ?name:string ->
    (* Where the daemon's input and output should go *)
    Unix.file_descr * Unix.file_descr * Unix.file_descr ->
    ('param, 'input, 'output) entry ->
    'param ->
    ('output, 'input) handle

  (* Close the typed channels associated to a 'spawned' child. *)
  val close : ('a, 'b) handle -> unit

  (* Kill a 'spawned' child and close the associated typed channels. *)
  val force_quit : ('a, 'b) handle -> unit

  (* Main function, that execute a alternate entry point.
     It should be called only once. Just before the main entry point.
     This function does not return when a custom entry point is selected. *)
  val check_entry_point : unit -> unit
end

module type SYSUTILS = sig
  val get_gc_time : unit -> float * float
  val executable_path : unit -> string
  val mkdir_no_fail : string -> unit
  val null_path : string
  val pid_of_handle : int -> int

  (* Calls Unix.select but ignores EINTR, i.e. retries select with
     an adjusted timout upon EINTR.
     We implement timers using sigalarm which means selects can be
     interrupted. This is a wrapper around EINTR which continues the select if it
     gets interrupted by a signal *)
  val select_non_intr :
    Unix.file_descr list ->
    Unix.file_descr list ->
    Unix.file_descr list ->
    float ->
    Unix.file_descr list * Unix.file_descr list * Unix.file_descr list

  val start_gc_profiling : unit -> unit

  val temp_dir_name : string
  val terminate_process : int -> unit
  (* LWT installs a sigchld handler. If using LWT, the old pattern of
     fork & waitpid will hit an EINTR when the forked process dies and the parent
     gets a sigchld signal.

     Note: this is only a problem if you're not using the WNOHANG flag, since
     EINTR isn't thrown for WNOHANG *)
  val waitpid_non_intr : Unix.wait_flag list -> int -> int * Unix.process_status
  val with_umask : int -> (unit -> 'a) -> 'a
end

module type PIDLOG = sig
  val log : ?reason:string -> ?no_fail:bool -> int -> unit
  val close : unit -> unit
end

module type FORK = sig
  val fork : unit -> int
end

(* TODO: combine with the STRING module? *)
module type STRINGUTILS = sig
  val split_on_newlines : string -> string list

  (* If s is longer than length len, return a copy of s truncated to length len. *)
  val truncate : int -> string -> string
end

module type EXCEPTION = sig
  type t [@@deriving show]
  val wrap : exn -> t
(*
     val wrap_unraised : ?frames:int -> exn -> t

     val unwrap : t -> exn
*)
  val reraise : t -> 'a
(*
     val to_exn : t -> exn
*)
  val to_string : t -> string

  val get_ctor_string : t -> string

  val get_backtrace_string : t -> string
(*
     val get_current_callstack_string : int -> string

     val record_backtrace : bool -> unit
*)
  val clean_stack : string -> string
end

module type UTILS = sig
  type callstack = Callstack of string [@@deriving show]

  val try_finally : f:(unit -> 'a) -> finally:(unit -> unit) -> 'a
  val with_context :
    enter:(unit -> unit) -> exit:(unit -> unit) -> do_:(unit -> 'a) -> 'a
end

module type HH_JSON = sig
  (* TODO - implement and/or remove unused functions and refactor so we can use a more generic JSON library *)
  type json =
    | JSON_Object of (string * json) list
    | JSON_Array of json list
    | JSON_String of string
    | JSON_Number of string
    | JSON_Bool of bool
    | JSON_Null

  exception Syntax_error of string

  (* A function conforming to `show`'s pretty printing signature;
      calls `json_to_multiline` internally
  *)
  val pp_json : Format.formatter -> json -> unit

  (** Calling this with [~pretty:true] is the same as calling [json_to_multiline] *)
  val json_to_string : ?sort_keys:bool -> ?pretty:bool -> json -> string

  val json_to_multiline : ?sort_keys:bool -> json -> string

  val json_to_output : out_channel -> json -> unit

  val json_to_multiline_output : out_channel -> json -> unit

  val json_of_string : ?strict:bool -> string -> json

  val json_of_file : ?strict:bool -> string -> json

  (** Truncate fields of a json object.
      String fields will be truncated according to [max_string_length].
      [max_object_child_count] determines the maximum number of children of objects.
      [max_array_elt_count] determines the maximum number of array elements.
      Fields at depth greater than [max_depth] will be removed.
      [max_total_count] is the maximum total number of children of arrays and objects
      aggregated over all arrays and objects *)
  val json_truncate :
    ?max_string_length:int ->
    ?max_object_child_count:int ->
    ?max_array_elt_count:int ->
    ?max_depth:int ->
    ?max_total_count:int ->
    ?has_changed:bool ref ->
    json ->
    json

  val json_truncate_string :
    ?max_string_length:int ->
    ?max_child_count:int ->
    ?max_depth:int ->
    ?max_total_count:int ->
    ?allowed_total_length:int ->
    ?if_reformat_multiline:bool ->
    string ->
    string

  val print_json_endline : ?pretty:bool -> json -> unit

  val prerr_json_endline : ?pretty:bool -> json -> unit

  val get_object_exn : json -> (string * json) list

  val get_array_exn : json -> json list

  val get_string_exn : json -> string

  val get_number_exn : json -> string

  val get_number_int_exn : json -> int

  val get_bool_exn : json -> bool

  val opt_string_to_json : string option -> json

  val opt_int_to_json : int option -> json

  val int_ : int -> json

  val float_ : float -> json

  val string_ : string -> json

  val bool_ : bool -> json

  val opt_ : ('a -> json) -> 'a option -> json

  val array_ : ('a -> json) -> 'a list -> json

  (* Types and functions for monadic API for traversing a JSON object. *)

  type json_type =
    | Object_t
    | Array_t
    | String_t
    | Number_t
    | Integer_t
    | Bool_t
end

module type TELEMETRY = sig
  module Exception : EXCEPTION
  module Hh_json : HH_JSON

  type t [@@deriving show]

  val create : unit -> t

  val to_string : ?pretty:bool -> t -> string

  val to_json : t -> Hh_json.json

  (** `diff ~all current ~prev` is for when `current` and `prev` have the same structure.
      It produces a hybrid telemetry object where, element by element, if they're the same
      then we only see the current element, but if they're different then we see both.
      (If you pass ~all:true then it hides elements that have remained the same.)
      It works with nested telemetry objects. In places where the structure differs,
      only `current` is kept. *)
  val diff : all:bool -> ?suffix_keys:bool -> t -> prev:t -> t

  val add : t -> t -> t

  val string_ : ?truncate:int -> key:string -> value:string -> t -> t

  val string_opt : ?truncate:int -> key:string -> value:string option -> t -> t

  val string_list :
    ?truncate_list:int ->
    ?truncate_each_string:int ->
    key:string ->
    value:string list ->
    t ->
    t

  val object_list : key:string -> value:t list -> t -> t

  val bool_ : key:string -> value:bool -> t -> t

  val int_ : key:string -> value:int -> t -> t

  val int_opt : key:string -> value:int option -> t -> t

  val int_list : ?truncate_list:int -> key:string -> value:int list -> t -> t

  val json_ : key:string -> value:Hh_json.json -> t -> t

  val object_ : key:string -> value:t -> t -> t

  val object_opt : key:string -> value:t option -> t -> t

  val duration : ?key:string -> start_time:float -> t -> t

  val float_ : key:string -> value:float -> t -> t

  val float_opt : key:string -> value:float option -> t -> t

  val error : e:string -> t -> t

  val error_with_stack : stack:string -> e:string -> t -> t

  val exception_ : e:Exception.t -> t -> t

  val quick_gc_stat : unit -> t
end

module type MEASURE = sig
  module Telemetry : TELEMETRY

  type record

  type record_data

  val create : unit -> record

  val push_global : unit -> unit

  val pop_global : unit -> record

  val serialize : record -> record_data

  val deserialize : record_data -> record

  val track_distribution : ?record:record -> string -> bucket_size:float -> unit

  val sample : ?record:record -> ?weight:float -> string -> float -> unit

  val time : ?record:record -> string -> (unit -> 'a) -> 'a

  val delete : ?record:record -> string -> unit

  val merge : ?record:record -> record -> unit

  val get_sum : ?record:record -> string -> float option

  val get_mean : ?record:record -> string -> float option

  val get_count : ?record:record -> string -> float option

  val get_max : ?record:record -> string -> float option

  val print_entry_stats :
    ?record:record -> ?print_raw:(string -> unit) -> string -> unit

  val print_stats : ?record:record -> ?print_raw:(string -> unit) -> unit -> unit

  val stats_to_telemetry : ?record:record -> unit -> Telemetry.t

  val print_entry_distribution : ?record:record -> string -> unit

  val print_distributions : ?record:record -> unit -> unit

end