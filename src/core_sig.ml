(*
 * Copyright (c) 2022, Tatiana Racheva
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type ARRAY = sig
  val length: 'a array -> int
  val of_list: 'a list -> 'a array
  val sub: 'a array -> pos:int -> len:int -> 'a array
  val to_list: 'a array -> 'a list
end
;;

module type LIST = sig
  val exists : 'a list -> f:('a -> bool) -> bool
  val filter : 'a list -> f:('a -> bool) -> 'a list
  val fold : 'a list -> init:'accum -> f:('accum -> 'a -> 'accum) -> 'accum

  (* TODO: this is weird, take it out *)
  val fold_left_env : 'a -> 'b list -> init:'c -> f:('a -> 'c -> 'b -> 'a * 'c) -> 'a * 'c

  val fold_right : 'a list -> f:('a -> 'b -> 'b) -> init:'b -> 'b

  val for_all : 'a list -> f:('a -> bool) -> bool
  val hd : 'a list -> 'a option
  val is_empty : 'a list -> bool
  val iter : 'a list -> f:('a -> unit) -> unit
  val length : 'a list -> int
  val map : 'a list -> f:('a -> 'b) -> 'b list
  val mem : 'a list -> 'a -> equal:('a -> 'a -> bool) -> bool
  val rev : 'a list -> 'a list
  val rev_filter_map : 'a list -> f:('a -> 'b option) -> 'b list
  val sort : 'a list -> compare:('a -> 'a -> int) -> 'a list
  val take : 'a list -> int -> 'a list
end
;;

module type OPTION = sig
  type 'a t = 'a option
  val iter: 'a t -> f:('a -> unit) -> unit
  val is_none: 'a t -> bool
  val is_some: 'a t -> bool
  val merge : 'a t -> 'a t -> f:('a -> 'a -> 'a) -> 'a t
  val value: default:'a -> 'a option -> 'a
  val value_exn: 'a option -> 'a
  val value_map: default:'b -> f:('a -> 'b) -> 'a option -> 'b
end
;;

module type OUTCHANNEL = sig
  type t = out_channel
  val open_for_append : string -> t
  val create : string -> t
  val close : t -> unit
end
;;

module type INCHANNEL = sig
  type t = in_channel
  val create : string -> t
  val input_line : t -> string option
  val close : t -> unit
end
;;

module type EXN = sig
  val to_string : exn -> string
end
;;

module type STRING = sig
  val concat : sep:string -> string list -> string
  val hash : string -> int
end
;;

module type FLOAT = sig
  type t = float

  val ( > ) : t -> t -> bool
  val ( < ) : t -> t -> bool
  val ( = ) : t -> t -> bool
  val ( <= ) : t -> t -> bool

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val min_value : t
  val max_value : t

  val round_down : t -> t
  val sqrt : t -> t
end

module type RESULT = sig
  type ('ok, 'a) t = ('ok, 'a) result

  val ok : ('ok, 'a) t -> 'ok option
end

module type COREOPS = sig
  module Array : ARRAY
  module Exn : EXN
  module Float : FLOAT
  module In_channel : INCHANNEL
  module List : LIST
  module Option : OPTION
  module Out_channel : OUTCHANNEL
  module Result : RESULT
  module String : STRING
end