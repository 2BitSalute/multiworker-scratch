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

module type LIST = sig
  val iter: 'a list -> f:('a -> unit) -> unit

  val fold_left: 'b list -> f:('a -> 'b -> 'a) -> init:'a -> 'a
end

module type OPTION = sig
  type 'a t = 'a option
  val iter: 'a t -> f:('a -> unit) -> unit
  val is_none: 'a t -> bool
  val value: default:'a -> 'a option -> 'a
end

module type OUTCHANNEL = sig
  type t = out_channel
  val create : string -> t
  val close : t -> unit
end

module type INCHANNEL = sig
  type t = in_channel
  val create : string -> t
  val input_line : t -> string option
  val close : t -> unit
end

module type COREOPS = sig
  module Array : ARRAY
  module List : LIST
  module Option : OPTION
  module Out_channel : OUTCHANNEL
  module In_channel : INCHANNEL
end