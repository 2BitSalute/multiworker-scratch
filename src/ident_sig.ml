(*
 * Copyright (c) 2022, Tatiana Racheva
 * Copyright (c) 2015, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type IDENT = sig

  type t = int [@@deriving eq]

  val compare : t -> t -> int

  val track_names : bool ref

  val tmp : unit -> t

  val to_string : t -> string

  val debug : ?normalize:(int -> int) -> t -> string

  val get_name : t -> string

  val set_name : t -> string -> unit

  val make : string -> t

  val pp : Format.formatter -> t -> unit

  val not_equal : t -> t -> bool

  val from_string_hash : string -> t

end