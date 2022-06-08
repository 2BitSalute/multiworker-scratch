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

module type COREOPS = sig
  module Array : ARRAY
end