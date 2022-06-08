(*
  Copyright (c) 2022, Tatiana Racheva
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