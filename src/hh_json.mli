(*
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

(*
 * Hh_json parsing and pretty printing library.
 *)

module MakeHhJson
    (Core: Core_sig.COREOPS)
  : Sys_sig.HH_JSON

(* TODO: need these? *)
(* module JsonKey : Set.OrderedType with type t = json

   module JSet : Set.S with type elt = json

   module JMap : WrappedMap.S with type key = json *)