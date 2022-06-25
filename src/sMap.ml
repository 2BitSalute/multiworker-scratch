(*
 * Copyright (c) 2022, Tatiana Racheva
 * Copyright (c) 2015, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module MakeSMap (Core: Core_sig.COREOPS) : Collections_sig.MAP with type key = string = struct
  include WrappedMap.Make (Core) (StringKey)

  let pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit =
    (fun pp_data -> make_pp (fun fmt s -> Format.fprintf fmt "%S" s) pp_data)

  let show pp_data x = Format.asprintf "%a" (pp pp_data) x
end