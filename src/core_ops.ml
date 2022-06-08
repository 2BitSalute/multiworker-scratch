(*
 * Copyright (c) 2022, Tatiana Racheva
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module CoreOps : Core_sig.COREOPS = struct
  module Array = struct
    let length = Array.length
    let of_list = Array.of_list
    let sub array ~pos ~len = Array.sub array pos len
    let to_list = Array.to_list
  end
end