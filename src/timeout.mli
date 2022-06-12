(*
 * Copyright (c) 2022, Tatiana Racheva
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Helpers for handling timeout, in particular input timeout. *)

(* TODO: what's the deal with the need for a generative functor here?
   https://stackoverflow.com/questions/39656203/parameterized-functors/
   suggests a different approach *)
module MakeTimeout
    (Core: Core_sig.COREOPS)
    (Exception: Sys_sig.EXCEPTION)
    (Timer: Sys_sig.TIMER)
    (Sys_utils: Sys_sig.SYSUTILS)
    ()
  : Sys_sig.TIMEOUT