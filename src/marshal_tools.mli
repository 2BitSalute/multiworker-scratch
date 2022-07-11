(*
 * Copyright (c) 2022, Tatiana Racheva
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 *)

module MakeMarshalTools
    (Exception : Sys_sig.EXCEPTION)
    (Timeout: Sys_sig.TIMEOUT)
    (Utils : Sys_sig.UTILS)
  : Marshal_tools_sig.MARSHAL_TOOLS