(*
 * Copyright (c) 2022, Tatiana Racheva
 * Copyright (c) 2015, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module MakeMeasure
    (Core: Core_sig.COREOPS)
    (SMap: Collections_sig.MAP with type key = string)
    (Telemetry: Sys_sig.TELEMETRY)
  : Sys_sig.MEASURE