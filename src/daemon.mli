(*
 * Copyright (c) Tatiana Racheva
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module MakeDaemon
    (Core: Core_sig.COREOPS)
    (Exception: Sys_sig.EXCEPTION)
    (Fork: Sys_sig.FORK)
    (PidLog: Sys_sig.PIDLOG)
    (Timeout: Sys_sig.TIMEOUT)
    (Sys_utils: Sys_sig.SYSUTILS)
  : Sys_sig.DAEMON