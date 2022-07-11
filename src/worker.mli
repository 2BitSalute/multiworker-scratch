(*
 * Copyright (c) 2022, Tatiana Racheva
 * Copyright (c) 2015, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module MakeWorker
    (Core: Core_sig.COREOPS)
    (Sys_utils: Sys_sig.SYSUTILS)
    (Daemon: Sys_sig.DAEMON)
    (Exception: Sys_sig.EXCEPTION)
    (Fork: Sys_sig.FORK)
    (Marshal_tools: Marshal_tools_sig.MARSHAL_TOOLS)
    (Measure: Sys_sig.MEASURE)
    (PidLog: Sys_sig.PIDLOG)
    (SharedMem: SharedMem_sig.SHAREDMEM)
    (Telemetry: Sys_sig.TELEMETRY)
    (Timeout: Sys_sig.TIMEOUT)
    (WorkerCancel: WorkerCancel.WORKERCANCEL)
  : Procs_sig.WORKER
