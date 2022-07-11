(*
 * Copyright (c) 2022, Tatiana Racheva
 * Copyright (c) 2015, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module MakeWorkerController
    (Core: Core_sig.COREOPS)
    (Exception: Sys_sig.EXCEPTION)
    (Marshal_tools: Marshal_tools_sig.MARSHAL_TOOLS)
    (Measure: Sys_sig.MEASURE)
    (PidLog: Sys_sig.PIDLOG)
    (SharedMem : SharedMem_sig.SHAREDMEM)
    (Sys_utils: Sys_sig.SYSUTILS)
    (Timeout: Sys_sig.TIMEOUT)
    (Worker : Procs_sig.WORKER)
    (WorkerCancel: WorkerCancel.WORKERCANCEL)
  : Procs_sig.WORKERCONTROLLER
