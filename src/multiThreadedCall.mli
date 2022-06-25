(*
 * Copyright (c) 2022, Tatiana Racheva
 * Copyright (c) 2015, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module MakeMultiThreadedCall
    (Bucket : Bucket.BUCKET)
    (Core : Core_sig.COREOPS)
    (Exception : Sys_sig.EXCEPTION)
    (WorkerController : Procs_sig.WORKERCONTROLLER)
  : Procs_sig.MULTITHREADEDCALL