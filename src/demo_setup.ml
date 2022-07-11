(* Sets up all of the modules *)

module Core = Core_ops.CoreOps

module String_utils = String_utils.String_utils

module Exception = Exception.MakeException
    (String_utils)

module Timer = Timer.MakeTimer
    (Core)
    (Exception)

module Utils = Sys_utils.MakeUtils
    (Exception)

module Sys_utils = Sys_utils.MakeSysUtils
    (Core)
    (Exception)
    (Utils)

module Timeout : Sys_sig.TIMEOUT = Timeout.MakeTimeout
    (Core)
    (Exception)
    (Timer)
    (Sys_utils)
    () (* The unit module parameter indicates that this is a generative functor *)

module Marshal_tools = Marshal_tools.MakeMarshalTools
    (Exception)
    (Timeout)
    (Utils)

module Hh_json = Hh_json.MakeHhJson
    (Core)

module Telemetry = Telemetry.MakeTelemetry
    (Core)
    (Exception)
    (Hh_json)
    (String_utils)

module SMap = SMap.MakeSMap

    (Core)
module Measure = Measure.MakeMeasure
    (Core)
    (SMap)
    (Telemetry)

module PidLog = PidLog.MakePidLog
    (Core)
    (Sys_utils)

module Fork = Fork.MakeFork
    (Core)
    (PidLog)

module Daemon = Daemon.MakeDaemon
    (Core)
    (Exception)
    (Fork)
    (PidLog)
    (Timeout)
    (Sys_utils)

module WorkerCancel = WorkerCancel.MakeWorkerCancel
    (Utils)

module SharedMem = SharedMem.SharedMem

module Worker = Worker.MakeWorker
    (Core)
    (Sys_utils)
    (Daemon)
    (Exception)
    (Fork)
    (Marshal_tools)
    (Measure)
    (PidLog)
    (SharedMem)
    (Telemetry)
    (Timeout)
    (WorkerCancel)

module WorkerController = WorkerController.MakeWorkerController
    (Core)
    (Exception)
    (Marshal_tools)
    (Measure)
    (PidLog)
    (SharedMem)
    (Sys_utils)
    (Timeout)
    (Worker)
    (WorkerCancel)

module Exit = Exit_status.Exit

module Exit_status = Exit_status.Exit_status

module Bucket = Bucket.MakeBucket
    (Core)

module MultiThreadedCall = MultiThreadedCall.MakeMultiThreadedCall
    (Bucket)
    (Core)
    (Exception)
    (WorkerController)

module MultiWorker = MultiWorker.MakeMultiWorker
    (MultiThreadedCall)

module WorkerControllerEntryPoint = WorkerControllerEntryPoint.MakeWorkerControllerEntryPoint
    (MultiWorker.MultiThreadedCall.WorkerController.Worker)
    (MultiWorker.MultiThreadedCall.WorkerController)