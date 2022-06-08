(*
 * Copyright (c) 2022, Tatiana Racheva
 * Copyright (c) 2015, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type request = Request of (serializer -> unit)

and serializer = { send: 'a. 'a -> unit }

type subprocess_job_status = Subprocess_terminated of Unix.process_status

val win32_worker_main :
  ('a -> 'b) ->
  'a * Unix.file_descr option ->
  request Daemon.in_channel * 'c Daemon.out_channel ->
  'd

val unix_worker_main :
  ('a -> 'b) ->
  'a * Unix.file_descr option ->
  request Daemon.in_channel * 'c Daemon.out_channel ->
  'd

val unix_worker_main_no_clone :
  ('a -> 'b) ->
  'a * Unix.file_descr option ->
  request Daemon.in_channel * 'c Daemon.out_channel ->
  'd
