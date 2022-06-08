(*
 * Copyright (c) 2022, Tatiana Racheva
 * Copyright (c) 2015, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(****************************************************************************)
(* Moduling Making buckets.
 * When we parallelize, we need to create "buckets" of tasks for the
 * workers.
 * Given a list of files, we want to split it up into buckets such that
 * every worker is busy long enough. If the bucket is too big, it hurts
 * load balancing, if it is too small, the overhead in synchronization time
 * hurts *)
(****************************************************************************)

(* TODO: move to _sig.ml *)
module type BUCKET = sig
  (* The general protocol for a next function is to return either Wait (indicating
     that workers should wait until more elements are added to the workload), or
     Job of a bucket, or Done to indicate there is no more work. *)
  type 'a bucket =
    | Job of 'a
    | Wait
    | Done

  val is_done : 'a bucket -> bool

  type 'a next = unit -> 'a bucket

  val set_max_bucket_size : int -> unit

  val max_size : unit -> int

  (** Given a number of jobs, number of workers, and a maximum bucket size, will
      calculate the optimal bucket size to get the work done as quickly as
      possible.

      Specifically, if the number of jobs is less than the number of workers times
      the maximum bucket size, smaller bucket sizes will be returned in order to
      utilize as many workers as possible. *)
  val calculate_bucket_size :
    num_jobs:int -> num_workers:int -> max_size:int -> int

  (* Makes a bucket out of a list, without regard for number of workers or the
     size of the list.  *)
  val of_list : 'a list -> 'a list bucket

  val make :
    num_workers:int ->
    ?progress_fn:(total:int -> start:int -> length:int -> unit) ->
    ?max_size:int ->
    'a list ->
    'a list next

  type 'a of_n = {
    work: 'a;
    bucket: int;
    total: int;
  }

  (**
   * Make n buckets (where n = "buckets").
   *
   * The "split" function provides the workload for the k'th bucket.
  *)
  val make_n_buckets : buckets:int -> split:(bucket:int -> 'a) -> 'a of_n next

  (* Specialized version to split into lists only. *)
  val make_list :
    num_workers:int ->
    ?progress_fn:(total:int -> start:int -> length:int -> unit) ->
    ?max_size:int ->
    'a list ->
    unit ->
    'a list
end

module MakeBucket (Core: Core_sig.COREOPS) : BUCKET = struct
  module Array = Core.Array

  type 'a bucket =
    | Job of 'a
    | Wait
    | Done

  let is_done = function
    | Done -> true
    | Wait
    | Job _ ->
      false

  type 'a next = unit -> 'a bucket

  let max_size_ref = ref 500

  let max_size () = !max_size_ref

  let set_max_bucket_size x = max_size_ref := x

  let calculate_bucket_size ~num_jobs ~num_workers ~max_size =
    if num_jobs < num_workers * max_size then
      max 1 (1 + (num_jobs / num_workers))
    else
      max_size

  let make_ progress_fn (bucket_size:int) jobs =
    let i = ref 0 in
    fun () ->
      let bucket_size = min (Array.length jobs - !i) bucket_size in
      progress_fn ~start:!i ~length:bucket_size;
      let result = Array.sub jobs ~pos:!i ~len:bucket_size in
      i := bucket_size + !i;
      Array.to_list result

  let make_list ~num_workers ?progress_fn ?max_size jobs =
    let progress_fn =
      Option.value ~default:(fun ~total:_ ~start:_ ~length:_ -> ()) progress_fn
    in
    let max_size = Option.value max_size ~default:!max_size_ref in
    let jobs = Array.of_list jobs in
    let bucket_size =
      calculate_bucket_size ~num_jobs:(Array.length jobs) ~num_workers ~max_size
    in
    make_ (progress_fn ~total:(Array.length jobs)) bucket_size jobs

  let of_list = function
    | [] -> Done
    | wl -> Job wl

  let make ~num_workers ?progress_fn ?max_size jobs =
    let max_size = Option.value max_size ~default:!max_size_ref in
    let maker = make_list ~num_workers ?progress_fn ~max_size jobs in
    (fun () -> of_list (maker ()))

  type 'a of_n = {
    work: 'a;
    bucket: int;
    total: int;
  }

  let make_n_buckets ~buckets ~split =
    let next_bucket = ref 0 in
    fun () ->
      let current = !next_bucket in
      incr next_bucket;
      if current < buckets then
        Job { work = split ~bucket:current; bucket = current; total = buckets }
      else
        Done
end