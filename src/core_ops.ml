(*
 * Copyright (c) 2022, Tatiana Racheva
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module CoreOps : Core_sig.COREOPS = struct
  module Array = struct
    let length = Array.length
    let of_list = Array.of_list
    let sub array ~pos ~len = Array.sub array pos len
    let to_list = Array.to_list
  end;;

  module List = struct
    let iter l ~f = List.iter f l
    let exists l ~f =
      let rec aux l =
        match l with
        | head :: rest ->
          if f head then true
          else aux rest
        | [] -> false
      in
      aux l

    let filter_rev l ~f =
      let rec aux acc l =
        match l with
        | head :: rest ->
          if f head then
            aux (head :: acc) rest
          else
            aux acc rest
        | [] -> acc
      in
      aux [] l

    let filter l ~f = List.rev (filter_rev l ~f)

    let fold l ~init ~f = List.fold_left f init l

    let rec fold_left_env env l ~init ~f =
      match l with
      | [] -> (env, init)
      | x :: xs ->
        let (env, init) = f env init x in
        fold_left_env env xs ~init ~f

    let fold_right l ~f ~init = List.fold_right f l init

    let for_all l ~f =
      let rec aux l =
        match l with
        | head :: rest ->
          if f head then
            aux rest
          else false
        | [] -> true
      in
      aux l

    let hd l =
      try
        Some (List.hd l)
      with _ -> None

    let is_empty l =
      match l with
      | [] -> true
      | _ -> false

    let length = List.length

    let map l ~f = List.map f l

    let mem l el ~equal =
      let rec aux l =
        match l with
        | head :: rest ->
          if equal el head then
            true
          else
            aux rest
        | [] -> false
      in
      aux l

    let rev = List.rev

    let rev_filter_map l ~f =
      let rec aux acc l =
        match l with
        | head :: rest ->
          begin
            match f head with
            | Some v -> aux ( v :: acc ) rest
            | None -> aux acc rest
          end
        | [] -> acc
      in
      aux [] l

    let sort l ~compare = List.sort compare l

    (* TODO: test that the logic is correct or replace *)
    let take l n =
      let rec aux ~acc l n =
        if n > 0 then
          match l with
          | head :: rest ->
            aux ~acc:(head :: acc) rest (n - 1)
          | [] -> acc
        else
          acc
      in
      List.rev (aux ~acc:[] l n)
    ;;
  end
  ;;

  module Option = struct
    type 'a t = 'a option

    let iter value ~f =
      match value with
      | Some value -> f value
      | None -> ()

    let is_none value =
      match value with
      | Some _ -> true
      | None -> false

    let is_some value = not (is_none value)

    let merge a b ~f =
      match a, b with
      | None, x | x, None -> x
      | Some a, Some b -> Some (f a b)
    ;;

    let value ~default v =
      match v with
      | Some v -> v
      | None -> default

    let value_exn v =
      match v with
      | Some v -> v
      | None -> failwith "TODO: proper value_exn exception"

    let value_map ~default ~f v =
      match v with
      | Some v -> f v
      | None -> default
  end
  ;;

  module Out_channel = struct
    type t = out_channel

    (* Sorta copied from https://github.com/janestreet/stdio/blob/master/src/out_channel.ml *)
    let create file =
      let perm = 0o666 in
      let flags = [ Open_wronly; Open_creat ] in
      open_out_gen flags perm file

    let close = close_out
  end
  ;;

  module In_channel = struct
    type t = in_channel

    (* Sorta copied from https://github.com/janestreet/stdio/blob/master/src/in_channel.ml *)
    let create file =
      let perm = 0o000 in
      let flags = [ Open_rdonly; Open_binary ] in
      open_in_gen flags perm file

    let may_eof f =
      try Some (f ()) with
      | End_of_file -> None

    let trim line =
      let len = String.length line in
      if len > 0 && Char.equal (String.get line (len - 1)) '\r'
      then
        (* TOTO: wrap String? *)
        String.sub line 0 (len - 1)
      else line

    let input_line t =
      match may_eof (fun () -> input_line t) with
      | None -> None
      | Some line -> Some (trim line)

    let close = close_in
  end
  ;;

  module Exn : Core_sig.EXN = struct
    let to_string exn = Printexc.to_string exn
  end
  ;;

  module String : Core_sig.STRING = struct
    let concat ~sep l = String.concat sep l
    let hash s = Hashtbl.hash s
  end
  ;;

  module Float = struct
    type t = Float.t

    let ( > ) x y =
      Float.compare x y > 0

    let ( < ) x y =
      Float.compare x y < 0

    let ( = ) x y =
      Float.compare x y = 0

    let ( <= ) x y =
      Float.compare x y < 0

    let compare = Float.compare
    let equal = Float.equal
    let min_value = Float.neg_infinity
    let max_value = Float.infinity
    let round_down = Float.floor
    let sqrt = Float.sqrt
  end
  ;;

  module Result : Core_sig.RESULT = struct
    type ('ok, 'a) t = ('ok, 'a) result

    let ok (r : ('ok, 'a) t) : 'ok option =
      match r with
      | Ok value -> Some value
      | Error _ -> None
  end
  ;;
end