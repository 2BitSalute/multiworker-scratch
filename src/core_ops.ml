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
  end

  module List = struct
    let iter l ~f = List.iter f l
    let fold_left l ~f ~init = List.fold_left f init l
  end

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

    let value ~default v =
      match v with
      | Some v -> v
      | None -> default
  end

  module Out_channel = struct
    type t = out_channel

    (* Sorta copied from https://github.com/janestreet/stdio/blob/master/src/out_channel.ml *)
    let create file =
      let perm = 0o666 in
      let flags = [ Open_wronly; Open_creat ] in
      open_out_gen flags perm file

    let close = close_out
  end

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
end