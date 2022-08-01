(* Sample replacement for "bunzip2": showcase for CamlBZ2
 *
 * Copyright © 2009      Stefano Zacchiroli <zack@upsilon.cc>
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License (with the
 * special exception on linking described in file COPYING) as published
 * by the Free Software Foundation; either version 2.1 of the License,
 * or (at your option) any later version.
*)

(* Compile with: ocamlfind ocamlc -package bz2 -linkpkg -o bunzip2 bunzip2.ml *)

open Printf

let catalog iname bytes_to_skip =
  let ic = open_in iname in

  In_channel.seek ic bytes_to_skip;

  let (oc, filename) =
    if not (Filename.check_suffix iname ".bz2") then
      eprintf "Error: unrecognized compressed file extension" ;
    let filename = Filename.chop_suffix iname ".bz2" in
    open_out (filename), filename
  in

  let buflen =
    8192
    (* 256 *)
  in

  let buf = Bytes.create buflen in

  let bzic = Bz2.open_in ic in

  (* TODO: Factor this out! *)
  Out_channel.output_string oc "<pages>\n";

  let rec read n =
    if n <> 0 then begin
      try
        let bytes_read = Bz2.read bzic buf 0 buflen in
        output oc buf 0 bytes_read;
        if bytes_read < buflen then
          raise End_of_file
        else
          read (n - 1)
      with End_of_file ->
        ()
    end
  in

  read (-1);

  (* TODO: Factor this out! *)
  Out_channel.output_string oc "</pages>\n";

  Bz2.close_in bzic;
  close_in ic;
  close_out oc;

  filename

type entry = {
  offset: int;
  id: int;
  title: string;
}

let index iname =
  let ic = open_in iname in

  let buflen =
    128
    (* 256 *)
  in

  let buf = Bytes.create buflen in

  let bzic = Bz2.open_in ic in

  let get_entry s =
    let tokens = Pcre.split ~pat:":" ~max:0 s in
    List.iter (fun s -> Printf.printf "- %s\n" s) tokens;
    {
      offset = int_of_string (List.nth tokens 0);
      id = int_of_string (List.nth tokens 1);
      title = List.nth tokens 2;
    }
  in

  let rec read n prefix =
    if n <> 0 then begin
      try
        let bytes_read = Bz2.read bzic buf 0 buflen in
        let start = ref 0 in
        let length = ref 0 in
        let prefix = ref prefix in
        let f c =
          if c = '\n' then begin
            let entry = get_entry (!prefix ^ Bytes.sub_string buf !start !length) in
            Printf.printf "%d %d %s\n" entry.offset entry.id entry.title;
            start := !start + !length + 1;
            length := 0;
            prefix := "";
          end
          else
            length := !length + 1;
        in
        Bytes.iter f buf;
        let prefix =
          if !length > 0 then
            Bytes.sub_string buf !start !length
          else
            ""
        in

        (* Out_channel.output Out_channel.stdout buf 0 bytes_read; *)
        if bytes_read < buflen then
          raise End_of_file
        else
          read (n - 1) prefix
      with End_of_file ->
        ()
    end
  in

  (* read (-1); *)
  read 3 "";

  Bz2.close_in bzic;
  close_in ic
