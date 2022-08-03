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

let read_catalog_at_offset iname bytes_to_skip =
  let ic = open_in iname in

  In_channel.seek ic bytes_to_skip;

  let buflen =
    8192
  in

  let total_buffer = Buffer.create buflen in

  let buf = Bytes.create buflen in
  let bzic = Bz2.open_in ic in

  (* TODO: Factor this out! *)
  Buffer.add_string total_buffer "<pages>\n";

  let rec read n =
    if n <> 0 then begin
      try
        let bytes_read = Bz2.read bzic buf 0 buflen in
        Buffer.add_subbytes total_buffer buf 0 bytes_read;
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
  Buffer.add_string total_buffer "</pages>\n";

  Bz2.close_in bzic;
  close_in ic;
  close_out oc;

  total_buffer

type entry = Reverse_index.entry

let index2 iname ~n_pages : entry list Seq.t =
  let ic = open_in iname in

  let buflen =
    8192
  in

  let buf = Bytes.create buflen in

  let bzic = Bz2.open_in ic in

  let get_entry s =
    try
      let tokens = Pcre.split ~pat:":" ~max:0 s in
      Some Reverse_index.{
          offset = Int64.of_string (List.nth tokens 0);
          id = Int64.of_string (List.nth tokens 1);
          name = List.nth tokens 2;
        }
    with _ ->
      (* There are some weird patterns like
         inmahintarayutthayamahadilokphopnopparatrajathaniburiromudomrajaniwesmahasatharnamornphimarnavatarnsathitsakkattiyavisanukamprasit
      *)
      Printf.printf "SKIPPING: %s\n" s;
      None
  in

  let num_entries = ref 0 in
  let last_entry = ref None in

  let read (n, prefix) : (entry list * (int * string)) option =
    if n <> 0 then begin
      try
        let bytes_read = Bz2.read bzic buf 0 buflen in
        let f (start, length, prefix, entries) c =
          if c = '\n' then begin
            match get_entry (prefix ^ Bytes.sub_string buf start length) with
            | Some entry ->
              last_entry := Some entry;
              num_entries := !num_entries + 1;
              (* Printf.printf "%s %s %s\n" (Int64.to_string entry.offset) (Int64.to_string entry.id) entry.name; *)
              (start + length + 1, 0, "", entry :: entries)
            | None ->
              (* SKIPPING *)
              (start + length + 1, 0, "", entries)
          end
          else
            (start, length + 1, prefix, entries)
        in
        let (start, length, _prefix, entries) = Bytes.fold_left f (0, 0, prefix, []) buf in
        let prefix =
          if length > 0 then
            Bytes.sub_string buf start length
          else
            ""
        in

        if bytes_read < buflen then
          raise End_of_file;

        Some (entries, ((n - 1), prefix))
      with End_of_file ->
        Bz2.close_in bzic;
        close_in ic;
        None
    end
    else
      match !last_entry with
      | Some { name; _ } ->
        Printf.printf "\n\nLAST ENTRY: %d %s\n\n" !num_entries name;
        None
      | None ->
        failwith "Woops"
  in

  Seq.unfold read (n_pages, "")

