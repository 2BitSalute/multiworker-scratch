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

let run iname bytes_to_skip =
  let ic = open_in iname in

  In_channel.seek ic bytes_to_skip;

  let oc =
    if not (Filename.check_suffix iname ".bz2") then
      eprintf "Error: unrecognized compressed file extension" ;
    open_out (Filename.chop_suffix iname ".bz2")
  in

  let buflen =
    8192
    (* 256 *)
  in

  let buf = Bytes.create buflen in

  let bzic = Bz2.open_in ic in

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

  Bz2.close_in bzic;
  close_in ic;
  close_out oc
