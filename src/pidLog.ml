(*
 * Copyright (c) 2022, Tatiana Racheva
 * Copyright (c) 2015, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module MakePidLog (Core: Core_sig.COREOPS) (Sys_utils: Sys_sig.SYSUTILS) : Sys_sig.PIDLOG = struct
  module In_channel = Core.In_channel
  module Out_channel = Core.Out_channel
  module Option = Core.Option

  let enabled = ref false

  let log_oc = ref None

  let enable () = enabled := true

  let init pids_file =
    Sys_utils.with_umask 0o111 (fun () ->
        Sys_utils.mkdir_no_fail (Filename.dirname pids_file);
        let oc = Out_channel.open_for_append pids_file in
        log_oc := Some oc;
        Unix.(set_close_on_exec (descr_of_out_channel oc)))

  (* exception FailedToGetPids *)

  (* let get_pids pids_file =
     try
      let ic = In_channel.create pids_file in
      let results = ref [] in
      begin
        try
          while true do
            let row = In_channel.input_line ic in
            match row with
            | None -> raise End_of_file
            | Some row ->
              if Str.string_match (Str.regexp "^\\([0-9]+\\)\t\\(.+\\)") row 0
              then
                let pid = int_of_string (Str.matched_group 1 row) in
                let reason = Str.matched_group 2 row in
                results := (pid, reason) :: !results
          done
        with
        | End_of_file -> ()
      end;
      In_channel.close ic;
      List.rev !results
     with
     | Sys_error _ -> raise FailedToGetPids *)

  let close () =
    Option.iter !log_oc ~f:Out_channel.close;
    log_oc := None

  let rec log ~pid reason =
    let pid = match pid with
      | -1 -> Unix.getpid ()
      | _ -> pid
    in
    let pid = Sys_utils.pid_of_handle pid in
    match !log_oc with
    | None ->
      init "pids.log";
      log ~pid reason;
    | Some oc ->
      try
        Printf.fprintf oc "%d\t%s\n%!" pid reason;
        close ()
      with Sys_error _ ->
        Printf.printf "SYS_ERROR---------"

  let log ?(pid = (-1)) reason =
    if !enabled then log ~pid reason
end