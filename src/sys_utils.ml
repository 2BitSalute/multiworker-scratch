module MakeUtils
    (Exception: Sys_sig.EXCEPTION) : Sys_sig.UTILS = struct
  let with_context
      ~(enter : unit -> unit) ~(exit : unit -> unit) ~(do_ : unit -> 'a) : 'a =
    enter ();
    let result =
      try do_ () with
      | exn ->
        let e = Exception.wrap exn in
        exit ();
        Exception.reraise e
    in
    exit ();
    result
end

external realpath : string -> string option = "hh_realpath"

module MakeSysUtils
    (Core: Core_sig.COREOPS)
    (Exception: Sys_sig.EXCEPTION)
    (Utils: Sys_sig.UTILS)
  : Sys_sig.SYSUTILS = struct
  external pid_of_handle : int -> int = "pid_of_handle"

  module List = Core.List

  let path_sep =
    if Sys.win32 then
      ";"
    else
      ":"

  let null_path =
    if Sys.win32 then
      "nul"
    else
      "/dev/null"

  let get_env name =
    try Some (Sys.getenv name) with
    | Not_found -> None

  let getenv_path () =
    let path_var = "PATH" in
    (* Same variable on windows *)
    get_env path_var

  let rec waitpid_non_intr flags pid =
    try Unix.waitpid flags pid with
    | Unix.Unix_error (Unix.EINTR, _, _) -> waitpid_non_intr flags pid

  let rec select_non_intr read write exn timeout =
    let start_time = Unix.gettimeofday () in
    try Unix.select read write exn timeout with
    | Unix.Unix_error (Unix.EINTR, _, _) ->
      (* Negative timeouts mean no timeout *)
      let timeout =
        if timeout < 0.0 then
          (* A negative timeout means no timeout, i.e. unbounded wait. *)
          timeout
        else
          Float.(max 0.0 (timeout -. (Unix.gettimeofday () -. start_time)))
      in
      select_non_intr read write exn timeout

  let with_umask umask f =
    let old_umask = ref 0 in
    Utils.with_context
      ~enter:(fun () -> old_umask := Unix.umask umask)
      ~exit:(fun () ->
          let _ = Unix.umask !old_umask in
          ())
      ~do_:f

  let with_umask umask f =
    if Sys.win32 then
      f ()
    else
      with_umask umask f

  let temp_dir_name =
    if Sys.win32 then
      Stdlib.Filename.get_temp_dir_name ()
    else
      "/tmp"

  let mkdir_no_fail dir =
    with_umask 0 (fun () ->
        (* Don't set sticky bit since the socket opening code wants to remove any
         * old sockets it finds, which may be owned by a different user. *)
        try Unix.mkdir dir 0o777 with
        | Unix.Unix_error (Unix.EEXIST, _, _) -> ())

  let getenv_home () =
    let home_var =
      if Sys.win32 then
        "APPDATA"
      else
        "HOME"
    in
    get_env home_var

  let expanduser path =
    Str.substitute_first
      (Str.regexp "^~\\([^/]*\\)")
      begin
        fun s ->
          match Str.matched_group 1 s with
          | "" ->
            begin
              match getenv_home () with
              | None -> (Unix.getpwuid (Unix.getuid ())).Unix.pw_dir
              | Some home -> home
            end
          | unixname ->
            (try (Unix.getpwnam unixname).Unix.pw_dir with
             | Not_found -> Str.matched_string s)
      end
      path

  let executable_path : unit -> string =
    let executable_path_ = ref None in
    let dir_sep = Filename.dir_sep.[0] in
    let search_path path =
      let paths =
        match getenv_path () with
        | None -> failwith "Unable to determine executable path"
        | Some paths -> Str.split (Str.regexp_string path_sep) paths
      in
      let path =
        List.fold_left
          paths
          ~f:
            begin
              fun acc p ->
                match acc with
                | Some _ -> acc
                | None -> realpath (expanduser (Filename.concat p path))
            end
          ~init:None
      in
      match path with
      | Some path -> path
      | None -> failwith "Unable to determine executable path"
    in
    fun () ->
      match !executable_path_ with
      | Some path -> path
      | None ->
        let path = Sys.executable_name in
        let path =
          if String.contains path dir_sep then
            match realpath path with
            | Some path -> path
            | None -> failwith "Unable to determine executable path"
          else
            search_path path
        in
        executable_path_ := Some path;
        path

  let terminate_process pid = Unix.kill pid Sys.sigkill
end