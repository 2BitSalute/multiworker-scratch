(* TODO: real *)
module Exit_status = struct
  type t =
    | No_error
    | Out_of_shared_memory
    | Hash_table_full
    | Heap_full
    | Sql_corrupt (* TODO: remove SQLite stuff*)
    | Sql_cantopen
    | Sql_misuse
    | Sql_assertion_failure
    | Type_error
    | Worker_not_found_exception

  let exit_code _code =
    (* TODO: convert status to int *)
    -1
end

module Exit = struct
  let exit status =
    exit (Exit_status.exit_code status)
end

module PrintSignal = struct
  let string_of_signal _signal : string = failwith "TODO: string_of_signal"
end