(*
 * Copyright (c) Tatiana Racheva
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This file was originally called naming_sqlite.ml, describing the naming table *)

(* TODO: do not open *)
open Sqlite_utils

(* TODO: functorize *)
module String_utils = String_utils.String_utils

module Exception = Exception.MakeException
    (String_utils)

type db_path = Db_path of string [@@deriving eq, show]

type insertion_error = {
  hash: Int64.t;
  name: string;
  origin_exception: Exception.t;
  [@printer (fun fmt e -> fprintf fmt "%s" (Exception.get_ctor_string e))]
}
[@@deriving show]

type save_result = {
  symbols_added: int;
  errors: insertion_error list;
}
[@@deriving show]

let empty_save_result = { symbols_added = 0; errors = [] }

let insert_safe ~name ~hash f :
  (unit, insertion_error) result =
  try Ok (f ()) with
  | e ->
    let origin_exception = Exception.wrap e in
    Error { hash; name; origin_exception }

module StatementCache = struct
  type t = {
    db: Sqlite3.db;
    statements: (string, Sqlite3.stmt) Hashtbl.t;
  }

  let make ~db = { db; statements = Hashtbl.create 6 }

  (** Prepared statements must be finalized before we can close the database
      connection, or else an exception is thrown. Call this function before
      attempting `Sqlite3.close_db`. *)
  let close t =
    (* TODO: add Hashtbl to Core_ops *)
    Hashtbl.iter
      (fun _query stmt ->
         Sqlite3.finalize stmt |> check_rc t.db)
      t.statements;
    Hashtbl.clear t.statements

  let make_stmt t query =
    let stmt =
      match Hashtbl.find_opt t.statements query with
      | Some statement -> statement
      | None ->
        let statement = Sqlite3.prepare t.db query in
        let () = Hashtbl.add
            t.statements
            query
            statement
        in
        statement
        (* TODO: add find_or_add in Core_ops *)
        (* Hashtbl.find_or_add t.statements query ~default:(fun () ->
            Sqlite3.prepare t.db query) *)
    in
    (* Clear any previous bindings for prepared statement parameters. *)
    Sqlite3.reset stmt |> check_rc t.db;
    stmt
end

(* TODO: move lowercase_ascii to Core_ops *)
let to_canon_name_key = String.lowercase_ascii

let fold_sqlite stmt ~f ~init =
  let rec helper acc =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.DONE -> acc
    | Sqlite3.Rc.ROW -> helper (f stmt acc)
    | rc ->
      failwith
        (Printf.sprintf "SQLite operation failed: %s" (Sqlite3.Rc.to_string rc))
  in
  helper init

module SymbolTable = struct
  let table_name = "NAMING_SYMBOLS"

  let create_table_sqlite =
    Printf.sprintf
      "
       CREATE TABLE IF NOT EXISTS %s(
         HASH INTEGER PRIMARY KEY NOT NULL,
         ID INTEGER NOT NULL,
         CATALOG_OFFSET INTEGER NOT NULL
       );
     "
      table_name

  let insert_sqlite =
    Printf.sprintf
      "
       INSERT INTO %s(
         HASH,
         ID,
         CATALOG_OFFSET)
       VALUES (?, ?, ?);
     "
      table_name

  let get_sqlite =
    Printf.sprintf
      "
        SELECT
          ID,
          CATALOG_OFFSET
        FROM %s
        WHERE HASH = ?
      "
      table_name

  (** Note: name parameter is used solely for debugging; it's only the hash and canon_hash that get inserted. *)
  let insert db stmt_cache ~name ~hash ~id ~catalog_offset
    : (unit, insertion_error) result =
    let insert_stmt = StatementCache.make_stmt stmt_cache insert_sqlite in
    Sqlite3.bind insert_stmt 1 (Sqlite3.Data.INT hash) |> check_rc db;
    Sqlite3.bind insert_stmt 2 (Sqlite3.Data.INT id) |> check_rc db;
    Sqlite3.bind insert_stmt 3 (Sqlite3.Data.INT catalog_offset) |> check_rc db;
    insert_safe ~name ~hash @@ fun () ->
    Sqlite3.step insert_stmt |> check_rc db

  let get db stmt_cache name stmt =
    let hash = Murmur3.hash64 (to_canon_name_key name) in
    let get_stmt = StatementCache.make_stmt stmt_cache stmt in
    Sqlite3.bind get_stmt 1 (Sqlite3.Data.INT hash) |> check_rc db;
    match Sqlite3.step get_stmt with
    | Sqlite3.Rc.DONE -> None
    | Sqlite3.Rc.ROW ->
      let id = column_int64 get_stmt 0 in
      let catalog_offset = column_int64 get_stmt 1 in
      Some
        ( name, id, catalog_offset )
    | rc ->
      failwith
        (Printf.sprintf "Failure retrieving row: %s" (Sqlite3.Rc.to_string rc))
end

let db_cache :
  [ `Not_yet_cached
  | `Cached of (db_path * Sqlite3.db * StatementCache.t) option
  ]
    ref =
  ref `Not_yet_cached

let open_db (Db_path path) : Sqlite3.db =
  let db = Sqlite3.db_open path in
  Sqlite3.exec db "PRAGMA synchronous = OFF;" |> check_rc db;
  Sqlite3.exec db "PRAGMA journal_mode = MEMORY;" |> check_rc db;
  db

let get_db_and_stmt_cache (path : db_path) : Sqlite3.db * StatementCache.t =
  match !db_cache with
  | `Cached (Some (existing_path, db, stmt_cache))
    when equal_db_path path existing_path ->
    (db, stmt_cache)
  | _ ->
    let db = open_db path in
    let stmt_cache = StatementCache.make ~db in
    db_cache := `Cached (Some (path, db, stmt_cache));
    (db, stmt_cache)

let validate_can_open_db (db_path : db_path) : unit =
  let (_ : Sqlite3.db * StatementCache.t) = get_db_and_stmt_cache db_path in
  ()

let free_db_cache () : unit = db_cache := `Not_yet_cached

let save_name db stmt_cache ~name ~id ~catalog_offset : (unit, insertion_error) result =
  let hash = Murmur3.hash64 (to_canon_name_key name) in
  SymbolTable.insert
    db
    stmt_cache
    ~name
    ~hash
    ~id
    ~catalog_offset

type entry = {
  offset: int64;
  id: int64;
  title: string;
}

let save_names db_name next_seq : save_result =
  let db = Sqlite3.db_open db_name in
  let stmt_cache = StatementCache.make ~db in
  Sqlite3.exec db "BEGIN TRANSACTION;" |> check_rc db;
  try
    Sqlite3.exec db SymbolTable.create_table_sqlite |> check_rc db;
    let save_entries save_result entries =
      let f save_result entry =
        match save_name db stmt_cache ~name:entry.title ~id:entry.id ~catalog_offset:entry.offset with
        | Error insertion_error -> { save_result with errors = insertion_error :: save_result.errors }
        | Ok () -> { save_result with symbols_added = save_result.symbols_added + 1 }
      in
      List.fold_left f save_result entries
    in
    let save_result = Seq.fold_left save_entries empty_save_result next_seq in

    Sqlite3.exec db "END TRANSACTION;" |> check_rc db;
    Sqlite3.exec db "VACUUM;" |> check_rc db;
    StatementCache.close stmt_cache;
    if not @@ Sqlite3.db_close db then
      failwith @@ Printf.sprintf "Could not close database at %s" db_name;
    save_result
  with
  | e ->
    Sqlite3.exec db "END TRANSACTION;" |> check_rc db;
    raise e

let get (db_path : db_path) name : (string * int64 * int64) option =
  let (db, stmt_cache) = get_db_and_stmt_cache db_path in
  SymbolTable.get
    db
    stmt_cache
    name
    SymbolTable.get_sqlite