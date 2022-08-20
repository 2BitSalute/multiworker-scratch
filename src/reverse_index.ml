(*
 * Copyright (c) Tatiana Racheva
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This file was originally called naming_sqlite.ml, describing the naming table *)

open Sqlite_utils

(* TODO: functorize *)
module String_utils = String_utils.String_utils
module Exception = Exception.MakeException
    (String_utils)

type db_path = Db_path of string [@@deriving eq, show]

type entry = {
  offset: int64;
  id: int64;
  name: string;
}
[@@deriving show]

type retrieval_entry = {
  entry: entry;
  hash: int64;
  canon_hash: int64;
}
[@@deriving show]

type index_entry = {
  hash: int64;
  canon_hash: int64;
  name: string;
}
[@@deriving show]

type retrieval_error = {
  retrieval_context: index_entry;
  message: string;
}
[@@deriving show]

type insertion_error = {
  insertion_context: index_entry;
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

let insert_safe ~name ~hash ~canon_hash f :
  (unit, insertion_error) result =
  try Ok (f ()) with
  | e ->
    let origin_exception = Exception.wrap e in
    Error {
      insertion_context = {
        hash;
        canon_hash;
        name;
      };
      origin_exception
    }

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
    try
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
      in
      (* Clear any previous bindings for prepared statement parameters. *)
      Sqlite3.reset stmt |> check_rc t.db;
      stmt
    with e ->
      Printf.printf "Faulty query: %s\n\n%!" query;
      raise e
end

(* TODO: move lowercase_ascii to Core_ops *)
let to_canon_name_key name =
  name
  (* TODO: put it back in and regen the reverse index *)
  (* |> Pcre.replace ~templ:"_" *)
  |> String.lowercase_ascii

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
          HASH INTEGER NOT NULL,
          CANON_HASH INTEGER NOT NULL,
          ID INTEGER NOT NULL,
          CATALOG_OFFSET INTEGER NOT NULL,
          PRIMARY KEY (HASH, ID, CATALOG_OFFSET)
        );
      "
      table_name

  (* TODO: canon_hash index *)

  let insert_sqlite =
    Printf.sprintf
      "
        INSERT INTO %s(
          HASH,
          CANON_HASH,
          ID,
          CATALOG_OFFSET)
        VALUES (?, ?, ?, ?);
      "
      table_name

  let create_index_sqlite =
    Printf.sprintf
      "
        CREATE INDEX IF NOT EXISTS SYMBOL_CANON ON %s (CANON_HASH);
      "
      table_name

  let get_sqlite =
    Printf.sprintf
      "
        SELECT
          HASH,
          ID,
          CATALOG_OFFSET
        FROM %s
        WHERE CANON_HASH = ?
      "
      table_name

  (** Note: name parameter is used solely for debugging; it's only the hash and canon_hash that get inserted. *)
  let insert (stmt_cache: StatementCache.t) ~name ~hash ~canon_hash ~id ~offset
    : (unit, insertion_error) result =
    let db = stmt_cache.db in
    let insert_stmt = StatementCache.make_stmt stmt_cache insert_sqlite in
    Sqlite3.bind insert_stmt 1 (Sqlite3.Data.INT hash) |> check_rc db;
    Sqlite3.bind insert_stmt 2 (Sqlite3.Data.INT canon_hash) |> check_rc db;
    Sqlite3.bind insert_stmt 3 (Sqlite3.Data.INT id) |> check_rc db;
    Sqlite3.bind insert_stmt 4 (Sqlite3.Data.INT offset) |> check_rc db;
    insert_safe ~name ~hash ~canon_hash @@ fun () ->
    Sqlite3.step insert_stmt |> check_rc db

  let get (stmt_cache: StatementCache.t) name stmt =
    let db = stmt_cache.db in
    let canon_hash = Murmur3.hash64 (to_canon_name_key name) in
    let get_stmt = StatementCache.make_stmt stmt_cache stmt in
    Sqlite3.bind get_stmt 1 (Sqlite3.Data.INT canon_hash) |> check_rc db;
    let rec loop entries =
      match Sqlite3.step get_stmt with
      | Sqlite3.Rc.DONE -> entries
      | Sqlite3.Rc.ROW ->
        let entry =
          {
            name;
            id = column_int64 get_stmt 1;
            offset = column_int64 get_stmt 2
          }
        in
        let hash = column_int64 get_stmt 0 in
        loop ({ entry; hash; canon_hash } :: entries)
      | rc ->
        failwith
          (Printf.sprintf "Failure retrieving row: %s" (Sqlite3.Rc.to_string rc))
    in
    match loop [] with
    | [] -> Error { 
        retrieval_context = {
          hash = Murmur3.hash64 name;
          canon_hash;
          name;
        };
        message = (Printf.sprintf "Entries corresponding to `%s` not found in the index" name);
      }
    | entries -> Ok entries
end

let save_name stmt_cache ~name ~id ~offset : (unit, insertion_error) result =
  let hash = Murmur3.hash64 name in
  let canon_hash = Murmur3.hash64 (to_canon_name_key name) in
  (* Printf.printf "SAVE: %s %s\n%!" name (Int64.to_string hash); *)
  SymbolTable.insert
    stmt_cache
    ~name
    ~canon_hash
    ~hash
    ~id
    ~offset

let resolve_db_name db_name name =
  let initial = String.sub name 0 1 |> String.lowercase_ascii in
  let suffix = match initial with
    (* Omitting q and x - these are rare enough that they can go into "_" *)
    | "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l"
    | "m" | "n" | "o" | "p" | "r" | "s" | "t" | "u" | "v" | "w" | "y" | "z"
      -> initial
    | _
      -> "_"
  in
  db_name ^ "." ^ suffix ^ ".sql"

module type DbCacheOps = sig
  val prep : string -> StatementCache.t
  val finalize : StatementCache.t -> unit
end

module DbCache (DbCacheOps: DbCacheOps) = struct
  type t = {
    db_name: string;
    db_map: (string, StatementCache.t) Hashtbl.t;
  }

  let prep = DbCacheOps.prep
  let finalize = DbCacheOps.finalize

  let make db_name = { db_name; db_map = Hashtbl.create 30 }

  let close t =
    Hashtbl.iter
      (fun _name (stmt_cache: StatementCache.t) -> finalize stmt_cache)
      t.db_map;
    Hashtbl.clear t.db_map

  let db t name =
    let db_name = resolve_db_name t.db_name name in
    try
      let stmt_cache =
        match Hashtbl.find_opt t.db_map db_name with
        | Some stmt_cache -> stmt_cache
        | None ->
          let stmt_cache = prep db_name in
          let () = Hashtbl.add
              t.db_map
              db_name
              stmt_cache
          in
          stmt_cache
      in
      stmt_cache
    with e ->
      Printf.printf "Faulty name: %s\n\n%!" name;
      raise e
end

module QueryDbCacheOps : DbCacheOps = struct
  let prep db_name =
    let db = Sqlite3.db_open db_name in
    Sqlite3.exec db "PRAGMA synchronous = OFF;" |> check_rc db;
    Sqlite3.exec db "PRAGMA journal_mode = MEMORY;" |> check_rc db;

    let stmt_cache = StatementCache.make ~db in
    stmt_cache

  let finalize (stmt_cache: StatementCache.t) =
    let db = stmt_cache.db in
    StatementCache.close stmt_cache;
    if not @@ Sqlite3.db_close db then
      failwith @@ Printf.sprintf "Could not close database"
end

module IngestionDbCacheOps : DbCacheOps = struct
  let prep db_name =
    let db = Sqlite3.db_open db_name in
    try
      let stmt_cache = StatementCache.make ~db in
      Sqlite3.exec db "BEGIN TRANSACTION;" |> check_rc db;
      Sqlite3.exec db SymbolTable.create_table_sqlite |> check_rc db;
      stmt_cache
    with e ->
      Sqlite3.exec db "END TRANSACTION;" |> check_rc db;
      raise e

  let finalize (stmt_cache: StatementCache.t) =
    let db = stmt_cache.db in
    try
      Sqlite3.exec db SymbolTable.create_index_sqlite |> check_rc db;
      Sqlite3.exec db "END TRANSACTION;" |> check_rc db;
      Sqlite3.exec db "VACUUM;" |> check_rc db;
      StatementCache.close stmt_cache;
      if not @@ Sqlite3.db_close db then
        failwith @@ Printf.sprintf "Could not close database";
    with e ->
      Sqlite3.exec db "END TRANSACTION;" |> check_rc db;
      raise e
end

module QueryDbCache = DbCache (QueryDbCacheOps)
module IngestionDbCache = DbCache (IngestionDbCacheOps)

let save_names db_name next_seq : save_result =
  let ingestion_cache = IngestionDbCache.make db_name in
  let save_entries save_result entries =
    let f save_result { name; id; offset } =
      let stmt_cache = IngestionDbCache.db ingestion_cache name in
      match save_name stmt_cache ~name ~id ~offset with
      | Error insertion_error -> { save_result with errors = insertion_error :: save_result.errors }
      | Ok () -> { save_result with symbols_added = save_result.symbols_added + 1 }
    in
    List.fold_left f save_result entries
  in
  let save_result = Seq.fold_left save_entries empty_save_result next_seq in

  IngestionDbCache.close ingestion_cache;
  save_result

let get (db_cache : QueryDbCache.t) name : (retrieval_entry list, retrieval_error) result =
  try
    SymbolTable.get
      (QueryDbCache.db db_cache name)
      name
      SymbolTable.get_sqlite
  with e ->
    let backtrace = Printexc.get_backtrace () in
    let e = Printexc.to_string e in
    let message =
      Printf.sprintf
        "Problem getting name `%s` at\n%s\n%s"
        name
        e
        backtrace
    in
    Printf.printf "MESSAGE: %s\n%!" message;
    Error {
      retrieval_context = {
        hash = 0L;
        canon_hash = 0L;
        name;
      };
      message;
    }

module type REVERSE_INDEX = sig
  val get_entries : string -> (retrieval_entry list, retrieval_error) result
end

module SqliteIndex = struct
  let db_cache = ref None

  let init cache =
    db_cache := Some cache

  let get_entries name : (retrieval_entry list, retrieval_error) result =
    match !db_cache with
    | Some db_cache ->
      get db_cache name
    | None -> failwith "DB Cache not set!"
end

module TestIndex = struct
  let (names : (int64, retrieval_entry list) Hashtbl.t) = Hashtbl.create 10

  let to_canon_hash name =
    to_canon_name_key name |> Murmur3.hash64

  let add name =
    let canon_hash = to_canon_hash name in
    let entry = {
      entry = {
        name;
        id = Int64.of_int 0;
        offset = Int64.of_int 0;
      };
      hash = Murmur3.hash64 name;
      canon_hash;
    }
    in
    match Hashtbl.find_opt names canon_hash with
    | None -> Hashtbl.add names canon_hash [ entry ]
    | Some entries -> Hashtbl.add names canon_hash (entry :: entries);
      ()

  let get_entries name : (retrieval_entry list, retrieval_error) result =
    Ok (Hashtbl.find names (to_canon_hash name))
end