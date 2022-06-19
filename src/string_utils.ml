(* Splits a string into a list of strings using only "\n" as a delimiter.
 * If the string ends with a delimiter, an empty string representing the
 * contents after the final delimiter is NOT included (unlike Str.split_delim).
*)

module String_utils : Sys_sig.STRINGUTILS = struct
  let split_on_newlines content =
    let re = Str.regexp "[\n]" in
    let lines = Str.split_delim re content in
    (* don't create a list entry for the line after a trailing newline *)
    match List.rev lines with
    | "" :: rest -> List.rev rest
    | _ -> lines


  let truncate len s =
    if String.length s <= len then
      s
    else
      String.sub s 0 len
end