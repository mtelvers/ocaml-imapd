let () =
  let input = "a FETCH 1 (BODY.PEEK[HEADER.FIELDS (date subject)])\r\n" in
  Printf.printf "Input: %s\n" input;
  match Imap_parser.parse_command input with
  | Ok cmd ->
    Printf.printf "Parsed successfully!\n";
    (match cmd.Imap_types.command with
     | Imap_types.Fetch { items; _ } ->
       List.iter (fun item ->
         match item with
         | Imap_types.Fetch_body_peek (s, _) ->
           Printf.printf "Fetch_body_peek section: %S\n" s
         | Imap_types.Fetch_body_section (s, _) ->
           Printf.printf "Fetch_body_section section: %S\n" s
         | _ -> Printf.printf "Other fetch item\n"
       ) items
     | _ -> Printf.printf "Not a FETCH command\n")
  | Error msg -> Printf.printf "Parse error: %s\n" msg
