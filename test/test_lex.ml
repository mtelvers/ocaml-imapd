let () =
  let input = "7.185 FETCH 1 (INTERNALDATE UID RFC822.SIZE FLAGS BODY.PEEK[HEADER.FIELDS (date subject from to cc message-id in-reply-to references content-type x-priority x-uniform-type-identifier x-universally-unique-identifier list-id list-unsubscribe)])\r\n" in
  Printf.printf "Input length: %d\n" (String.length input);
  Printf.printf "Input:\n%s\n" input;
  Printf.printf "\nCharacter at position 109: '%c'\n" input.[108];
  Printf.printf "Characters around position 109: '...%s...'\n" (String.sub input 100 25);
  Printf.printf "\n";
  match Imap_parser.parse_command input with
  | Ok _cmd -> Printf.printf "Parsed successfully!\n"
  | Error msg -> Printf.printf "Parse error: %s\n" msg
