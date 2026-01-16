(** Tests for imap_parser module *)

open Imap_parser

let test_parse_capability () =
  match parse_command "A001 CAPABILITY\r\n" with
  | Ok { tag; command = Capability } ->
    Alcotest.(check string) "tag" "A001" tag
  | Ok _ -> Alcotest.fail "Wrong command parsed"
  | Error msg -> Alcotest.fail msg

let test_parse_noop () =
  match parse_command "A002 NOOP\r\n" with
  | Ok { tag; command = Noop } ->
    Alcotest.(check string) "tag" "A002" tag
  | Ok _ -> Alcotest.fail "Wrong command parsed"
  | Error msg -> Alcotest.fail msg

let test_parse_logout () =
  match parse_command "A003 LOGOUT\r\n" with
  | Ok { tag; command = Logout } ->
    Alcotest.(check string) "tag" "A003" tag
  | Ok _ -> Alcotest.fail "Wrong command parsed"
  | Error msg -> Alcotest.fail msg

let test_parse_login () =
  match parse_command "A004 LOGIN \"user\" \"pass\"\r\n" with
  | Ok { tag; command = Login { username; password } } ->
    Alcotest.(check string) "tag" "A004" tag;
    Alcotest.(check string) "username" "user" username;
    Alcotest.(check string) "password" "pass" password
  | Ok _ -> Alcotest.fail "Wrong command parsed"
  | Error msg -> Alcotest.fail msg

let test_parse_login_atom () =
  match parse_command "A005 LOGIN user pass\r\n" with
  | Ok { tag; command = Login { username; password } } ->
    Alcotest.(check string) "tag" "A005" tag;
    Alcotest.(check string) "username" "user" username;
    Alcotest.(check string) "password" "pass" password
  | Ok _ -> Alcotest.fail "Wrong command parsed"
  | Error msg -> Alcotest.fail msg

let test_parse_select () =
  match parse_command "A006 SELECT INBOX\r\n" with
  | Ok { tag; command = Select mailbox } ->
    Alcotest.(check string) "tag" "A006" tag;
    Alcotest.(check string) "mailbox" "INBOX" mailbox
  | Ok _ -> Alcotest.fail "Wrong command parsed"
  | Error msg -> Alcotest.fail msg

let test_parse_examine () =
  match parse_command "A007 EXAMINE \"Sent Items\"\r\n" with
  | Ok { tag; command = Examine mailbox } ->
    Alcotest.(check string) "tag" "A007" tag;
    Alcotest.(check string) "mailbox" "Sent Items" mailbox
  | Ok _ -> Alcotest.fail "Wrong command parsed"
  | Error msg -> Alcotest.fail msg

let test_parse_list () =
  match parse_command "A008 LIST \"\" \"*\"\r\n" with
  | Ok { tag; command = List { reference; pattern } } ->
    Alcotest.(check string) "tag" "A008" tag;
    Alcotest.(check string) "reference" "" reference;
    Alcotest.(check string) "pattern" "*" pattern
  | Ok _ -> Alcotest.fail "Wrong command parsed"
  | Error msg -> Alcotest.fail msg

let test_parse_fetch () =
  match parse_command "A009 FETCH 1:* FLAGS\r\n" with
  | Ok { tag; command = Fetch { sequence; items } } ->
    Alcotest.(check string) "tag" "A009" tag;
    Alcotest.(check int) "sequence length" 1 (List.length sequence);
    Alcotest.(check int) "items length" 1 (List.length items)
  | Ok _ -> Alcotest.fail "Wrong command parsed"
  | Error msg -> Alcotest.fail msg

let test_parse_store () =
  match parse_command "A010 STORE 1 +FLAGS (\\Seen)\r\n" with
  | Ok { tag; command = Store { sequence = _; silent; action; flags } } ->
    Alcotest.(check string) "tag" "A010" tag;
    Alcotest.(check bool) "silent" false silent;
    Alcotest.(check bool) "action is add" true (action = Imap_types.Store_add);
    Alcotest.(check int) "flags length" 1 (List.length flags)
  | Ok _ -> Alcotest.fail "Wrong command parsed"
  | Error msg -> Alcotest.fail msg

let test_response_ok () =
  let resp = response_to_string (Ok { tag = Some "A001"; code = None; text = "completed" }) in
  Alcotest.(check string) "OK response" "A001 OK completed\r\n" resp

let test_response_capability () =
  let resp = response_to_string (Capability_response ["IMAP4rev2"; "IDLE"]) in
  Alcotest.(check string) "CAPABILITY response" "* CAPABILITY IMAP4rev2 IDLE\r\n" resp

let test_response_exists () =
  let resp = response_to_string (Exists 42) in
  Alcotest.(check string) "EXISTS response" "* 42 EXISTS\r\n" resp

let test_parse_id_nil () =
  match parse_command "A011 ID NIL\r\n" with
  | Ok { tag; command = Id None } ->
    Alcotest.(check string) "tag" "A011" tag
  | Ok _ -> Alcotest.fail "Wrong command parsed"
  | Error msg -> Alcotest.fail msg

let test_parse_id_params () =
  match parse_command "A012 ID (\"name\" \"test\" \"version\" \"1.0\")\r\n" with
  | Ok { tag; command = Id (Some params) } ->
    Alcotest.(check string) "tag" "A012" tag;
    Alcotest.(check int) "params length" 2 (List.length params);
    Alcotest.(check string) "first key" "name" (fst (List.hd params))
  | Ok _ -> Alcotest.fail "Wrong command parsed"
  | Error msg -> Alcotest.fail msg

let test_response_id () =
  let resp = response_to_string (Id_response (Some [("name", "imapd"); ("version", "0.1.0")])) in
  Alcotest.(check bool) "has ID" true (String.sub resp 0 5 = "* ID ");
  Alcotest.(check bool) "has name" true (String.length resp > 10)

let test_response_id_nil () =
  let resp = response_to_string (Id_response None) in
  Alcotest.(check string) "ID NIL response" "* ID NIL\r\n" resp

let () =
  let open Alcotest in
  run "imap_parser" [
    "commands", [
      test_case "CAPABILITY" `Quick test_parse_capability;
      test_case "NOOP" `Quick test_parse_noop;
      test_case "LOGOUT" `Quick test_parse_logout;
      test_case "LOGIN quoted" `Quick test_parse_login;
      test_case "LOGIN atom" `Quick test_parse_login_atom;
      test_case "SELECT" `Quick test_parse_select;
      test_case "EXAMINE" `Quick test_parse_examine;
      test_case "LIST" `Quick test_parse_list;
      test_case "FETCH" `Quick test_parse_fetch;
      test_case "STORE" `Quick test_parse_store;
      test_case "ID NIL" `Quick test_parse_id_nil;
      test_case "ID params" `Quick test_parse_id_params;
    ];
    "responses", [
      test_case "OK response" `Quick test_response_ok;
      test_case "CAPABILITY response" `Quick test_response_capability;
      test_case "EXISTS response" `Quick test_response_exists;
      test_case "ID response" `Quick test_response_id;
      test_case "ID NIL response" `Quick test_response_id_nil;
    ];
  ]
