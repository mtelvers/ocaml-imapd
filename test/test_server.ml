(** Integration tests for imap_server module *)

(* Note: Full connection handling tests require EIO mock flows which are complex to set up.
   These tests focus on response serialization and parser integration. *)

open Imap_types
open Imap_parser

(* Helper: check if string contains substring *)
let contains_substring ~substring s =
  let len = String.length substring in
  let rec loop i =
    if i + len > String.length s then false
    else if String.sub s i len = substring then true
    else loop (i + 1)
  in
  loop 0

(* Helper: check if string starts with prefix *)
let starts_with ~prefix s =
  let len = String.length prefix in
  String.length s >= len && String.sub s 0 len = prefix

(* Test response serialization *)
let test_ok_response () =
  let response = Ok { tag = Some "A001"; code = None; text = "completed" } in
  let serialized = response_to_string response in
  Alcotest.(check bool) "has tag" true
    (contains_substring ~substring:"A001" serialized);
  Alcotest.(check bool) "has OK" true
    (contains_substring ~substring:"OK" serialized);
  Alcotest.(check bool) "has text" true
    (contains_substring ~substring:"completed" serialized)

let test_no_response () =
  let response = No { tag = Some "A002"; code = Some Code_nonexistent; text = "not found" } in
  let serialized = response_to_string response in
  Alcotest.(check bool) "has tag" true
    (contains_substring ~substring:"A002" serialized);
  Alcotest.(check bool) "has NO" true
    (contains_substring ~substring:"NO" serialized);
  Alcotest.(check bool) "has NONEXISTENT code" true
    (contains_substring ~substring:"NONEXISTENT" serialized)

let test_bad_response () =
  let response = Bad { tag = Some "A003"; code = None; text = "syntax error" } in
  let serialized = response_to_string response in
  Alcotest.(check bool) "has tag" true
    (contains_substring ~substring:"A003" serialized);
  Alcotest.(check bool) "has BAD" true
    (contains_substring ~substring:"BAD" serialized)

let test_capability_response () =
  let response = Capability_response ["IMAP4rev2"; "STARTTLS"; "IDLE"] in
  let serialized = response_to_string response in
  Alcotest.(check bool) "has CAPABILITY" true
    (contains_substring ~substring:"CAPABILITY" serialized);
  Alcotest.(check bool) "has IMAP4rev2" true
    (contains_substring ~substring:"IMAP4rev2" serialized);
  Alcotest.(check bool) "has STARTTLS" true
    (contains_substring ~substring:"STARTTLS" serialized)

let test_list_response () =
  let response = List_response { flags = []; delimiter = Some '/'; name = "INBOX" } in
  let serialized = response_to_string response in
  Alcotest.(check bool) "has LIST" true
    (contains_substring ~substring:"LIST" serialized);
  Alcotest.(check bool) "has delimiter" true
    (contains_substring ~substring:"\"/\"" serialized);
  Alcotest.(check bool) "has INBOX" true
    (contains_substring ~substring:"INBOX" serialized)

let test_exists_response () =
  let response = Exists 42 in
  let serialized = response_to_string response in
  Alcotest.(check bool) "has EXISTS" true
    (contains_substring ~substring:"EXISTS" serialized);
  Alcotest.(check bool) "has count" true
    (contains_substring ~substring:"42" serialized)

let test_bye_response () =
  let response = Bye { code = None; text = "Server shutting down" } in
  let serialized = response_to_string response in
  Alcotest.(check bool) "has BYE" true
    (contains_substring ~substring:"BYE" serialized);
  Alcotest.(check bool) "has text" true
    (contains_substring ~substring:"Server shutting down" serialized)

let test_continuation_response () =
  let response = Continuation (Some "Ready for data") in
  let serialized = response_to_string response in
  Alcotest.(check bool) "starts with +" true
    (starts_with ~prefix:"+ " serialized)

let test_namespace_response () =
  let response = Namespace_response {
    personal = Some [{ prefix = ""; delimiter = Some '/' }];
    other = None;
    shared = None;
  } in
  let serialized = response_to_string response in
  Alcotest.(check bool) "has NAMESPACE" true
    (contains_substring ~substring:"NAMESPACE" serialized)

let test_status_response () =
  let response = Status_response {
    mailbox = "INBOX";
    items = [(Status_messages, 10L); (Status_uidnext, 100L)];
  } in
  let serialized = response_to_string response in
  Alcotest.(check bool) "has STATUS" true
    (contains_substring ~substring:"STATUS" serialized);
  Alcotest.(check bool) "has MESSAGES" true
    (contains_substring ~substring:"MESSAGES 10" serialized)

let test_enabled_response () =
  let response = Enabled ["IMAP4rev2"; "UTF8=ACCEPT"] in
  let serialized = response_to_string response in
  Alcotest.(check bool) "has ENABLED" true
    (contains_substring ~substring:"ENABLED" serialized)

(* Test command parsing *)
let test_parse_login () =
  match parse_command "A001 LOGIN user pass\r\n" with
  | Result.Ok cmd ->
    Alcotest.(check string) "tag" "A001" cmd.tag;
    (match cmd.command with
     | Login { username; password } ->
       Alcotest.(check string) "username" "user" username;
       Alcotest.(check string) "password" "pass" password
     | _ -> Alcotest.fail "Expected Login command")
  | Result.Error msg -> Alcotest.fail ("Parse failed: " ^ msg)

let test_parse_select () =
  match parse_command "A002 SELECT INBOX\r\n" with
  | Result.Ok cmd ->
    Alcotest.(check string) "tag" "A002" cmd.tag;
    (match cmd.command with
     | Select mb -> Alcotest.(check string) "mailbox" "INBOX" mb
     | _ -> Alcotest.fail "Expected Select command")
  | Result.Error msg -> Alcotest.fail ("Parse failed: " ^ msg)

let test_parse_fetch () =
  match parse_command "A003 FETCH 1:5 FLAGS\r\n" with
  | Result.Ok cmd ->
    Alcotest.(check string) "tag" "A003" cmd.tag;
    (match cmd.command with
     | Fetch { sequence; items } ->
       Alcotest.(check int) "sequence length" 1 (List.length sequence);
       Alcotest.(check int) "items length" 1 (List.length items)
     | _ -> Alcotest.fail "Expected Fetch command")
  | Result.Error msg -> Alcotest.fail ("Parse failed: " ^ msg)

let test_parse_store () =
  match parse_command "A004 STORE 1 +FLAGS (\\Seen)\r\n" with
  | Result.Ok cmd ->
    Alcotest.(check string) "tag" "A004" cmd.tag;
    (match cmd.command with
     | Store { action; flags; _ } ->
       Alcotest.(check bool) "action is add" true (action = Store_add);
       Alcotest.(check int) "one flag" 1 (List.length flags)
     | _ -> Alcotest.fail "Expected Store command")
  | Result.Error msg -> Alcotest.fail ("Parse failed: " ^ msg)

let test_parse_list () =
  match parse_command "A005 LIST \"\" \"*\"\r\n" with
  | Result.Ok cmd ->
    Alcotest.(check string) "tag" "A005" cmd.tag;
    (match cmd.command with
     | List { reference; pattern } ->
       Alcotest.(check string) "reference" "" reference;
       Alcotest.(check string) "pattern" "*" pattern
     | _ -> Alcotest.fail "Expected List command")
  | Result.Error msg -> Alcotest.fail ("Parse failed: " ^ msg)

let test_parse_create () =
  match parse_command "A006 CREATE Drafts\r\n" with
  | Result.Ok cmd ->
    Alcotest.(check string) "tag" "A006" cmd.tag;
    (match cmd.command with
     | Create mb -> Alcotest.(check string) "mailbox" "Drafts" mb
     | _ -> Alcotest.fail "Expected Create command")
  | Result.Error msg -> Alcotest.fail ("Parse failed: " ^ msg)

let test_parse_namespace () =
  match parse_command "A007 NAMESPACE\r\n" with
  | Result.Ok cmd ->
    Alcotest.(check string) "tag" "A007" cmd.tag;
    (match cmd.command with
     | Namespace -> ()
     | _ -> Alcotest.fail "Expected Namespace command")
  | Result.Error msg -> Alcotest.fail ("Parse failed: " ^ msg)

let test_parse_idle () =
  match parse_command "A008 IDLE\r\n" with
  | Result.Ok cmd ->
    Alcotest.(check string) "tag" "A008" cmd.tag;
    (match cmd.command with
     | Idle -> ()
     | _ -> Alcotest.fail "Expected Idle command")
  | Result.Error msg -> Alcotest.fail ("Parse failed: " ^ msg)

let test_parse_starttls () =
  match parse_command "A009 STARTTLS\r\n" with
  | Result.Ok cmd ->
    Alcotest.(check string) "tag" "A009" cmd.tag;
    (match cmd.command with
     | Starttls -> ()
     | _ -> Alcotest.fail "Expected Starttls command")
  | Result.Error msg -> Alcotest.fail ("Parse failed: " ^ msg)

let test_parse_capability () =
  match parse_command "A010 CAPABILITY\r\n" with
  | Result.Ok cmd ->
    Alcotest.(check string) "tag" "A010" cmd.tag;
    (match cmd.command with
     | Capability -> ()
     | _ -> Alcotest.fail "Expected Capability command")
  | Result.Error msg -> Alcotest.fail ("Parse failed: " ^ msg)

let test_parse_logout () =
  match parse_command "A011 LOGOUT\r\n" with
  | Result.Ok cmd ->
    Alcotest.(check string) "tag" "A011" cmd.tag;
    (match cmd.command with
     | Logout -> ()
     | _ -> Alcotest.fail "Expected Logout command")
  | Result.Error msg -> Alcotest.fail ("Parse failed: " ^ msg)

let test_parse_uid_fetch () =
  match parse_command "A012 UID FETCH 100:200 FLAGS\r\n" with
  | Result.Ok cmd ->
    Alcotest.(check string) "tag" "A012" cmd.tag;
    (match cmd.command with
     | Uid (Uid_fetch _) -> ()
     | _ -> Alcotest.fail "Expected UID FETCH command")
  | Result.Error msg -> Alcotest.fail ("Parse failed: " ^ msg)

let () =
  let open Alcotest in
  run "imap_server" [
    "response_serialization", [
      test_case "ok_response" `Quick test_ok_response;
      test_case "no_response" `Quick test_no_response;
      test_case "bad_response" `Quick test_bad_response;
      test_case "capability_response" `Quick test_capability_response;
      test_case "list_response" `Quick test_list_response;
      test_case "exists_response" `Quick test_exists_response;
      test_case "bye_response" `Quick test_bye_response;
      test_case "continuation_response" `Quick test_continuation_response;
      test_case "namespace_response" `Quick test_namespace_response;
      test_case "status_response" `Quick test_status_response;
      test_case "enabled_response" `Quick test_enabled_response;
    ];
    "command_parsing", [
      test_case "login" `Quick test_parse_login;
      test_case "select" `Quick test_parse_select;
      test_case "fetch" `Quick test_parse_fetch;
      test_case "store" `Quick test_parse_store;
      test_case "list" `Quick test_parse_list;
      test_case "create" `Quick test_parse_create;
      test_case "namespace" `Quick test_parse_namespace;
      test_case "idle" `Quick test_parse_idle;
      test_case "starttls" `Quick test_parse_starttls;
      test_case "capability" `Quick test_parse_capability;
      test_case "logout" `Quick test_parse_logout;
      test_case "uid_fetch" `Quick test_parse_uid_fetch;
    ];
  ]
