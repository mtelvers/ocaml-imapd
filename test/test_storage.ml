(** Tests for imap_storage module *)

open Imap_storage
open Imap_types

let test_memory_create_mailbox () =
  let storage = Memory_storage.create () in
  Memory_storage.add_test_user storage ~username:"test";

  (* Create a new mailbox *)
  let result = Memory_storage.create_mailbox storage ~username:"test" "Drafts" in
  Alcotest.(check bool) "create success" true (Result.is_ok result);

  (* Creating same mailbox again should fail *)
  let result2 = Memory_storage.create_mailbox storage ~username:"test" "Drafts" in
  Alcotest.(check bool) "duplicate fails" true (Result.is_error result2)

let test_memory_delete_mailbox () =
  let storage = Memory_storage.create () in
  Memory_storage.add_test_user storage ~username:"test";
  ignore (Memory_storage.create_mailbox storage ~username:"test" "ToDelete");

  (* Delete the mailbox *)
  let result = Memory_storage.delete_mailbox storage ~username:"test" "ToDelete" in
  Alcotest.(check bool) "delete success" true (Result.is_ok result);

  (* Deleting INBOX should fail *)
  let result2 = Memory_storage.delete_mailbox storage ~username:"test" "INBOX" in
  Alcotest.(check bool) "delete inbox fails" true (Result.is_error result2)

let test_memory_select_mailbox () =
  let storage = Memory_storage.create () in
  Memory_storage.add_test_user storage ~username:"test";

  (* Select INBOX *)
  let result = Memory_storage.select_mailbox storage ~username:"test" "INBOX" ~readonly:false in
  match result with
  | Ok state ->
    Alcotest.(check string) "mailbox name" "INBOX" state.name;
    Alcotest.(check bool) "not readonly" false state.readonly
  | Error _ -> Alcotest.fail "select failed"

let test_memory_list_mailboxes () =
  let storage = Memory_storage.create () in
  Memory_storage.add_test_user storage ~username:"test";
  ignore (Memory_storage.create_mailbox storage ~username:"test" "Sent");
  ignore (Memory_storage.create_mailbox storage ~username:"test" "Drafts");

  let mailboxes = Memory_storage.list_mailboxes storage ~username:"test" ~reference:"" ~pattern:"*" in
  Alcotest.(check bool) "has mailboxes" true (List.length mailboxes >= 3)  (* INBOX + 2 created *)

let test_memory_append_message () =
  let storage = Memory_storage.create () in
  Memory_storage.add_test_user storage ~username:"test";

  let result = Memory_storage.append storage
    ~username:"test"
    ~mailbox:"INBOX"
    ~flags:[System Seen]
    ~date:None
    ~message:"From: test@example.com\r\nSubject: Test\r\n\r\nHello World"
  in
  match result with
  | Ok uid ->
    Alcotest.(check bool) "uid is positive" true (uid > 0l)
  | Error _ -> Alcotest.fail "append failed"

let test_memory_fetch_messages () =
  let storage = Memory_storage.create () in
  Memory_storage.add_test_user storage ~username:"test";
  ignore (Memory_storage.append storage
    ~username:"test"
    ~mailbox:"INBOX"
    ~flags:[]
    ~date:None
    ~message:"Test message");

  let result = Memory_storage.fetch_messages storage
    ~username:"test"
    ~mailbox:"INBOX"
    ~sequence:[Single 1]
    ~items:[Fetch_flags]
  in
  match result with
  | Ok msgs ->
    Alcotest.(check int) "one message" 1 (List.length msgs)
  | Error _ -> Alcotest.fail "fetch failed"

let test_memory_store_flags () =
  let storage = Memory_storage.create () in
  Memory_storage.add_test_user storage ~username:"test";
  ignore (Memory_storage.append storage
    ~username:"test"
    ~mailbox:"INBOX"
    ~flags:[]
    ~date:None
    ~message:"Test");

  let result = Memory_storage.store_flags storage
    ~username:"test"
    ~mailbox:"INBOX"
    ~sequence:[Single 1]
    ~action:Store_add
    ~flags:[System Seen]
  in
  match result with
  | Ok msgs ->
    let msg = List.hd msgs in
    Alcotest.(check bool) "has Seen flag" true (List.mem (System Seen) msg.flags)
  | Error _ -> Alcotest.fail "store failed"

let test_memory_expunge () =
  let storage = Memory_storage.create () in
  Memory_storage.add_test_user storage ~username:"test";
  ignore (Memory_storage.append storage
    ~username:"test"
    ~mailbox:"INBOX"
    ~flags:[System Deleted]
    ~date:None
    ~message:"To be deleted");

  let result = Memory_storage.expunge storage ~username:"test" ~mailbox:"INBOX" in
  match result with
  | Ok uids ->
    Alcotest.(check int) "one expunged" 1 (List.length uids)
  | Error _ -> Alcotest.fail "expunge failed"

(* ===== Maildir Storage Tests ===== *)

(* Helper to create a temporary directory *)
let with_temp_dir f =
  let base = Filename.get_temp_dir_name () in
  let rec try_create n =
    let path = Printf.sprintf "%s/imapd_test_%d_%d" base (Unix.getpid ()) n in
    if Sys.file_exists path then try_create (n + 1)
    else begin
      Unix.mkdir path 0o700;
      Fun.protect ~finally:(fun () ->
        (* Clean up recursively *)
        let rec rm path =
          if Sys.is_directory path then begin
            Array.iter (fun name -> rm (Filename.concat path name)) (Sys.readdir path);
            Unix.rmdir path
          end else
            Sys.remove path
        in
        (try rm path with _ -> ())
      ) (fun () -> f path)
    end
  in
  try_create 0

let test_maildir_create_mailbox () =
  with_temp_dir @@ fun tmp_path ->
  let storage = Maildir_storage.create_with_path ~base_path:tmp_path in
  Maildir_storage.ensure_user storage ~username:"test";

  (* Create a new mailbox *)
  let result = Maildir_storage.create_mailbox storage ~username:"test" "Drafts" in
  Alcotest.(check bool) "create success" true (Result.is_ok result);

  (* Creating same mailbox again should fail *)
  let result2 = Maildir_storage.create_mailbox storage ~username:"test" "Drafts" in
  Alcotest.(check bool) "duplicate fails" true (Result.is_error result2)

let test_maildir_delete_mailbox () =
  with_temp_dir @@ fun tmp_path ->
  let storage = Maildir_storage.create_with_path ~base_path:tmp_path in
  Maildir_storage.ensure_user storage ~username:"test";
  ignore (Maildir_storage.create_mailbox storage ~username:"test" "ToDelete");

  (* Delete the mailbox *)
  let result = Maildir_storage.delete_mailbox storage ~username:"test" "ToDelete" in
  Alcotest.(check bool) "delete success" true (Result.is_ok result);

  (* Deleting INBOX should fail *)
  let result2 = Maildir_storage.delete_mailbox storage ~username:"test" "INBOX" in
  Alcotest.(check bool) "delete inbox fails" true (Result.is_error result2)

let test_maildir_select_mailbox () =
  with_temp_dir @@ fun tmp_path ->
  let storage = Maildir_storage.create_with_path ~base_path:tmp_path in
  Maildir_storage.ensure_user storage ~username:"test";

  (* Select INBOX *)
  let result = Maildir_storage.select_mailbox storage ~username:"test" "INBOX" ~readonly:false in
  match result with
  | Ok state ->
    Alcotest.(check string) "mailbox name" "INBOX" state.name;
    Alcotest.(check bool) "not readonly" false state.readonly
  | Error _ -> Alcotest.fail "select failed"

let test_maildir_list_mailboxes () =
  with_temp_dir @@ fun tmp_path ->
  let storage = Maildir_storage.create_with_path ~base_path:tmp_path in
  Maildir_storage.ensure_user storage ~username:"test";
  ignore (Maildir_storage.create_mailbox storage ~username:"test" "Sent");
  ignore (Maildir_storage.create_mailbox storage ~username:"test" "Drafts");

  let mailboxes = Maildir_storage.list_mailboxes storage ~username:"test" ~reference:"" ~pattern:"*" in
  Alcotest.(check bool) "has mailboxes" true (List.length mailboxes >= 3)  (* INBOX + 2 created *)

let test_maildir_append_message () =
  with_temp_dir @@ fun tmp_path ->
  let storage = Maildir_storage.create_with_path ~base_path:tmp_path in
  Maildir_storage.ensure_user storage ~username:"test";

  let result = Maildir_storage.append storage
    ~username:"test"
    ~mailbox:"INBOX"
    ~flags:[System Seen]
    ~date:None
    ~message:"From: test@example.com\r\nSubject: Test\r\n\r\nHello World"
  in
  match result with
  | Ok uid ->
    Alcotest.(check bool) "uid is positive" true (uid > 0l)
  | Error _ -> Alcotest.fail "append failed"

let test_maildir_fetch_messages () =
  with_temp_dir @@ fun tmp_path ->
  let storage = Maildir_storage.create_with_path ~base_path:tmp_path in
  Maildir_storage.ensure_user storage ~username:"test";
  ignore (Maildir_storage.append storage
    ~username:"test"
    ~mailbox:"INBOX"
    ~flags:[]
    ~date:None
    ~message:"Test message");

  let result = Maildir_storage.fetch_messages storage
    ~username:"test"
    ~mailbox:"INBOX"
    ~sequence:[Single 1]
    ~items:[Fetch_flags]
  in
  match result with
  | Ok msgs ->
    Alcotest.(check int) "one message" 1 (List.length msgs)
  | Error _ -> Alcotest.fail "fetch failed"

let test_maildir_store_flags () =
  with_temp_dir @@ fun tmp_path ->
  let storage = Maildir_storage.create_with_path ~base_path:tmp_path in
  Maildir_storage.ensure_user storage ~username:"test";
  ignore (Maildir_storage.append storage
    ~username:"test"
    ~mailbox:"INBOX"
    ~flags:[]
    ~date:None
    ~message:"Test");

  let result = Maildir_storage.store_flags storage
    ~username:"test"
    ~mailbox:"INBOX"
    ~sequence:[Single 1]
    ~action:Store_add
    ~flags:[System Seen]
  in
  match result with
  | Ok msgs ->
    let msg = List.hd msgs in
    Alcotest.(check bool) "has Seen flag" true (List.mem (System Seen) msg.flags)
  | Error _ -> Alcotest.fail "store failed"

let test_maildir_expunge () =
  with_temp_dir @@ fun tmp_path ->
  let storage = Maildir_storage.create_with_path ~base_path:tmp_path in
  Maildir_storage.ensure_user storage ~username:"test";
  ignore (Maildir_storage.append storage
    ~username:"test"
    ~mailbox:"INBOX"
    ~flags:[System Deleted]
    ~date:None
    ~message:"To be deleted");

  let result = Maildir_storage.expunge storage ~username:"test" ~mailbox:"INBOX" in
  match result with
  | Ok uids ->
    Alcotest.(check int) "one expunged" 1 (List.length uids)
  | Error _ -> Alcotest.fail "expunge failed"

let test_maildir_flag_persistence () =
  with_temp_dir @@ fun tmp_path ->
  let storage = Maildir_storage.create_with_path ~base_path:tmp_path in
  Maildir_storage.ensure_user storage ~username:"test";

  (* Append message with flags *)
  ignore (Maildir_storage.append storage
    ~username:"test"
    ~mailbox:"INBOX"
    ~flags:[System Seen; System Flagged]
    ~date:None
    ~message:"Test with flags");

  (* Fetch and verify flags are preserved *)
  let result = Maildir_storage.fetch_messages storage
    ~username:"test"
    ~mailbox:"INBOX"
    ~sequence:[Single 1]
    ~items:[Fetch_flags]
  in
  match result with
  | Ok msgs ->
    let msg = List.hd msgs in
    Alcotest.(check bool) "has Seen" true (List.mem (System Seen) msg.flags);
    Alcotest.(check bool) "has Flagged" true (List.mem (System Flagged) msg.flags)
  | Error _ -> Alcotest.fail "fetch failed"

let () =
  let open Alcotest in
  run "imap_storage" [
    "memory_storage", [
      test_case "create_mailbox" `Quick test_memory_create_mailbox;
      test_case "delete_mailbox" `Quick test_memory_delete_mailbox;
      test_case "select_mailbox" `Quick test_memory_select_mailbox;
      test_case "list_mailboxes" `Quick test_memory_list_mailboxes;
      test_case "append_message" `Quick test_memory_append_message;
      test_case "fetch_messages" `Quick test_memory_fetch_messages;
      test_case "store_flags" `Quick test_memory_store_flags;
      test_case "expunge" `Quick test_memory_expunge;
    ];
    "maildir_storage", [
      test_case "create_mailbox" `Quick test_maildir_create_mailbox;
      test_case "delete_mailbox" `Quick test_maildir_delete_mailbox;
      test_case "select_mailbox" `Quick test_maildir_select_mailbox;
      test_case "list_mailboxes" `Quick test_maildir_list_mailboxes;
      test_case "append_message" `Quick test_maildir_append_message;
      test_case "fetch_messages" `Quick test_maildir_fetch_messages;
      test_case "store_flags" `Quick test_maildir_store_flags;
      test_case "expunge" `Quick test_maildir_expunge;
      test_case "flag_persistence" `Quick test_maildir_flag_persistence;
    ];
  ]
