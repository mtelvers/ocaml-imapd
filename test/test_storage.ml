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

(* ===== Subscription Tests - Memory Storage ===== *)

let test_memory_subscribe () =
  let storage = Memory_storage.create () in
  Memory_storage.add_test_user storage ~username:"test";

  (* Subscribe to a mailbox *)
  let result = Memory_storage.subscribe storage ~username:"test" "INBOX" in
  Alcotest.(check bool) "subscribe success" true (Result.is_ok result);

  (* Check it's subscribed *)
  let is_sub = Memory_storage.is_subscribed storage ~username:"test" "INBOX" in
  Alcotest.(check bool) "is subscribed" true is_sub

let test_memory_unsubscribe () =
  let storage = Memory_storage.create () in
  Memory_storage.add_test_user storage ~username:"test";

  (* Subscribe then unsubscribe *)
  ignore (Memory_storage.subscribe storage ~username:"test" "INBOX");
  let result = Memory_storage.unsubscribe storage ~username:"test" "INBOX" in
  Alcotest.(check bool) "unsubscribe success" true (Result.is_ok result);

  (* Check it's no longer subscribed *)
  let is_sub = Memory_storage.is_subscribed storage ~username:"test" "INBOX" in
  Alcotest.(check bool) "not subscribed" false is_sub

let test_memory_list_subscribed () =
  let storage = Memory_storage.create () in
  Memory_storage.add_test_user storage ~username:"test";
  ignore (Memory_storage.create_mailbox storage ~username:"test" "Sent");
  ignore (Memory_storage.create_mailbox storage ~username:"test" "Drafts");

  (* Subscribe to multiple mailboxes *)
  ignore (Memory_storage.subscribe storage ~username:"test" "INBOX");
  ignore (Memory_storage.subscribe storage ~username:"test" "Sent");

  let subs = Memory_storage.list_subscribed storage ~username:"test" in
  Alcotest.(check int) "two subscriptions" 2 (List.length subs);
  Alcotest.(check bool) "has INBOX" true (List.mem "INBOX" subs);
  Alcotest.(check bool) "has Sent" true (List.mem "Sent" subs);
  Alcotest.(check bool) "no Drafts" false (List.mem "Drafts" subs)

(* ===== Subscription Tests - Maildir Storage ===== *)

let test_maildir_subscribe () =
  with_temp_dir @@ fun tmp_path ->
  let storage = Maildir_storage.create_with_path ~base_path:tmp_path in
  Maildir_storage.ensure_user storage ~username:"test";

  (* Subscribe to a mailbox *)
  let result = Maildir_storage.subscribe storage ~username:"test" "INBOX" in
  Alcotest.(check bool) "subscribe success" true (Result.is_ok result);

  (* Check it's subscribed *)
  let is_sub = Maildir_storage.is_subscribed storage ~username:"test" "INBOX" in
  Alcotest.(check bool) "is subscribed" true is_sub

let test_maildir_unsubscribe () =
  with_temp_dir @@ fun tmp_path ->
  let storage = Maildir_storage.create_with_path ~base_path:tmp_path in
  Maildir_storage.ensure_user storage ~username:"test";

  (* Subscribe then unsubscribe *)
  ignore (Maildir_storage.subscribe storage ~username:"test" "INBOX");
  let result = Maildir_storage.unsubscribe storage ~username:"test" "INBOX" in
  Alcotest.(check bool) "unsubscribe success" true (Result.is_ok result);

  (* Check it's no longer subscribed *)
  let is_sub = Maildir_storage.is_subscribed storage ~username:"test" "INBOX" in
  Alcotest.(check bool) "not subscribed" false is_sub

let test_maildir_list_subscribed () =
  with_temp_dir @@ fun tmp_path ->
  let storage = Maildir_storage.create_with_path ~base_path:tmp_path in
  Maildir_storage.ensure_user storage ~username:"test";
  ignore (Maildir_storage.create_mailbox storage ~username:"test" "Sent");
  ignore (Maildir_storage.create_mailbox storage ~username:"test" "Drafts");

  (* Subscribe to multiple mailboxes *)
  ignore (Maildir_storage.subscribe storage ~username:"test" "INBOX");
  ignore (Maildir_storage.subscribe storage ~username:"test" "Sent");

  let subs = Maildir_storage.list_subscribed storage ~username:"test" in
  Alcotest.(check int) "two subscriptions" 2 (List.length subs);
  Alcotest.(check bool) "has INBOX" true (List.mem "INBOX" subs);
  Alcotest.(check bool) "has Sent" true (List.mem "Sent" subs);
  Alcotest.(check bool) "no Drafts" false (List.mem "Drafts" subs)

let test_maildir_subscription_persistence () =
  with_temp_dir @@ fun tmp_path ->
  (* First storage instance - subscribe *)
  let storage1 = Maildir_storage.create_with_path ~base_path:tmp_path in
  Maildir_storage.ensure_user storage1 ~username:"test";
  ignore (Maildir_storage.subscribe storage1 ~username:"test" "INBOX");
  ignore (Maildir_storage.subscribe storage1 ~username:"test" "Sent");

  (* Second storage instance - verify persistence *)
  let storage2 = Maildir_storage.create_with_path ~base_path:tmp_path in
  let subs = Maildir_storage.list_subscribed storage2 ~username:"test" in
  Alcotest.(check int) "subscriptions persisted" 2 (List.length subs)

(* ===== Search Tests - Memory Storage ===== *)

let test_memory_search_all () =
  let storage = Memory_storage.create () in
  Memory_storage.add_test_user storage ~username:"test";
  ignore (Memory_storage.append storage ~username:"test" ~mailbox:"INBOX"
    ~flags:[] ~date:None ~message:"Message 1");
  ignore (Memory_storage.append storage ~username:"test" ~mailbox:"INBOX"
    ~flags:[] ~date:None ~message:"Message 2");

  match Memory_storage.search storage ~username:"test" ~mailbox:"INBOX" ~criteria:Search_all with
  | Ok uids -> Alcotest.(check int) "found all" 2 (List.length uids)
  | Error _ -> Alcotest.fail "search failed"

let test_memory_search_seen () =
  let storage = Memory_storage.create () in
  Memory_storage.add_test_user storage ~username:"test";
  ignore (Memory_storage.append storage ~username:"test" ~mailbox:"INBOX"
    ~flags:[System Seen] ~date:None ~message:"Seen message");
  ignore (Memory_storage.append storage ~username:"test" ~mailbox:"INBOX"
    ~flags:[] ~date:None ~message:"Unseen message");

  match Memory_storage.search storage ~username:"test" ~mailbox:"INBOX" ~criteria:Search_seen with
  | Ok uids -> Alcotest.(check int) "found seen" 1 (List.length uids)
  | Error _ -> Alcotest.fail "search failed"

let test_memory_search_text () =
  let storage = Memory_storage.create () in
  Memory_storage.add_test_user storage ~username:"test";
  ignore (Memory_storage.append storage ~username:"test" ~mailbox:"INBOX"
    ~flags:[] ~date:None ~message:"Subject: Hello World\r\n\r\nBody text");
  ignore (Memory_storage.append storage ~username:"test" ~mailbox:"INBOX"
    ~flags:[] ~date:None ~message:"Subject: Goodbye\r\n\r\nOther content");

  match Memory_storage.search storage ~username:"test" ~mailbox:"INBOX" ~criteria:(Search_text "Hello") with
  | Ok uids -> Alcotest.(check int) "found text" 1 (List.length uids)
  | Error _ -> Alcotest.fail "search failed"

let test_memory_search_subject () =
  let storage = Memory_storage.create () in
  Memory_storage.add_test_user storage ~username:"test";
  ignore (Memory_storage.append storage ~username:"test" ~mailbox:"INBOX"
    ~flags:[] ~date:None ~message:"Subject: Important Meeting\r\n\r\nBody");
  ignore (Memory_storage.append storage ~username:"test" ~mailbox:"INBOX"
    ~flags:[] ~date:None ~message:"Subject: Casual chat\r\n\r\nBody");

  match Memory_storage.search storage ~username:"test" ~mailbox:"INBOX" ~criteria:(Search_subject "Important") with
  | Ok uids -> Alcotest.(check int) "found subject" 1 (List.length uids)
  | Error _ -> Alcotest.fail "search failed"

let test_memory_search_from () =
  let storage = Memory_storage.create () in
  Memory_storage.add_test_user storage ~username:"test";
  ignore (Memory_storage.append storage ~username:"test" ~mailbox:"INBOX"
    ~flags:[] ~date:None ~message:"From: alice@example.com\r\nSubject: Test\r\n\r\nBody");
  ignore (Memory_storage.append storage ~username:"test" ~mailbox:"INBOX"
    ~flags:[] ~date:None ~message:"From: bob@example.com\r\nSubject: Test\r\n\r\nBody");

  match Memory_storage.search storage ~username:"test" ~mailbox:"INBOX" ~criteria:(Search_from "alice") with
  | Ok uids -> Alcotest.(check int) "found from" 1 (List.length uids)
  | Error _ -> Alcotest.fail "search failed"

let test_memory_search_larger () =
  let storage = Memory_storage.create () in
  Memory_storage.add_test_user storage ~username:"test";
  ignore (Memory_storage.append storage ~username:"test" ~mailbox:"INBOX"
    ~flags:[] ~date:None ~message:"Short");
  ignore (Memory_storage.append storage ~username:"test" ~mailbox:"INBOX"
    ~flags:[] ~date:None ~message:(String.make 1000 'x'));

  match Memory_storage.search storage ~username:"test" ~mailbox:"INBOX" ~criteria:(Search_larger 100L) with
  | Ok uids -> Alcotest.(check int) "found larger" 1 (List.length uids)
  | Error _ -> Alcotest.fail "search failed"

let test_memory_search_and () =
  let storage = Memory_storage.create () in
  Memory_storage.add_test_user storage ~username:"test";
  ignore (Memory_storage.append storage ~username:"test" ~mailbox:"INBOX"
    ~flags:[System Seen] ~date:None ~message:"Subject: Hello\r\n\r\nBody");
  ignore (Memory_storage.append storage ~username:"test" ~mailbox:"INBOX"
    ~flags:[] ~date:None ~message:"Subject: Hello\r\n\r\nBody");
  ignore (Memory_storage.append storage ~username:"test" ~mailbox:"INBOX"
    ~flags:[System Seen] ~date:None ~message:"Subject: Goodbye\r\n\r\nBody");

  let criteria = Search_and [Search_seen; Search_subject "Hello"] in
  match Memory_storage.search storage ~username:"test" ~mailbox:"INBOX" ~criteria with
  | Ok uids -> Alcotest.(check int) "found and" 1 (List.length uids)
  | Error _ -> Alcotest.fail "search failed"

let test_memory_search_or () =
  let storage = Memory_storage.create () in
  Memory_storage.add_test_user storage ~username:"test";
  ignore (Memory_storage.append storage ~username:"test" ~mailbox:"INBOX"
    ~flags:[System Flagged] ~date:None ~message:"Subject: Test\r\n\r\nBody");
  ignore (Memory_storage.append storage ~username:"test" ~mailbox:"INBOX"
    ~flags:[System Deleted] ~date:None ~message:"Subject: Test\r\n\r\nBody");
  ignore (Memory_storage.append storage ~username:"test" ~mailbox:"INBOX"
    ~flags:[] ~date:None ~message:"Subject: Test\r\n\r\nBody");

  let criteria = Search_or (Search_flagged, Search_deleted) in
  match Memory_storage.search storage ~username:"test" ~mailbox:"INBOX" ~criteria with
  | Ok uids -> Alcotest.(check int) "found or" 2 (List.length uids)
  | Error _ -> Alcotest.fail "search failed"

let test_memory_search_not () =
  let storage = Memory_storage.create () in
  Memory_storage.add_test_user storage ~username:"test";
  ignore (Memory_storage.append storage ~username:"test" ~mailbox:"INBOX"
    ~flags:[System Seen] ~date:None ~message:"Seen");
  ignore (Memory_storage.append storage ~username:"test" ~mailbox:"INBOX"
    ~flags:[] ~date:None ~message:"Unseen");

  match Memory_storage.search storage ~username:"test" ~mailbox:"INBOX" ~criteria:(Search_not Search_seen) with
  | Ok uids -> Alcotest.(check int) "found not seen" 1 (List.length uids)
  | Error _ -> Alcotest.fail "search failed"

(* ===== Search Tests - Maildir Storage ===== *)

let test_maildir_search_all () =
  with_temp_dir @@ fun tmp_path ->
  let storage = Maildir_storage.create_with_path ~base_path:tmp_path in
  Maildir_storage.ensure_user storage ~username:"test";
  ignore (Maildir_storage.append storage ~username:"test" ~mailbox:"INBOX"
    ~flags:[] ~date:None ~message:"Message 1");
  ignore (Maildir_storage.append storage ~username:"test" ~mailbox:"INBOX"
    ~flags:[] ~date:None ~message:"Message 2");

  match Maildir_storage.search storage ~username:"test" ~mailbox:"INBOX" ~criteria:Search_all with
  | Ok uids -> Alcotest.(check int) "found all" 2 (List.length uids)
  | Error _ -> Alcotest.fail "search failed"

let test_maildir_search_seen () =
  with_temp_dir @@ fun tmp_path ->
  let storage = Maildir_storage.create_with_path ~base_path:tmp_path in
  Maildir_storage.ensure_user storage ~username:"test";
  ignore (Maildir_storage.append storage ~username:"test" ~mailbox:"INBOX"
    ~flags:[System Seen] ~date:None ~message:"Seen message");
  ignore (Maildir_storage.append storage ~username:"test" ~mailbox:"INBOX"
    ~flags:[] ~date:None ~message:"Unseen message");

  match Maildir_storage.search storage ~username:"test" ~mailbox:"INBOX" ~criteria:Search_seen with
  | Ok uids -> Alcotest.(check int) "found seen" 1 (List.length uids)
  | Error _ -> Alcotest.fail "search failed"

let test_maildir_search_subject () =
  with_temp_dir @@ fun tmp_path ->
  let storage = Maildir_storage.create_with_path ~base_path:tmp_path in
  Maildir_storage.ensure_user storage ~username:"test";
  ignore (Maildir_storage.append storage ~username:"test" ~mailbox:"INBOX"
    ~flags:[] ~date:None ~message:"Subject: Important Meeting\r\n\r\nBody");
  ignore (Maildir_storage.append storage ~username:"test" ~mailbox:"INBOX"
    ~flags:[] ~date:None ~message:"Subject: Casual chat\r\n\r\nBody");

  match Maildir_storage.search storage ~username:"test" ~mailbox:"INBOX" ~criteria:(Search_subject "Important") with
  | Ok uids -> Alcotest.(check int) "found subject" 1 (List.length uids)
  | Error _ -> Alcotest.fail "search failed"

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
    "memory_subscriptions", [
      test_case "subscribe" `Quick test_memory_subscribe;
      test_case "unsubscribe" `Quick test_memory_unsubscribe;
      test_case "list_subscribed" `Quick test_memory_list_subscribed;
    ];
    "memory_search", [
      test_case "search_all" `Quick test_memory_search_all;
      test_case "search_seen" `Quick test_memory_search_seen;
      test_case "search_text" `Quick test_memory_search_text;
      test_case "search_subject" `Quick test_memory_search_subject;
      test_case "search_from" `Quick test_memory_search_from;
      test_case "search_larger" `Quick test_memory_search_larger;
      test_case "search_and" `Quick test_memory_search_and;
      test_case "search_or" `Quick test_memory_search_or;
      test_case "search_not" `Quick test_memory_search_not;
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
    "maildir_subscriptions", [
      test_case "subscribe" `Quick test_maildir_subscribe;
      test_case "unsubscribe" `Quick test_maildir_unsubscribe;
      test_case "list_subscribed" `Quick test_maildir_list_subscribed;
      test_case "subscription_persistence" `Quick test_maildir_subscription_persistence;
    ];
    "maildir_search", [
      test_case "search_all" `Quick test_maildir_search_all;
      test_case "search_seen" `Quick test_maildir_search_seen;
      test_case "search_subject" `Quick test_maildir_search_subject;
    ];
  ]
