(** Tests for imap_types module *)

open Imap_types

let test_normalize_mailbox_name () =
  Alcotest.(check string) "INBOX uppercase" "INBOX" (normalize_mailbox_name "INBOX");
  Alcotest.(check string) "inbox lowercase" "INBOX" (normalize_mailbox_name "inbox");
  Alcotest.(check string) "InBoX mixed" "INBOX" (normalize_mailbox_name "InBoX");
  Alcotest.(check string) "Other mailbox" "Sent" (normalize_mailbox_name "Sent")

let test_is_inbox () =
  Alcotest.(check bool) "INBOX is inbox" true (is_inbox "INBOX");
  Alcotest.(check bool) "inbox is inbox" true (is_inbox "inbox");
  Alcotest.(check bool) "Sent is not inbox" false (is_inbox "Sent")

let test_flag_to_string () =
  Alcotest.(check string) "Seen flag" "\\Seen" (flag_to_string (System Seen));
  Alcotest.(check string) "Answered flag" "\\Answered" (flag_to_string (System Answered));
  Alcotest.(check string) "Flagged flag" "\\Flagged" (flag_to_string (System Flagged));
  Alcotest.(check string) "Deleted flag" "\\Deleted" (flag_to_string (System Deleted));
  Alcotest.(check string) "Draft flag" "\\Draft" (flag_to_string (System Draft));
  Alcotest.(check string) "Keyword flag" "$Forwarded" (flag_to_string (Keyword "$Forwarded"))

let test_string_to_flag () =
  let flag_testable = Alcotest.testable
    (fun fmt f -> Format.pp_print_string fmt (match f with Some fl -> flag_to_string fl | None -> "None"))
    (fun a b -> a = b)
  in
  Alcotest.(check flag_testable) "Parse \\Seen" (Some (System Seen)) (string_to_flag "\\Seen");
  Alcotest.(check flag_testable) "Parse \\SEEN" (Some (System Seen)) (string_to_flag "\\SEEN");
  Alcotest.(check flag_testable) "Parse keyword" (Some (Keyword "$Junk")) (string_to_flag "$Junk");
  Alcotest.(check flag_testable) "Parse invalid" None (string_to_flag "\\Invalid")

let () =
  let open Alcotest in
  run "imap_types" [
    "mailbox_name", [
      test_case "normalize_mailbox_name" `Quick test_normalize_mailbox_name;
      test_case "is_inbox" `Quick test_is_inbox;
    ];
    "flags", [
      test_case "flag_to_string" `Quick test_flag_to_string;
      test_case "string_to_flag" `Quick test_string_to_flag;
    ];
  ]
