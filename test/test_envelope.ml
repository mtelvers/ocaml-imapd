(** Tests for imap_envelope module *)

open Imap_envelope

(* Test header parsing *)
let test_split_headers_body () =
  let message = "Subject: Test\r\nFrom: test@example.com\r\n\r\nBody content" in
  let headers, body = split_headers_body message in
  Alcotest.(check bool) "headers not empty" true (String.length headers > 0);
  Alcotest.(check bool) "body not empty" true (String.length body > 0);
  Alcotest.(check bool) "body contains content" true (body = "Body content")

let test_parse_header_lines () =
  let headers = "Subject: Test Message\r\nFrom: alice@example.com\r\nTo: bob@example.com" in
  let parsed = parse_header_lines headers in
  Alcotest.(check int) "three headers" 3 (List.length parsed);
  Alcotest.(check (option string)) "subject" (Some "Test Message") (get_header parsed "Subject");
  Alcotest.(check (option string)) "from" (Some "alice@example.com") (get_header parsed "From")

let test_get_header_case_insensitive () =
  let headers = [("subject", "Test"); ("from", "alice@example.com")] in
  Alcotest.(check (option string)) "lowercase" (Some "Test") (get_header headers "subject");
  Alcotest.(check (option string)) "uppercase" (Some "Test") (get_header headers "SUBJECT");
  Alcotest.(check (option string)) "mixed" (Some "Test") (get_header headers "Subject")

(* Test address parsing *)
let test_parse_single_address_simple () =
  match parse_single_address "alice@example.com" with
  | Some addr ->
    Alcotest.(check (option string)) "mailbox" (Some "alice") addr.mailbox;
    Alcotest.(check (option string)) "host" (Some "example.com") addr.host
  | None -> Alcotest.fail "Failed to parse address"

let test_parse_single_address_with_name () =
  match parse_single_address "Alice Smith <alice@example.com>" with
  | Some addr ->
    Alcotest.(check (option string)) "name" (Some "Alice Smith") addr.name;
    Alcotest.(check (option string)) "mailbox" (Some "alice") addr.mailbox;
    Alcotest.(check (option string)) "host" (Some "example.com") addr.host
  | None -> Alcotest.fail "Failed to parse address"

let test_parse_single_address_quoted_name () =
  match parse_single_address "\"Alice Smith\" <alice@example.com>" with
  | Some addr ->
    Alcotest.(check (option string)) "name" (Some "Alice Smith") addr.name;
    Alcotest.(check (option string)) "mailbox" (Some "alice") addr.mailbox
  | None -> Alcotest.fail "Failed to parse address"

let test_parse_address_list () =
  let addrs = parse_address_list "alice@example.com, bob@example.com" in
  Alcotest.(check int) "two addresses" 2 (List.length addrs)

(* Test RFC 2047 decoding *)
let test_decode_encoded_word_base64 () =
  let encoded = "=?UTF-8?B?SGVsbG8gV29ybGQ=?=" in
  let decoded = decode_encoded_word encoded in
  Alcotest.(check string) "decoded" "Hello World" decoded

let test_decode_encoded_word_quoted_printable () =
  let encoded = "=?UTF-8?Q?Hello_World?=" in
  let decoded = decode_encoded_word encoded in
  Alcotest.(check string) "decoded" "Hello World" decoded

let test_decode_plain_text () =
  let plain = "Plain text" in
  let decoded = decode_encoded_word plain in
  Alcotest.(check string) "unchanged" "Plain text" decoded

(* Test envelope construction *)
let test_envelope_from_headers () =
  let headers = [
    ("date", "Mon, 20 Jan 2025 10:00:00 +0000");
    ("subject", "Test Subject");
    ("from", "alice@example.com");
    ("to", "bob@example.com");
  ] in
  let env = envelope_from_headers headers in
  Alcotest.(check (option string)) "date" (Some "Mon, 20 Jan 2025 10:00:00 +0000") env.date;
  Alcotest.(check (option string)) "subject" (Some "Test Subject") env.subject;
  Alcotest.(check int) "one from" 1 (List.length env.from);
  Alcotest.(check int) "one to" 1 (List.length env.to_)

let test_parse_envelope () =
  let message = "Date: Mon, 20 Jan 2025 10:00:00 +0000\r\nSubject: Test\r\nFrom: alice@example.com\r\n\r\nBody" in
  let env = parse_envelope message in
  Alcotest.(check (option string)) "subject" (Some "Test") env.subject

(* Test body structure parsing *)
let test_parse_body_structure_simple () =
  let message = "Content-Type: text/plain\r\n\r\nSimple body" in
  let bs = parse_body_structure message in
  match bs.body_type with
  | Imap_types.Text { subtype; _ } ->
    Alcotest.(check string) "subtype" "PLAIN" subtype
  | Imap_types.Basic { media_type; subtype; _ } ->
    Alcotest.(check string) "type" "TEXT" media_type;
    Alcotest.(check string) "subtype" "PLAIN" subtype
  | _ -> Alcotest.fail "Expected Text or Basic body type"

(* Test MIME part extraction *)
let test_extract_mime_part_simple () =
  let message = "Content-Type: text/plain\r\n\r\nSimple body content" in
  match extract_mime_part message "1" with
  | Some content ->
    Alcotest.(check bool) "has content" true (String.length content > 0)
  | None -> Alcotest.fail "Failed to extract part"

let test_extract_mime_part_multipart () =
  let message = String.concat "\r\n" [
    "Content-Type: multipart/mixed; boundary=\"boundary123\"";
    "";
    "--boundary123";
    "Content-Type: text/plain";
    "";
    "Part 1 content";
    "--boundary123";
    "Content-Type: text/html";
    "";
    "<html>Part 2</html>";
    "--boundary123--"
  ] in
  match extract_mime_part message "1" with
  | Some content -> Alcotest.(check bool) "part 1 exists" true (String.length content > 0)
  | None -> Alcotest.fail "Failed to extract part 1"

(* Test binary extraction with decoding *)
let test_extract_binary_part_base64 () =
  let message = String.concat "\r\n" [
    "Content-Type: text/plain";
    "Content-Transfer-Encoding: base64";
    "";
    "SGVsbG8gV29ybGQ="  (* "Hello World" in base64 *)
  ] in
  match extract_binary_part message "1" with
  | Some decoded -> Alcotest.(check string) "decoded" "Hello World" decoded
  | None -> Alcotest.fail "Failed to decode base64"

let test_extract_binary_part_quoted_printable () =
  let message = String.concat "\r\n" [
    "Content-Type: text/plain";
    "Content-Transfer-Encoding: quoted-printable";
    "";
    "Hello=20World"
  ] in
  match extract_binary_part message "1" with
  | Some decoded -> Alcotest.(check string) "decoded" "Hello World" decoded
  | None -> Alcotest.fail "Failed to decode quoted-printable"

let test_extract_binary_part_7bit () =
  let message = String.concat "\r\n" [
    "Content-Type: text/plain";
    "Content-Transfer-Encoding: 7bit";
    "";
    "Plain text"
  ] in
  match extract_binary_part message "1" with
  | Some decoded -> Alcotest.(check string) "unchanged" "Plain text" decoded
  | None -> Alcotest.fail "Failed to extract 7bit content"

let test_get_binary_part_size () =
  let message = String.concat "\r\n" [
    "Content-Type: text/plain";
    "Content-Transfer-Encoding: base64";
    "";
    "SGVsbG8gV29ybGQ="
  ] in
  match get_binary_part_size message "1" with
  | Some size -> Alcotest.(check int64) "size" 11L size  (* "Hello World" = 11 bytes *)
  | None -> Alcotest.fail "Failed to get size"

let () =
  let open Alcotest in
  run "imap_envelope" [
    "header_parsing", [
      test_case "split_headers_body" `Quick test_split_headers_body;
      test_case "parse_header_lines" `Quick test_parse_header_lines;
      test_case "get_header_case_insensitive" `Quick test_get_header_case_insensitive;
    ];
    "address_parsing", [
      test_case "simple address" `Quick test_parse_single_address_simple;
      test_case "address with name" `Quick test_parse_single_address_with_name;
      test_case "quoted name" `Quick test_parse_single_address_quoted_name;
      test_case "address list" `Quick test_parse_address_list;
    ];
    "rfc2047_decoding", [
      test_case "base64 encoded word" `Quick test_decode_encoded_word_base64;
      test_case "quoted-printable encoded word" `Quick test_decode_encoded_word_quoted_printable;
      test_case "plain text unchanged" `Quick test_decode_plain_text;
    ];
    "envelope", [
      test_case "envelope_from_headers" `Quick test_envelope_from_headers;
      test_case "parse_envelope" `Quick test_parse_envelope;
    ];
    "body_structure", [
      test_case "simple body structure" `Quick test_parse_body_structure_simple;
    ];
    "mime_extraction", [
      test_case "simple part" `Quick test_extract_mime_part_simple;
      test_case "multipart" `Quick test_extract_mime_part_multipart;
    ];
    "binary_extraction", [
      test_case "base64 decoding" `Quick test_extract_binary_part_base64;
      test_case "quoted-printable decoding" `Quick test_extract_binary_part_quoted_printable;
      test_case "7bit passthrough" `Quick test_extract_binary_part_7bit;
      test_case "binary size" `Quick test_get_binary_part_size;
    ];
  ]
