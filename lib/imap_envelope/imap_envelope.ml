(** IMAP Envelope Parser - RFC 5322 Header Parsing

    Parses email headers to build IMAP ENVELOPE structure.

    References:
    - {{:https://datatracker.ietf.org/doc/html/rfc5322}RFC 5322} - Internet Message Format
    - {{:https://datatracker.ietf.org/doc/html/rfc9051#section-2.3.5}RFC 9051 Section 2.3.5} - ENVELOPE *)

open Imap_types

(** {1 Header Parsing} *)

(** Split raw message into headers and body.
    Headers end at the first blank line (CRLF CRLF or LF LF). *)
let split_headers_body raw =
  (* Try CRLF first, then LF *)
  let crlf_sep = "\r\n\r\n" in
  let lf_sep = "\n\n" in
  match String.index_opt raw '\r' with
  | Some _ ->
    (match String.split_on_char '\000' (Str.global_replace (Str.regexp_string crlf_sep) "\000" raw) with
     | headers :: rest -> (headers, String.concat crlf_sep rest)
     | [] -> (raw, ""))
  | None ->
    (match String.split_on_char '\000' (Str.global_replace (Str.regexp_string lf_sep) "\000" raw) with
     | headers :: rest -> (headers, String.concat lf_sep rest)
     | [] -> (raw, ""))

(** Unfold header lines (RFC 5322 Section 2.2.3).
    Continuation lines start with whitespace. *)
let unfold_headers headers =
  (* Replace CRLF + whitespace with single space *)
  let s = Str.global_replace (Str.regexp "\r\n[ \t]+") " " headers in
  (* Also handle LF + whitespace *)
  Str.global_replace (Str.regexp "\n[ \t]+") " " s

(** Parse headers into (name, value) pairs.
    Header names are case-insensitive, stored lowercase. *)
let parse_header_lines headers =
  let unfolded = unfold_headers headers in
  let lines = Str.split (Str.regexp "\r\n\\|\n") unfolded in
  List.filter_map (fun line ->
    match String.index_opt line ':' with
    | None -> None
    | Some i ->
      let name = String.lowercase_ascii (String.trim (String.sub line 0 i)) in
      let value = String.trim (String.sub line (i + 1) (String.length line - i - 1)) in
      Some (name, value)
  ) lines

(** Get header value by name (case-insensitive). *)
let get_header headers name =
  let name = String.lowercase_ascii name in
  List.find_map (fun (n, v) -> if n = name then Some v else None) headers

(** {1 Address Parsing - RFC 5322 Section 3.4} *)

(** Remove RFC 5322 comments from a string.
    Comments are nested parentheses: (comment (nested) here) *)
let remove_comments s =
  let buf = Buffer.create (String.length s) in
  let depth = ref 0 in
  let escaped = ref false in
  String.iter (fun c ->
    if !escaped then begin
      if !depth = 0 then Buffer.add_char buf c;
      escaped := false
    end else match c with
      | '\\' -> escaped := true; if !depth = 0 then Buffer.add_char buf c
      | '(' -> incr depth
      | ')' -> if !depth > 0 then decr depth
      | _ -> if !depth = 0 then Buffer.add_char buf c
  ) s;
  Buffer.contents buf

(** Decode RFC 2047 encoded words in header values.
    Format: =?charset?encoding?encoded_text?= *)
let decode_encoded_word s =
  let re = Str.regexp "=\\?\\([^?]+\\)\\?\\([BbQq]\\)\\?\\([^?]*\\)\\?=" in
  let decode_match _ =
    try
      let _charset = Str.matched_group 1 s in
      let encoding = String.uppercase_ascii (Str.matched_group 2 s) in
      let text = Str.matched_group 3 s in
      match encoding with
      | "B" ->
        (* Base64 *)
        (match Base64.decode text with
         | Ok decoded -> decoded
         | Error _ -> Str.matched_string s)
      | "Q" ->
        (* Quoted-printable, with _ for space *)
        let text = Str.global_replace (Str.regexp "_") " " text in
        let buf = Buffer.create (String.length text) in
        let i = ref 0 in
        while !i < String.length text do
          if text.[!i] = '=' && !i + 2 < String.length text then begin
            let hex = String.sub text (!i + 1) 2 in
            (try
               Buffer.add_char buf (Char.chr (int_of_string ("0x" ^ hex)));
               i := !i + 3
             with _ ->
               Buffer.add_char buf text.[!i];
               incr i)
          end else begin
            Buffer.add_char buf text.[!i];
            incr i
          end
        done;
        Buffer.contents buf
      | _ -> Str.matched_string s
    with _ -> Str.matched_string s
  in
  Str.global_substitute re decode_match s

(** Parse a single email address.
    Handles formats:
    - user@domain
    - <user@domain>
    - Display Name <user@domain>
    - "Quoted Name" <user@domain> *)
let parse_single_address s =
  let s = String.trim (remove_comments s) in
  if s = "" then None
  else
    (* Check for angle bracket format *)
    match String.rindex_opt s '>' with
    | Some end_pos ->
      (match String.rindex_opt s '<' with
       | Some start_pos when start_pos < end_pos ->
         let addr = String.sub s (start_pos + 1) (end_pos - start_pos - 1) in
         let display = String.trim (String.sub s 0 start_pos) in
         (* Remove quotes from display name *)
         let display =
           if String.length display >= 2 && display.[0] = '"' && display.[String.length display - 1] = '"' then
             String.sub display 1 (String.length display - 2)
           else display
         in
         let display = decode_encoded_word display in
         (match String.index_opt addr '@' with
          | Some at_pos ->
            let mailbox = String.sub addr 0 at_pos in
            let host = String.sub addr (at_pos + 1) (String.length addr - at_pos - 1) in
            Some {
              name = if display = "" then None else Some display;
              adl = None;
              mailbox = Some mailbox;
              host = Some host;
            }
          | None ->
            (* No @ in address - might be a local address *)
            Some {
              name = if display = "" then None else Some display;
              adl = None;
              mailbox = Some addr;
              host = None;
            })
       | _ -> None)
    | None ->
      (* Plain address without angle brackets *)
      (match String.index_opt s '@' with
       | Some at_pos ->
         let mailbox = String.sub s 0 at_pos in
         let host = String.sub s (at_pos + 1) (String.length s - at_pos - 1) in
         Some {
           name = None;
           adl = None;
           mailbox = Some mailbox;
           host = Some host;
         }
       | None ->
         (* Just a name or invalid *)
         if s <> "" then
           Some {
             name = Some (decode_encoded_word s);
             adl = None;
             mailbox = None;
             host = None;
           }
         else None)

(** Parse an address list (comma-separated addresses).
    Also handles group syntax: group-name: addr1, addr2; *)
let parse_address_list s =
  let s = String.trim s in
  if s = "" then []
  else
    (* Handle group syntax - look for : and ; *)
    let s = Str.global_replace (Str.regexp "[^:]+:") "" s in  (* Remove group name *)
    let s = Str.global_replace (Str.regexp ";") "" s in        (* Remove group terminator *)

    (* Split on commas, but be careful about commas in quoted strings *)
    let parts = ref [] in
    let current = Buffer.create 64 in
    let in_quotes = ref false in
    let in_angle = ref false in
    let depth = ref 0 in
    String.iter (fun c ->
      match c with
      | '"' when !depth = 0 ->
        in_quotes := not !in_quotes;
        Buffer.add_char current c
      | '<' when not !in_quotes ->
        in_angle := true;
        Buffer.add_char current c
      | '>' when not !in_quotes ->
        in_angle := false;
        Buffer.add_char current c
      | '(' when not !in_quotes ->
        incr depth;
        Buffer.add_char current c
      | ')' when not !in_quotes && !depth > 0 ->
        decr depth;
        Buffer.add_char current c
      | ',' when not !in_quotes && not !in_angle && !depth = 0 ->
        parts := Buffer.contents current :: !parts;
        Buffer.clear current
      | _ ->
        Buffer.add_char current c
    ) s;
    if Buffer.length current > 0 then
      parts := Buffer.contents current :: !parts;

    List.filter_map parse_single_address (List.rev !parts)

(** {1 Envelope Construction} *)

(** Build envelope from parsed headers. *)
let envelope_from_headers headers =
  let from = match get_header headers "from" with
    | Some v -> parse_address_list v
    | None -> []
  in
  let sender = match get_header headers "sender" with
    | Some v -> parse_address_list v
    | None -> from  (* RFC 5322: defaults to From *)
  in
  let reply_to = match get_header headers "reply-to" with
    | Some v -> parse_address_list v
    | None -> from  (* RFC 5322: defaults to From *)
  in
  {
    date = get_header headers "date";
    subject = Option.map decode_encoded_word (get_header headers "subject");
    from;
    sender;
    reply_to;
    to_ = (match get_header headers "to" with Some v -> parse_address_list v | None -> []);
    cc = (match get_header headers "cc" with Some v -> parse_address_list v | None -> []);
    bcc = (match get_header headers "bcc" with Some v -> parse_address_list v | None -> []);
    in_reply_to = get_header headers "in-reply-to";
    message_id = get_header headers "message-id";
  }

(** Parse raw message and extract envelope. *)
let parse_envelope raw_message =
  let headers_str, _body = split_headers_body raw_message in
  let headers = parse_header_lines headers_str in
  envelope_from_headers headers

(** Parse raw message and return both headers and body separately. *)
let parse_message raw_message =
  let headers_str, body = split_headers_body raw_message in
  (headers_str, body)
