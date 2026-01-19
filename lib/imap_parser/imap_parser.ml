(** IMAP4rev2 Parser

    Implements {{:https://datatracker.ietf.org/doc/html/rfc9051#section-9}RFC 9051 Section 9} Formal Syntax.

    This module uses Menhir for parsing and Faraday for response serialization. *)

open Imap_types

(* Re-export types from Imap_types for backward compatibility *)
type command = Imap_types.command =
  | Capability
  | Noop
  | Logout
  | Starttls
  | Login of { username : string; password : string }
  | Authenticate of { mechanism : string; initial_response : string option }
  | Enable of string list
  | Select of mailbox_name
  | Examine of mailbox_name
  | Create of mailbox_name
  | Delete of mailbox_name
  | Rename of { old_name : mailbox_name; new_name : mailbox_name }
  | Subscribe of mailbox_name
  | Unsubscribe of mailbox_name
  | List of { reference : string; pattern : string }
  | Namespace
  | Status of { mailbox : mailbox_name; items : status_item list }
  | Append of { mailbox : mailbox_name; flags : flag list; date : string option; message : string }
  | Idle
  | Close
  | Unselect
  | Expunge
  | Search of { charset : string option; criteria : search_key }
  | Fetch of { sequence : sequence_set; items : fetch_item list }
  | Store of { sequence : sequence_set; silent : bool; action : store_action; flags : flag list }
  | Copy of { sequence : sequence_set; mailbox : mailbox_name }
  | Move of { sequence : sequence_set; mailbox : mailbox_name }
  | Uid of uid_command
  | Id of (string * string) list option

type uid_command = Imap_types.uid_command =
  | Uid_fetch of { sequence : sequence_set; items : fetch_item list }
  | Uid_store of { sequence : sequence_set; silent : bool; action : store_action; flags : flag list }
  | Uid_copy of { sequence : sequence_set; mailbox : mailbox_name }
  | Uid_move of { sequence : sequence_set; mailbox : mailbox_name }
  | Uid_search of { charset : string option; criteria : search_key }
  | Uid_expunge of sequence_set

type tagged_command = Imap_types.tagged_command = {
  tag : string;
  command : command;
}

type response = Imap_types.response =
  | Ok of { tag : string option; code : response_code option; text : string }
  | No of { tag : string option; code : response_code option; text : string }
  | Bad of { tag : string option; code : response_code option; text : string }
  | Preauth of { code : response_code option; text : string }
  | Bye of { code : response_code option; text : string }
  | Capability_response of string list
  | Enabled of string list
  | List_response of { flags : list_flag list; delimiter : char option; name : mailbox_name }
  | Namespace_response of namespace_data
  | Status_response of { mailbox : mailbox_name; items : (status_item * int64) list }
  | Esearch of { tag : string option; uid : bool; results : esearch_result list }
  | Flags_response of flag list
  | Exists of int
  | Expunge_response of int
  | Fetch_response of { seq : int; items : fetch_response_item list }
  | Continuation of string option
  | Id_response of (string * string) list option

(* ===== Menhir Parser Interface ===== *)

let parse_command input =
  let lexbuf = Lexing.from_string input in
  try
    Result.Ok (Imap_grammar.command Imap_lexer.token lexbuf)
  with
  | Imap_lexer.Lexer_error msg -> Result.Error ("Lexer error: " ^ msg)
  | Imap_grammar.Error ->
    let pos = lexbuf.Lexing.lex_curr_p in
    Result.Error (Printf.sprintf "Parse error at line %d, column %d"
             pos.Lexing.pos_lnum
             (pos.Lexing.pos_cnum - pos.Lexing.pos_bol))

(* ===== Faraday Response Serializer ===== *)

let crlf = "\r\n"

let write_string f s = Faraday.write_string f s
let write_char f c = Faraday.write_char f c
let write_sp f = write_char f ' '
let write_crlf f = write_string f crlf

let write_quoted_string f s =
  write_char f '"';
  String.iter (fun c ->
    match c with
    | '"' | '\\' -> write_char f '\\'; write_char f c
    | _ -> write_char f c
  ) s;
  write_char f '"'

let write_literal f s =
  write_char f '{';
  write_string f (string_of_int (String.length s));
  write_string f "}\r\n";
  write_string f s

let write_flag f flag =
  write_string f (flag_to_string flag)

let write_flag_list f flags =
  write_char f '(';
  List.iteri (fun i flag ->
    if i > 0 then write_sp f;
    write_flag f flag
  ) flags;
  write_char f ')'

(* Write NIL or quoted string for envelope fields *)
let write_nstring f = function
  | None -> write_string f "NIL"
  | Some s -> write_quoted_string f s

(* Write a single address per RFC 9051 *)
let write_address f (addr : address) =
  write_char f '(';
  write_nstring f addr.name;
  write_sp f;
  write_nstring f addr.adl;
  write_sp f;
  write_nstring f addr.mailbox;
  write_sp f;
  write_nstring f addr.host;
  write_char f ')'

(* Write an address list (NIL if empty, or parenthesized list) *)
let write_address_list f addrs =
  match addrs with
  | [] -> write_string f "NIL"
  | _ ->
    write_char f '(';
    List.iter (fun addr -> write_address f addr) addrs;
    write_char f ')'

(* Write ENVELOPE per RFC 9051 Section 7.5.2 *)
let write_envelope f (env : envelope) =
  write_char f '(';
  write_nstring f env.date;
  write_sp f;
  write_nstring f env.subject;
  write_sp f;
  write_address_list f env.from;
  write_sp f;
  write_address_list f env.sender;
  write_sp f;
  write_address_list f env.reply_to;
  write_sp f;
  write_address_list f env.to_;
  write_sp f;
  write_address_list f env.cc;
  write_sp f;
  write_address_list f env.bcc;
  write_sp f;
  write_nstring f env.in_reply_to;
  write_sp f;
  write_nstring f env.message_id;
  write_char f ')'

(* Write body parameters as parenthesized list or NIL *)
let write_body_params f params =
  match params with
  | [] -> write_string f "NIL"
  | _ ->
    write_char f '(';
    List.iteri (fun i (k, v) ->
      if i > 0 then write_sp f;
      write_quoted_string f k;
      write_sp f;
      write_quoted_string f v
    ) params;
    write_char f ')'

(* Write body fields: (params) content-id description encoding size *)
let write_body_fields f (fields : body_fields) =
  write_body_params f fields.params;
  write_sp f;
  write_nstring f fields.content_id;
  write_sp f;
  write_nstring f fields.description;
  write_sp f;
  write_quoted_string f fields.encoding;
  write_sp f;
  write_string f (Int64.to_string fields.size)

(* Write body disposition: NIL or (name (param-list)) *)
let write_body_disposition f disp =
  match disp with
  | None -> write_string f "NIL"
  | Some (name, params) ->
    write_char f '(';
    write_quoted_string f name;
    write_sp f;
    write_body_params f params;
    write_char f ')'

(* Write language list *)
let write_body_language f lang =
  match lang with
  | None -> write_string f "NIL"
  | Some [] -> write_string f "NIL"
  | Some [l] -> write_quoted_string f l
  | Some langs ->
    write_char f '(';
    List.iteri (fun i l ->
      if i > 0 then write_sp f;
      write_quoted_string f l
    ) langs;
    write_char f ')'

(* Write BODY structure (basic, without extension data) *)
let rec write_body f (bs : body_structure) =
  match bs.body_type with
  | Text { subtype; fields; lines } ->
    write_char f '(';
    write_quoted_string f "TEXT";
    write_sp f;
    write_quoted_string f subtype;
    write_sp f;
    write_body_fields f fields;
    write_sp f;
    write_string f (Int64.to_string lines);
    write_char f ')'
  | Basic { media_type; subtype; fields } ->
    write_char f '(';
    write_quoted_string f media_type;
    write_sp f;
    write_quoted_string f subtype;
    write_sp f;
    write_body_fields f fields;
    write_char f ')'
  | Message_rfc822 { fields; envelope; body; lines } ->
    write_char f '(';
    write_quoted_string f "MESSAGE";
    write_sp f;
    write_quoted_string f "RFC822";
    write_sp f;
    write_body_fields f fields;
    write_sp f;
    write_envelope f envelope;
    write_sp f;
    write_body f body;
    write_sp f;
    write_string f (Int64.to_string lines);
    write_char f ')'
  | Multipart { subtype; parts; params = _ } ->
    write_char f '(';
    List.iter (fun part -> write_body f part) parts;
    write_sp f;
    write_quoted_string f subtype;
    write_char f ')'

(* Write BODYSTRUCTURE (extended, with extension data) *)
let rec write_bodystructure f (bs : body_structure) =
  match bs.body_type with
  | Text { subtype; fields; lines } ->
    write_char f '(';
    write_quoted_string f "TEXT";
    write_sp f;
    write_quoted_string f subtype;
    write_sp f;
    write_body_fields f fields;
    write_sp f;
    write_string f (Int64.to_string lines);
    (* Extension data: md5, disposition, language, location *)
    write_sp f;
    write_string f "NIL";  (* md5 *)
    write_sp f;
    write_body_disposition f bs.disposition;
    write_sp f;
    write_body_language f bs.language;
    write_sp f;
    write_nstring f bs.location;
    write_char f ')'
  | Basic { media_type; subtype; fields } ->
    write_char f '(';
    write_quoted_string f media_type;
    write_sp f;
    write_quoted_string f subtype;
    write_sp f;
    write_body_fields f fields;
    (* Extension data *)
    write_sp f;
    write_string f "NIL";  (* md5 *)
    write_sp f;
    write_body_disposition f bs.disposition;
    write_sp f;
    write_body_language f bs.language;
    write_sp f;
    write_nstring f bs.location;
    write_char f ')'
  | Message_rfc822 { fields; envelope; body; lines } ->
    write_char f '(';
    write_quoted_string f "MESSAGE";
    write_sp f;
    write_quoted_string f "RFC822";
    write_sp f;
    write_body_fields f fields;
    write_sp f;
    write_envelope f envelope;
    write_sp f;
    write_bodystructure f body;
    write_sp f;
    write_string f (Int64.to_string lines);
    (* Extension data *)
    write_sp f;
    write_string f "NIL";  (* md5 *)
    write_sp f;
    write_body_disposition f bs.disposition;
    write_sp f;
    write_body_language f bs.language;
    write_sp f;
    write_nstring f bs.location;
    write_char f ')'
  | Multipart { subtype; parts; params } ->
    write_char f '(';
    List.iter (fun part -> write_bodystructure f part) parts;
    write_sp f;
    write_quoted_string f subtype;
    (* Extension data for multipart *)
    write_sp f;
    write_body_params f params;
    write_sp f;
    write_body_disposition f bs.disposition;
    write_sp f;
    write_body_language f bs.language;
    write_sp f;
    write_nstring f bs.location;
    write_char f ')'

let write_response_code f code =
  write_char f '[';
  (match code with
   | Code_alert -> write_string f "ALERT"
   | Code_alreadyexists -> write_string f "ALREADYEXISTS"
   | Code_capability caps ->
     write_string f "CAPABILITY";
     List.iter (fun c -> write_sp f; write_string f c) caps
   | Code_permanentflags flags ->
     write_string f "PERMANENTFLAGS ";
     write_flag_list f flags
   | Code_readonly -> write_string f "READ-ONLY"
   | Code_readwrite -> write_string f "READ-WRITE"
   | Code_uidvalidity v ->
     write_string f "UIDVALIDITY ";
     write_string f (Int32.to_string v)
   | Code_uidnext u ->
     write_string f "UIDNEXT ";
     write_string f (Int32.to_string u)
   | Code_appenduid (v, u) ->
     write_string f "APPENDUID ";
     write_string f (Int32.to_string v);
     write_sp f;
     write_string f (Int32.to_string u)
   | Code_trycreate -> write_string f "TRYCREATE"
   | Code_nonexistent -> write_string f "NONEXISTENT"
   | Code_authenticationfailed -> write_string f "AUTHENTICATIONFAILED"
   | Code_authorizationfailed -> write_string f "AUTHORIZATIONFAILED"
   | Code_parse -> write_string f "PARSE"
   | Code_closed -> write_string f "CLOSED"
   | Code_cannot -> write_string f "CANNOT"
   | Code_noperm -> write_string f "NOPERM"
   | Code_overquota -> write_string f "OVERQUOTA"
   | Code_inuse -> write_string f "INUSE"
   | Code_haschildren -> write_string f "HASCHILDREN"
   | Code_serverbug -> write_string f "SERVERBUG"
   | Code_clientbug -> write_string f "CLIENTBUG"
   | Code_other (name, value) ->
     write_string f name;
     (match value with Some v -> write_sp f; write_string f v | None -> ())
   | _ -> write_string f "UNKNOWN");
  write_char f ']';
  write_sp f

let serialize_response f resp =
  match resp with
  | Ok { tag; code; text } ->
    (match tag with
     | Some t -> write_string f t; write_sp f
     | None -> write_string f "* ");
    write_string f "OK ";
    (match code with Some c -> write_response_code f c | None -> ());
    write_string f text;
    write_crlf f

  | No { tag; code; text } ->
    (match tag with
     | Some t -> write_string f t; write_sp f
     | None -> write_string f "* ");
    write_string f "NO ";
    (match code with Some c -> write_response_code f c | None -> ());
    write_string f text;
    write_crlf f

  | Bad { tag; code; text } ->
    (match tag with
     | Some t -> write_string f t; write_sp f
     | None -> write_string f "* ");
    write_string f "BAD ";
    (match code with Some c -> write_response_code f c | None -> ());
    write_string f text;
    write_crlf f

  | Preauth { code; text } ->
    write_string f "* PREAUTH ";
    (match code with Some c -> write_response_code f c | None -> ());
    write_string f text;
    write_crlf f

  | Bye { code; text } ->
    write_string f "* BYE ";
    (match code with Some c -> write_response_code f c | None -> ());
    write_string f text;
    write_crlf f

  | Capability_response caps ->
    write_string f "* CAPABILITY";
    List.iter (fun c -> write_sp f; write_string f c) caps;
    write_crlf f

  | Enabled caps ->
    write_string f "* ENABLED";
    List.iter (fun c -> write_sp f; write_string f c) caps;
    write_crlf f

  | List_response { flags; delimiter; name } ->
    write_string f "* LIST (";
    List.iteri (fun i flag ->
      if i > 0 then write_sp f;
      match flag with
      | List_noinferiors -> write_string f "\\Noinferiors"
      | List_noselect -> write_string f "\\Noselect"
      | List_marked -> write_string f "\\Marked"
      | List_unmarked -> write_string f "\\Unmarked"
      | List_subscribed -> write_string f "\\Subscribed"
      | List_haschildren -> write_string f "\\HasChildren"
      | List_hasnochildren -> write_string f "\\HasNoChildren"
      | List_all -> write_string f "\\All"
      | List_archive -> write_string f "\\Archive"
      | List_drafts -> write_string f "\\Drafts"
      | List_flagged -> write_string f "\\Flagged"
      | List_junk -> write_string f "\\Junk"
      | List_sent -> write_string f "\\Sent"
      | List_trash -> write_string f "\\Trash"
      | List_extension s -> write_string f s
    ) flags;
    write_string f ") ";
    (match delimiter with
     | Some d -> write_quoted_string f (String.make 1 d)
     | None -> write_string f "NIL");
    write_sp f;
    write_quoted_string f name;
    write_crlf f

  | Namespace_response { personal; other; shared } ->
    let write_namespace ns =
      match ns with
      | None -> write_string f "NIL"
      | Some entries ->
        write_char f '(';
        List.iteri (fun i entry ->
          if i > 0 then write_sp f;
          write_char f '(';
          write_quoted_string f entry.prefix;
          write_sp f;
          (match entry.delimiter with
           | Some d -> write_quoted_string f (String.make 1 d)
           | None -> write_string f "NIL");
          write_char f ')'
        ) entries;
        write_char f ')'
    in
    write_string f "* NAMESPACE ";
    write_namespace personal;
    write_sp f;
    write_namespace other;
    write_sp f;
    write_namespace shared;
    write_crlf f

  | Status_response { mailbox; items } ->
    write_string f "* STATUS ";
    write_quoted_string f mailbox;
    write_string f " (";
    List.iteri (fun i (item, value) ->
      if i > 0 then write_sp f;
      (match item with
       | Status_messages -> write_string f "MESSAGES"
       | Status_uidnext -> write_string f "UIDNEXT"
       | Status_uidvalidity -> write_string f "UIDVALIDITY"
       | Status_unseen -> write_string f "UNSEEN"
       | Status_deleted -> write_string f "DELETED"
       | Status_size -> write_string f "SIZE");
      write_sp f;
      write_string f (Int64.to_string value)
    ) items;
    write_char f ')';
    write_crlf f

  | Esearch { tag = _; uid; results } ->
    write_string f "* ESEARCH";
    if uid then write_string f " UID";
    List.iter (fun r ->
      write_sp f;
      match r with
      | Esearch_min n -> write_string f "MIN "; write_string f (string_of_int n)
      | Esearch_max n -> write_string f "MAX "; write_string f (string_of_int n)
      | Esearch_count n -> write_string f "COUNT "; write_string f (string_of_int n)
      | Esearch_all _ -> write_string f "ALL ..."
    ) results;
    write_crlf f

  | Flags_response flags ->
    write_string f "* FLAGS ";
    write_flag_list f flags;
    write_crlf f

  | Exists n ->
    write_string f "* ";
    write_string f (string_of_int n);
    write_string f " EXISTS";
    write_crlf f

  | Expunge_response n ->
    write_string f "* ";
    write_string f (string_of_int n);
    write_string f " EXPUNGE";
    write_crlf f

  | Fetch_response { seq; items } ->
    write_string f "* ";
    write_string f (string_of_int seq);
    write_string f " FETCH (";
    List.iteri (fun i item ->
      if i > 0 then write_sp f;
      match item with
      | Fetch_item_flags flags ->
        write_string f "FLAGS ";
        write_flag_list f flags
      | Fetch_item_uid uid ->
        write_string f "UID ";
        write_string f (Int32.to_string uid)
      | Fetch_item_internaldate date ->
        write_string f "INTERNALDATE ";
        write_quoted_string f date
      | Fetch_item_rfc822_size size ->
        write_string f "RFC822.SIZE ";
        write_string f (Int64.to_string size)
      | Fetch_item_envelope env ->
        write_string f "ENVELOPE ";
        write_envelope f env
      | Fetch_item_body body_struct ->
        write_string f "BODY ";
        write_body f body_struct
      | Fetch_item_bodystructure body_struct ->
        write_string f "BODYSTRUCTURE ";
        write_bodystructure f body_struct
      | Fetch_item_body_section { section; origin; data } ->
        write_string f "BODY[";
        (match section with
         | None -> ()
         | Some Section_header -> write_string f "HEADER"
         | Some Section_text -> write_string f "TEXT"
         | Some Section_mime -> write_string f "MIME"
         | Some (Section_header_fields fields) ->
           write_string f "HEADER.FIELDS (";
           write_string f (String.concat " " (List.map String.uppercase_ascii fields));
           write_string f ")"
         | Some (Section_header_fields_not fields) ->
           write_string f "HEADER.FIELDS.NOT (";
           write_string f (String.concat " " (List.map String.uppercase_ascii fields));
           write_string f ")"
         | Some (Section_part (parts, subsection)) ->
           write_string f (String.concat "." (List.map string_of_int parts));
           (match subsection with
            | None -> ()
            | Some Section_header -> write_string f ".HEADER"
            | Some Section_text -> write_string f ".TEXT"
            | Some Section_mime -> write_string f ".MIME"
            | Some (Section_header_fields fields) ->
              write_string f ".HEADER.FIELDS (";
              write_string f (String.concat " " (List.map String.uppercase_ascii fields));
              write_string f ")"
            | Some (Section_header_fields_not fields) ->
              write_string f ".HEADER.FIELDS.NOT (";
              write_string f (String.concat " " (List.map String.uppercase_ascii fields));
              write_string f ")"
            | Some (Section_part _) -> () (* Nested part not supported *)
           )
        );
        write_string f "] ";
        (match origin with Some o -> write_string f ("<" ^ string_of_int o ^ "> ") | None -> ());
        (match data with Some d -> write_literal f d | None -> write_string f "NIL")
      | Fetch_item_binary _ ->
        write_string f "BINARY NIL"
      | Fetch_item_binary_size { section = _; size } ->
        write_string f "BINARY.SIZE ";
        write_string f (Int64.to_string size)
    ) items;
    write_char f ')';
    write_crlf f

  | Continuation text ->
    write_string f "+ ";
    (match text with Some t -> write_string f t | None -> ());
    write_crlf f

  | Id_response params ->
    write_string f "* ID ";
    (match params with
     | None -> write_string f "NIL"
     | Some pairs ->
       write_char f '(';
       let first = ref true in
       List.iter (fun (key, value) ->
         if not !first then write_sp f;
         first := false;
         write_quoted_string f key;
         write_sp f;
         write_quoted_string f value
       ) pairs;
       write_char f ')');
    write_crlf f

let response_to_string resp =
  let f = Faraday.create 256 in
  serialize_response f resp;
  Faraday.serialize_to_string f
