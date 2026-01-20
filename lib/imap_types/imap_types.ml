(** IMAP4rev2 Core Types

    Implements types from {{:https://datatracker.ietf.org/doc/html/rfc9051}RFC 9051}. *)

(* Basic types *)
type mailbox_name = string
type uid = int32
type seq_num = int
type uidvalidity = int32

(* Message flags - RFC 9051 Section 2.3.2 *)
type system_flag =
  | Seen
  | Answered
  | Flagged
  | Deleted
  | Draft

type flag =
  | System of system_flag
  | Keyword of string

(* Email addresses *)
type address = {
  name : string option;
  adl : string option;
  mailbox : string option;
  host : string option;
}

(* Message envelope - RFC 9051 Section 2.3.5 *)
type envelope = {
  date : string option;
  subject : string option;
  from : address list;
  sender : address list;
  reply_to : address list;
  to_ : address list;
  cc : address list;
  bcc : address list;
  in_reply_to : string option;
  message_id : string option;
}

(* Body structure - RFC 9051 Section 2.3.6 *)
type body_fields = {
  params : (string * string) list;
  content_id : string option;
  description : string option;
  encoding : string;
  size : int64;
}

type body_type =
  | Text of {
      subtype : string;
      fields : body_fields;
      lines : int64;
    }
  | Message_rfc822 of {
      fields : body_fields;
      envelope : envelope;
      body : body_structure;
      lines : int64;
    }
  | Basic of {
      media_type : string;
      subtype : string;
      fields : body_fields;
    }
  | Multipart of {
      subtype : string;
      parts : body_structure list;
      params : (string * string) list;
    }

and body_structure = {
  body_type : body_type;
  disposition : (string * (string * string) list) option;
  language : string list option;
  location : string option;
}

(* Sequence sets - RFC 9051 Section 4.1.1 *)
type sequence_range =
  | Single of int
  | Range of int * int
  | From of int
  | All

type sequence_set = sequence_range list

(* Section specification for BODY[...] - RFC 9051 Section 6.4.5 *)
type section_spec =
  | Section_header
  | Section_header_fields of string list
  | Section_header_fields_not of string list
  | Section_text
  | Section_mime
  | Section_part of int list * section_spec option

type body_section = {
  section : section_spec option;
  partial : (int * int) option;
}

(* FETCH items - RFC 9051 Section 6.4.5 *)
type fetch_item =
  | Fetch_envelope
  | Fetch_flags
  | Fetch_internaldate
  | Fetch_rfc822
  | Fetch_rfc822_size
  | Fetch_rfc822_header
  | Fetch_rfc822_text
  | Fetch_uid
  | Fetch_body
  | Fetch_bodystructure
  | Fetch_body_section of string * (int * int) option  (* section string, partial *)
  | Fetch_body_peek of string * (int * int) option
  | Fetch_binary of string * (int * int) option
  | Fetch_binary_peek of string * (int * int) option
  | Fetch_binary_size of string

(* SEARCH criteria - RFC 9051 Section 6.4.4 *)
type search_key =
  | Search_all
  | Search_answered
  | Search_bcc of string
  | Search_before of string
  | Search_body of string
  | Search_cc of string
  | Search_deleted
  | Search_flagged
  | Search_from of string
  | Search_keyword of string
  | Search_new
  | Search_not of search_key
  | Search_old
  | Search_on of string
  | Search_or of search_key * search_key
  | Search_seen
  | Search_since of string
  | Search_subject of string
  | Search_text of string
  | Search_to of string
  | Search_unanswered
  | Search_undeleted
  | Search_unflagged
  | Search_unkeyword of string
  | Search_unseen
  | Search_draft
  | Search_undraft
  | Search_header of string * string
  | Search_larger of int64
  | Search_smaller of int64
  | Search_uid of sequence_set
  | Search_sequence_set of sequence_set
  | Search_and of search_key list
  | Search_sentbefore of string
  | Search_senton of string
  | Search_sentsince of string

(* STORE actions - RFC 9051 Section 6.4.6 *)
type store_action =
  | Store_set
  | Store_add
  | Store_remove

type store_silent = bool

(* STATUS items - RFC 9051 Section 6.3.11 *)
type status_item =
  | Status_messages
  | Status_uidnext
  | Status_uidvalidity
  | Status_unseen
  | Status_deleted
  | Status_size

(* LIST flags - RFC 9051 Section 7.3.1 *)
type list_flag =
  | List_noinferiors
  | List_noselect
  | List_marked
  | List_unmarked
  | List_subscribed
  | List_haschildren
  | List_hasnochildren
  | List_all
  | List_archive
  | List_drafts
  | List_flagged
  | List_junk
  | List_sent
  | List_trash
  | List_extension of string

(* Connection state - RFC 9051 Section 3 *)
type connection_state =
  | Not_authenticated
  | Authenticated of { username : string }
  | Selected of { username : string; mailbox : mailbox_name; readonly : bool }
  | Logout

(* Mailbox state *)
type mailbox_state = {
  name : mailbox_name;
  exists : int;
  uidvalidity : uidvalidity;
  uidnext : uid;
  flags : flag list;
  permanent_flags : flag list;
  readonly : bool;
}

(* Message representation *)
type message = {
  uid : uid;
  seq : seq_num;
  flags : flag list;
  internal_date : string;
  size : int64;
  envelope : envelope option;
  body_structure : body_structure option;
  raw_headers : string option;
  raw_body : string option;
}

(* Response codes - RFC 9051 Section 7.1 *)
type response_code =
  | Code_alert
  | Code_alreadyexists
  | Code_appenduid of uidvalidity * uid
  | Code_authenticationfailed
  | Code_authorizationfailed
  | Code_badcharset of string list
  | Code_cannot
  | Code_capability of string list
  | Code_clientbug
  | Code_closed
  | Code_contactadmin
  | Code_copyuid of uidvalidity * sequence_set * sequence_set
  | Code_corruption
  | Code_expired
  | Code_expungeissued
  | Code_haschildren
  | Code_inuse
  | Code_limit
  | Code_nonexistent
  | Code_noperm
  | Code_overquota
  | Code_parse
  | Code_permanentflags of flag list
  | Code_privacyrequired
  | Code_readonly
  | Code_readwrite
  | Code_serverbug
  | Code_trycreate
  | Code_uidnotsticky
  | Code_uidvalidity of uidvalidity
  | Code_uidnext of uid
  | Code_unavailable
  | Code_unknown_cte
  | Code_other of string * string option

(* Utility functions *)

(** Validate a username for path safety.
    Prevents path traversal attacks by rejecting dangerous characters. *)
let is_safe_username username =
  let len = String.length username in
  if len = 0 || len > 256 then false
  else
    (* Reject null bytes, path separators, and path traversal *)
    not (String.contains username '\x00') &&
    not (String.contains username '/') &&
    not (String.contains username '\\') &&
    username <> "." &&
    username <> ".." &&
    (* Reject leading/trailing dots and spaces *)
    username.[0] <> '.' &&
    username.[len - 1] <> '.' &&
    username.[0] <> ' ' &&
    username.[len - 1] <> ' '

(** Validate a mailbox name for path safety.
    Prevents path traversal attacks. Allows '/' as hierarchy delimiter. *)
let is_safe_mailbox_name name =
  let len = String.length name in
  if len = 0 || len > 1024 then false
  else
    (* Reject null bytes and backslashes *)
    not (String.contains name '\x00') &&
    not (String.contains name '\\') &&
    (* Reject path components that are . or .. *)
    let parts = String.split_on_char '/' name in
    not (List.exists (fun p -> p = "." || p = "..") parts)

let normalize_mailbox_name name =
  if String.uppercase_ascii name = "INBOX" then "INBOX"
  else name

let is_inbox name =
  String.uppercase_ascii name = "INBOX"

let system_flag_to_string = function
  | Seen -> "\\Seen"
  | Answered -> "\\Answered"
  | Flagged -> "\\Flagged"
  | Deleted -> "\\Deleted"
  | Draft -> "\\Draft"

let flag_to_string = function
  | System sf -> system_flag_to_string sf
  | Keyword kw -> kw

let string_to_flag s =
  let s_upper = String.uppercase_ascii s in
  match s_upper with
  | "\\SEEN" -> Some (System Seen)
  | "\\ANSWERED" -> Some (System Answered)
  | "\\FLAGGED" -> Some (System Flagged)
  | "\\DELETED" -> Some (System Deleted)
  | "\\DRAFT" -> Some (System Draft)
  | _ ->
    if String.length s > 0 && s.[0] <> '\\' then
      Some (Keyword s)
    else
      None

(* === Commands - RFC 9051 Section 6 === *)

type command =
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
  | Lsub of { reference : string; pattern : string }
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
  | Id of (string * string) list option  (** RFC 2971 - NIL or list of field/value pairs *)

and uid_command =
  | Uid_fetch of { sequence : sequence_set; items : fetch_item list }
  | Uid_store of { sequence : sequence_set; silent : bool; action : store_action; flags : flag list }
  | Uid_copy of { sequence : sequence_set; mailbox : mailbox_name }
  | Uid_move of { sequence : sequence_set; mailbox : mailbox_name }
  | Uid_search of { charset : string option; criteria : search_key }
  | Uid_expunge of sequence_set

type tagged_command = {
  tag : string;
  command : command;
}

(* === Responses - RFC 9051 Section 7 === *)

type namespace_entry = {
  prefix : string;
  delimiter : char option;
}

type namespace_data = {
  personal : namespace_entry list option;
  other : namespace_entry list option;
  shared : namespace_entry list option;
}

type esearch_result =
  | Esearch_min of int
  | Esearch_max of int
  | Esearch_count of int
  | Esearch_all of sequence_set

type fetch_response_item =
  | Fetch_item_envelope of envelope
  | Fetch_item_flags of flag list
  | Fetch_item_internaldate of string
  | Fetch_item_rfc822_size of int64
  | Fetch_item_uid of uid
  | Fetch_item_body of body_structure
  | Fetch_item_bodystructure of body_structure
  | Fetch_item_body_section of { section : section_spec option; origin : int option; data : string option }
  | Fetch_item_binary of { section : int list; data : string option }
  | Fetch_item_binary_size of { section : int list; size : int64 }

type response =
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
  | Recent of int
  | Expunge_response of int
  | Fetch_response of { seq : int; items : fetch_response_item list }
  | Continuation of string option
  | Id_response of (string * string) list option  (** RFC 2971 - NIL or list of field/value pairs *)
