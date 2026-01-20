(** IMAP4rev2 Core Types

    This module defines the core types for the IMAP4rev2 protocol as specified in
    {{:https://datatracker.ietf.org/doc/html/rfc9051}RFC 9051}.

    {2 References}
    {ul
    {- {{:https://datatracker.ietf.org/doc/html/rfc9051}RFC 9051} - IMAP4rev2}
    {- {{:https://datatracker.ietf.org/doc/html/rfc9051#section-2.3}RFC 9051 Section 2.3} - Message Attributes}
    {- {{:https://datatracker.ietf.org/doc/html/rfc9051#section-3}RFC 9051 Section 3} - State and Flow Diagram}} *)

(** {1 Basic Types} *)

type mailbox_name = string
(** Mailbox name. INBOX is case-insensitive per {{:https://datatracker.ietf.org/doc/html/rfc9051#section-5.1}RFC 9051 Section 5.1}. *)

type uid = int32
(** Unique identifier for a message.
    See {{:https://datatracker.ietf.org/doc/html/rfc9051#section-2.3.1.1}RFC 9051 Section 2.3.1.1}. *)

type seq_num = int
(** Message sequence number (1-based).
    See {{:https://datatracker.ietf.org/doc/html/rfc9051#section-2.3.1.2}RFC 9051 Section 2.3.1.2}. *)

type uidvalidity = int32
(** UIDVALIDITY value for a mailbox.
    See {{:https://datatracker.ietf.org/doc/html/rfc9051#section-2.3.1.1}RFC 9051 Section 2.3.1.1}. *)

(** {1 Message Flags}

    See {{:https://datatracker.ietf.org/doc/html/rfc9051#section-2.3.2}RFC 9051 Section 2.3.2}. *)

type system_flag =
  | Seen      (** Message has been read *)
  | Answered  (** Message has been answered *)
  | Flagged   (** Message is flagged for urgent/special attention *)
  | Deleted   (** Message is marked for deletion *)
  | Draft     (** Message has not completed composition *)

type flag =
  | System of system_flag
  | Keyword of string  (** User-defined keyword (e.g., "$Forwarded", "$Junk") *)

(** {1 Email Addresses}

    See {{:https://datatracker.ietf.org/doc/html/rfc9051#section-7.5.2}RFC 9051 Section 7.5.2} ENVELOPE structure. *)

type address = {
  name : string option;      (** Display name *)
  adl : string option;       (** Source route (obsolete, usually NIL) *)
  mailbox : string option;   (** Local part of email address *)
  host : string option;      (** Domain part of email address *)
}

(** {1 Message Envelope}

    Parsed representation of RFC 5322 header.
    See {{:https://datatracker.ietf.org/doc/html/rfc9051#section-2.3.5}RFC 9051 Section 2.3.5}. *)

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

(** {1 Body Structure}

    See {{:https://datatracker.ietf.org/doc/html/rfc9051#section-2.3.6}RFC 9051 Section 2.3.6}. *)

type body_fields = {
  params : (string * string) list;  (** Content-Type parameters *)
  content_id : string option;
  description : string option;
  encoding : string;                (** Content-Transfer-Encoding *)
  size : int64;                     (** Size in octets *)
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

(** {1 Sequence Sets}

    See {{:https://datatracker.ietf.org/doc/html/rfc9051#section-4.1.1}RFC 9051 Section 4.1.1}. *)

type sequence_range =
  | Single of int           (** Single message number *)
  | Range of int * int      (** Range n:m *)
  | From of int             (** n:* (from n to end) *)
  | All                     (** * (all messages) *)

type sequence_set = sequence_range list

(** {1 Section Specification}

    For BODY[...] fetch items.
    See {{:https://datatracker.ietf.org/doc/html/rfc9051#section-6.4.5}RFC 9051 Section 6.4.5}. *)

type section_spec =
  | Section_header
  | Section_header_fields of string list
  | Section_header_fields_not of string list
  | Section_text
  | Section_mime
  | Section_part of int list * section_spec option

type body_section = {
  section : section_spec option;
  partial : (int * int) option;  (** <offset.length> *)
}

(** {1 FETCH Items}

    See {{:https://datatracker.ietf.org/doc/html/rfc9051#section-6.4.5}RFC 9051 Section 6.4.5}. *)

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
  | Fetch_body_section of string * (int * int) option  (** section string, partial *)
  | Fetch_body_peek of string * (int * int) option
  | Fetch_binary of string * (int * int) option
  | Fetch_binary_peek of string * (int * int) option
  | Fetch_binary_size of string

(** {1 SEARCH Criteria}

    See {{:https://datatracker.ietf.org/doc/html/rfc9051#section-6.4.4}RFC 9051 Section 6.4.4}. *)

type search_key =
  | Search_all
  | Search_answered
  | Search_bcc of string
  | Search_before of string  (** date *)
  | Search_body of string
  | Search_cc of string
  | Search_deleted
  | Search_flagged
  | Search_from of string
  | Search_keyword of string
  | Search_new
  | Search_not of search_key
  | Search_old
  | Search_on of string  (** date *)
  | Search_or of search_key * search_key
  | Search_seen
  | Search_since of string  (** date *)
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

(** {1 STORE Actions}

    See {{:https://datatracker.ietf.org/doc/html/rfc9051#section-6.4.6}RFC 9051 Section 6.4.6}. *)

type store_action =
  | Store_set     (** FLAGS - replace flags *)
  | Store_add     (** +FLAGS - add flags *)
  | Store_remove  (** -FLAGS - remove flags *)

type store_silent = bool  (** .SILENT modifier *)

(** {1 STATUS Items}

    See {{:https://datatracker.ietf.org/doc/html/rfc9051#section-6.3.11}RFC 9051 Section 6.3.11}. *)

type status_item =
  | Status_messages
  | Status_uidnext
  | Status_uidvalidity
  | Status_unseen
  | Status_deleted
  | Status_size

(** {1 LIST Flags}

    See {{:https://datatracker.ietf.org/doc/html/rfc9051#section-7.3.1}RFC 9051 Section 7.3.1}. *)

type list_flag =
  | List_noinferiors   (** \Noinferiors *)
  | List_noselect      (** \Noselect *)
  | List_marked        (** \Marked *)
  | List_unmarked      (** \Unmarked *)
  | List_subscribed    (** \Subscribed *)
  | List_haschildren   (** \HasChildren *)
  | List_hasnochildren (** \HasNoChildren *)
  | List_all           (** \All - special-use *)
  | List_archive       (** \Archive *)
  | List_drafts        (** \Drafts *)
  | List_flagged       (** \Flagged *)
  | List_junk          (** \Junk *)
  | List_sent          (** \Sent *)
  | List_trash         (** \Trash *)
  | List_extension of string  (** Other flags *)

(** {1 Connection State}

    See {{:https://datatracker.ietf.org/doc/html/rfc9051#section-3}RFC 9051 Section 3}. *)

type connection_state =
  | Not_authenticated
  | Authenticated of { username : string }
  | Selected of { username : string; mailbox : mailbox_name; readonly : bool }
  | Logout

(** {1 Mailbox State}

    Information about a selected mailbox. *)

type mailbox_state = {
  name : mailbox_name;
  exists : int;                    (** Number of messages *)
  uidvalidity : uidvalidity;
  uidnext : uid;
  flags : flag list;               (** Available flags *)
  permanent_flags : flag list;     (** Flags that can be changed permanently *)
  readonly : bool;
}

(** {1 Message Representation} *)

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

(** {1 Response Codes}

    See {{:https://datatracker.ietf.org/doc/html/rfc9051#section-7.1}RFC 9051 Section 7.1}. *)

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

(** {1 Commands}

    See {{:https://datatracker.ietf.org/doc/html/rfc9051#section-6}RFC 9051 Section 6}. *)

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
  | Id of (string * string) list option  (** RFC 2971 *)

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

(** {1 Responses}

    See {{:https://datatracker.ietf.org/doc/html/rfc9051#section-7}RFC 9051 Section 7}. *)

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
  | Id_response of (string * string) list option  (** RFC 2971 *)

(** {1 Utility Functions} *)

(** {2 Security Validation} *)

val is_safe_username : string -> bool
(** Validate username for path safety. Prevents path traversal attacks.
    Returns false for usernames containing null bytes, path separators,
    or traversal patterns like [..]. *)

val is_safe_mailbox_name : mailbox_name -> bool
(** Validate mailbox name for path safety. Prevents path traversal attacks.
    Returns false for names containing null bytes, backslashes, or
    path components that are [.] or [..]. Allows [/] as hierarchy delimiter. *)

(** {2 Mailbox Utilities} *)

val normalize_mailbox_name : mailbox_name -> mailbox_name
(** Normalize mailbox name. INBOX is case-insensitive. *)

val is_inbox : mailbox_name -> bool
(** Check if mailbox name is INBOX (case-insensitive). *)

val flag_to_string : flag -> string
(** Convert flag to IMAP string representation. *)

val string_to_flag : string -> flag option
(** Parse IMAP flag string. *)

val system_flag_to_string : system_flag -> string
(** Convert system flag to string. *)
