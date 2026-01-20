(** IMAP4rev2 Parser

    Implements {{:https://datatracker.ietf.org/doc/html/rfc9051#section-9}RFC 9051 Section 9} Formal Syntax.

    This module uses Menhir for parsing and Faraday for response serialization. *)

open Imap_types

(** {1 Type Re-exports}

    Types are defined in {!Imap_types} and re-exported here for convenience. *)

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
  | Recent of int
  | Expunge_response of int
  | Fetch_response of { seq : int; items : fetch_response_item list }
  | Continuation of string option
  | Id_response of (string * string) list option

(** {1 Parsing} *)

val parse_command : string -> (tagged_command, string) result
(** Parse a complete IMAP command line. *)

(** {1 Serialization} *)

val serialize_response : Faraday.t -> response -> unit
(** Serialize a response to a Faraday buffer. *)

val response_to_string : response -> string
(** Convert response to string. *)

(** {1 Utilities} *)

val crlf : string
(** CRLF line ending. *)
