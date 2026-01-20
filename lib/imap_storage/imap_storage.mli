(** IMAP Storage Backends

    This module provides pluggable storage backends for the IMAP server.

    {2 References}
    {ul
    {- {{:https://datatracker.ietf.org/doc/html/rfc9051}RFC 9051} - IMAP4rev2}
    {- {{:https://datatracker.ietf.org/doc/html/rfc9051#section-2.3}RFC 9051 Section 2.3} - Message Attributes}} *)

open Imap_types

(** {1 Storage Errors} *)

type error =
  | Mailbox_not_found
  | Mailbox_already_exists
  | Message_not_found
  | Permission_denied
  | Storage_error of string
  | Quota_exceeded

val error_to_string : error -> string

(** {1 Mailbox Information} *)

type mailbox_info = {
  name : mailbox_name;
  delimiter : char option;
  flags : list_flag list;
}

(** {1 Storage Backend Signature} *)

module type STORAGE = sig
  type t

  (** {2 Lifecycle} *)

  val create : unit -> t
  (** Create a new storage instance. *)

  (** {2 Mailbox Operations} *)

  val list_mailboxes :
    t ->
    username:string ->
    reference:string ->
    pattern:string ->
    mailbox_info list
  (** List mailboxes matching the pattern.
      See {{:https://datatracker.ietf.org/doc/html/rfc9051#section-6.3.9}RFC 9051 Section 6.3.9}. *)

  val create_mailbox :
    t ->
    username:string ->
    mailbox_name ->
    (unit, error) result
  (** Create a new mailbox.
      See {{:https://datatracker.ietf.org/doc/html/rfc9051#section-6.3.4}RFC 9051 Section 6.3.4}. *)

  val delete_mailbox :
    t ->
    username:string ->
    mailbox_name ->
    (unit, error) result
  (** Delete a mailbox.
      See {{:https://datatracker.ietf.org/doc/html/rfc9051#section-6.3.5}RFC 9051 Section 6.3.5}. *)

  val rename_mailbox :
    t ->
    username:string ->
    old_name:mailbox_name ->
    new_name:mailbox_name ->
    (unit, error) result
  (** Rename a mailbox.
      See {{:https://datatracker.ietf.org/doc/html/rfc9051#section-6.3.6}RFC 9051 Section 6.3.6}. *)

  val select_mailbox :
    t ->
    username:string ->
    mailbox_name ->
    readonly:bool ->
    (mailbox_state, error) result
  (** Select a mailbox for access.
      See {{:https://datatracker.ietf.org/doc/html/rfc9051#section-6.3.2}RFC 9051 Section 6.3.2}. *)

  val status_mailbox :
    t ->
    username:string ->
    mailbox_name ->
    items:status_item list ->
    ((status_item * int64) list, error) result
  (** Get mailbox status.
      See {{:https://datatracker.ietf.org/doc/html/rfc9051#section-6.3.11}RFC 9051 Section 6.3.11}. *)

  (** {2 Message Operations} *)

  val fetch_messages :
    t ->
    username:string ->
    mailbox:mailbox_name ->
    sequence:sequence_set ->
    items:fetch_item list ->
    (message list, error) result
  (** Fetch messages.
      See {{:https://datatracker.ietf.org/doc/html/rfc9051#section-6.4.5}RFC 9051 Section 6.4.5}. *)

  val fetch_by_uid :
    t ->
    username:string ->
    mailbox:mailbox_name ->
    uids:sequence_set ->
    items:fetch_item list ->
    (message list, error) result
  (** Fetch messages by UID. *)

  val store_flags :
    t ->
    username:string ->
    mailbox:mailbox_name ->
    sequence:sequence_set ->
    action:store_action ->
    flags:flag list ->
    (message list, error) result
  (** Store flags on messages by sequence number.
      See {{:https://datatracker.ietf.org/doc/html/rfc9051#section-6.4.6}RFC 9051 Section 6.4.6}. *)

  val store_by_uid :
    t ->
    username:string ->
    mailbox:mailbox_name ->
    uids:sequence_set ->
    action:store_action ->
    flags:flag list ->
    (message list, error) result
  (** Store flags on messages by UID. *)

  val expunge :
    t ->
    username:string ->
    mailbox:mailbox_name ->
    (uid list, error) result
  (** Expunge deleted messages.
      See {{:https://datatracker.ietf.org/doc/html/rfc9051#section-6.4.3}RFC 9051 Section 6.4.3}. *)

  val append :
    t ->
    username:string ->
    mailbox:mailbox_name ->
    flags:flag list ->
    date:string option ->
    message:string ->
    (uid, error) result
  (** Append a message.
      See {{:https://datatracker.ietf.org/doc/html/rfc9051#section-6.3.12}RFC 9051 Section 6.3.12}. *)

  val copy :
    t ->
    username:string ->
    src_mailbox:mailbox_name ->
    sequence:sequence_set ->
    dst_mailbox:mailbox_name ->
    (uid list, error) result
  (** Copy messages.
      See {{:https://datatracker.ietf.org/doc/html/rfc9051#section-6.4.7}RFC 9051 Section 6.4.7}. *)

  val move :
    t ->
    username:string ->
    src_mailbox:mailbox_name ->
    sequence:sequence_set ->
    dst_mailbox:mailbox_name ->
    (uid list, error) result
  (** Move messages.
      See {{:https://datatracker.ietf.org/doc/html/rfc9051#section-6.4.8}RFC 9051 Section 6.4.8}. *)

  val search :
    t ->
    username:string ->
    mailbox:mailbox_name ->
    criteria:search_key ->
    (uid list, error) result
  (** Search messages.
      See {{:https://datatracker.ietf.org/doc/html/rfc9051#section-6.4.4}RFC 9051 Section 6.4.4}. *)
end

(** {1 In-Memory Storage}

    Simple in-memory storage for development and testing. *)

module Memory_storage : sig
  include STORAGE

  val add_test_user : t -> username:string -> unit
  (** Add a test user with default INBOX. *)

  val add_test_message : t -> username:string -> mailbox:mailbox_name -> message:message -> unit
  (** Add a test message directly. *)
end

(** {1 Maildir Storage}

    Maildir-based storage for production use.
    See {{:https://cr.yp.to/proto/maildir.html}Maildir specification}. *)

module Maildir_storage : sig
  include STORAGE

  val create_with_path : base_path:string -> t
  (** Create storage with a specific base path for Maildir directories.
      Mail is stored at {i base_path}/{i username}/. *)

  val create_home_directory : unit -> t
  (** Create storage using users' home directories.
      Mail is stored at ~{i username}/Maildir/ (the traditional Unix location).
      This is the recommended mode for fork-per-connection with setuid. *)

  val ensure_user : t -> username:string -> unit
  (** Ensure user's INBOX exists (creates Maildir structure). *)
end
