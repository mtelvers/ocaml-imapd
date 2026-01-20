(** IMAP Envelope Parser - RFC 5322 Header Parsing

    Parses email headers to build IMAP ENVELOPE structure.

    References:
    - {{:https://datatracker.ietf.org/doc/html/rfc5322}RFC 5322} - Internet Message Format
    - {{:https://datatracker.ietf.org/doc/html/rfc9051#section-2.3.5}RFC 9051 Section 2.3.5} - ENVELOPE *)

open Imap_types

(** {1 Header Parsing} *)

(** Split raw message into headers and body strings.
    Headers end at the first blank line. *)
val split_headers_body : string -> string * string

(** Parse headers string into (name, value) pairs.
    Header names are stored lowercase. *)
val parse_header_lines : string -> (string * string) list

(** Get header value by name (case-insensitive). *)
val get_header : (string * string) list -> string -> string option

(** {1 Address Parsing} *)

(** Parse a single email address string into an address record.
    Handles formats:
    - user@domain
    - <user@domain>
    - Display Name <user@domain>
    - "Quoted Name" <user@domain> *)
val parse_single_address : string -> address option

(** Parse an address list header (comma-separated addresses).
    Also handles group syntax: group-name: addr1, addr2; *)
val parse_address_list : string -> address list

(** {1 RFC 2047 Decoding} *)

(** Decode RFC 2047 encoded words in header values.
    Handles =?charset?B?base64?= and =?charset?Q?quoted-printable?= *)
val decode_encoded_word : string -> string

(** {1 Envelope Construction} *)

(** Build envelope from parsed headers. *)
val envelope_from_headers : (string * string) list -> envelope

(** Parse raw message and extract envelope. *)
val parse_envelope : string -> envelope

(** Parse raw message and return (headers_string, body_string). *)
val parse_message : string -> string * string

(** {1 Body Structure Parsing} *)

(** Parse raw message and extract MIME body structure.
    Handles multipart messages recursively. *)
val parse_body_structure : string -> body_structure
