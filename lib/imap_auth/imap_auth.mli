(** IMAP Authentication Module

    This module provides authentication backends for the IMAP server.

    Implements {{:https://datatracker.ietf.org/doc/html/rfc9051#section-6.2.3}RFC 9051 Section 6.2.3}
    LOGIN command authentication.

    {2 References}
    {ul
    {- {{:https://datatracker.ietf.org/doc/html/rfc9051}RFC 9051} - IMAP4rev2}
    {- {{:https://datatracker.ietf.org/doc/html/rfc9051#section-6.2.2}RFC 9051 Section 6.2.2} - AUTHENTICATE command}} *)

(** Authentication backend signature *)
module type AUTH = sig
  type t

  val create : service_name:string -> t
  (** [create ~service_name] creates an authenticator using the given service name.
      For PAM, this corresponds to the PAM service configuration (e.g., "imapd"). *)

  val authenticate : t -> username:string -> password:string -> bool
  (** [authenticate t ~username ~password] returns [true] if authentication succeeds. *)
end

(** PAM-based authentication using system accounts.

    Uses Linux-PAM to authenticate against system users.
    Requires a PAM service configuration file (e.g., /etc/pam.d/imapd). *)
module Pam_auth : sig
  include AUTH

  val is_available : unit -> bool
  (** [is_available ()] returns [true] if PAM support is compiled in. *)
end

(** Mock authenticator for testing.

    Stores credentials in memory. Useful for unit tests. *)
module Mock_auth : sig
  include AUTH

  val add_user : t -> username:string -> password:string -> unit
  (** [add_user t ~username ~password] adds a user with the given credentials. *)

  val remove_user : t -> username:string -> unit
  (** [remove_user t ~username] removes a user. *)
end
