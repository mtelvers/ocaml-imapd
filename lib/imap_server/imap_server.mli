(** IMAP4rev2 Server

    This module implements the IMAP server connection handler and state machine
    as specified in {{:https://datatracker.ietf.org/doc/html/rfc9051}RFC 9051}.

    {2 References}
    {ul
    {- {{:https://datatracker.ietf.org/doc/html/rfc9051}RFC 9051} - IMAP4rev2}
    {- {{:https://datatracker.ietf.org/doc/html/rfc9051#section-3}RFC 9051 Section 3} - State and Flow Diagram}
    {- {{:https://datatracker.ietf.org/doc/html/rfc8314}RFC 8314} - Use of TLS for Email}} *)

(** {1 Server Configuration} *)

type config = {
  hostname : string;
  (** Server hostname for greeting. *)

  capabilities : string list;
  (** Additional capabilities to advertise. *)

  greeting : string option;
  (** Custom greeting message. *)

  autologout_timeout : float;
  (** Inactivity timeout in seconds. Default 1800 (30 minutes) per RFC 9051. *)

  tls_config : Tls.Config.server option;
  (** TLS configuration for STARTTLS support. If provided, STARTTLS capability
      is advertised and clients can upgrade to TLS mid-connection. *)
}

val default_config : config
(** Default server configuration. *)

(** {1 Server Functor}

    Create a server instance with a specific storage backend. *)

module Make
    (Storage : Imap_storage.STORAGE)
    (Auth : Imap_auth.AUTH) : sig

  type t
  (** Server instance. *)

  val create : config:config -> storage:Storage.t -> auth:Auth.t -> t
  (** Create a new server instance. *)

  (** {2 Connection Handling} *)

  val handle_connection :
    t ->
    [> `Close | `Flow | `R | `Shutdown | `W ] Eio.Resource.t ->
    _ ->
    unit
  (** Handle a single client connection.

      This implements the IMAP state machine:
      - Sends greeting
      - Processes commands
      - Manages state transitions
      - Handles logout/disconnect *)

  (** {2 Running the Server} *)

  val run :
    t ->
    sw:Eio.Switch.t ->
    net:'a Eio.Net.t ->
    addr:Eio.Net.Sockaddr.stream ->
    ?after_bind:(unit -> unit) ->
    unit ->
    unit
  (** Run the server on a cleartext port (143).

      @param after_bind Optional callback invoked after binding but before
             accepting connections. Use for privilege dropping.

      Implements {{:https://datatracker.ietf.org/doc/html/rfc9051#section-2.1}RFC 9051 Section 2.1}. *)

  val run_tls :
    t ->
    sw:Eio.Switch.t ->
    net:'a Eio.Net.t ->
    addr:Eio.Net.Sockaddr.stream ->
    tls_config:Tls.Config.server ->
    ?after_bind:(unit -> unit) ->
    unit ->
    unit
  (** Run the server on an implicit TLS port (993).

      @param after_bind Optional callback invoked after binding but before
             accepting connections. Use for privilege dropping.

      Implements {{:https://datatracker.ietf.org/doc/html/rfc8314#section-3.2}RFC 8314 Section 3.2}. *)

  val run_forked :
    t ->
    sw:Eio.Switch.t ->
    net:'a Eio.Net.t ->
    addr:Eio.Net.Sockaddr.stream ->
    tls_config:Tls.Config.server option ->
    unit
  (** Run the server with fork-per-connection privilege separation.

      Each incoming connection forks a child process. After successful
      authentication, the child drops privileges to the authenticated user
      via setuid/setgid. This provides strong isolation between users.

      Requires running as root. STARTTLS is not supported in this mode
      (use implicit TLS instead).

      This is the recommended mode for production deployments. *)
end

