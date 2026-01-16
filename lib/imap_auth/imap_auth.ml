(** IMAP Authentication Module

    Implements {{:https://datatracker.ietf.org/doc/html/rfc9051#section-6.2.3}RFC 9051 Section 6.2.3}
    LOGIN command authentication. *)

module type AUTH = sig
  type t

  val create : service_name:string -> t
  val authenticate : t -> username:string -> password:string -> bool
end

(* External C functions for PAM *)
external pam_authenticate_ext : string -> string -> string -> bool = "caml_pam_authenticate"
external pam_available_ext : unit -> bool = "caml_pam_available"

module Pam_auth = struct
  type t = {
    service_name : string;
  }

  let create ~service_name = { service_name }

  let authenticate t ~username ~password =
    pam_authenticate_ext t.service_name username password

  let is_available () = pam_available_ext ()
end

module Mock_auth = struct
  type t = {
    mutable users : (string * string) list;
    service_name : string;
  }

  let create ~service_name = { users = []; service_name }

  let add_user t ~username ~password =
    t.users <- (username, password) :: List.filter (fun (u, _) -> u <> username) t.users

  let remove_user t ~username =
    t.users <- List.filter (fun (u, _) -> u <> username) t.users

  let authenticate t ~username ~password =
    List.exists (fun (u, p) -> u = username && p = password) t.users
end
