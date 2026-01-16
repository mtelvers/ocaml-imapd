(** IMAP4rev2 Server Entry Point

    Implements {{:https://datatracker.ietf.org/doc/html/rfc9051}RFC 9051} IMAP4rev2 server.

    Default ports:
    - 143: Cleartext (STARTTLS available)
    - 993: Implicit TLS per {{:https://datatracker.ietf.org/doc/html/rfc8314}RFC 8314} *)

open Cmdliner

(* Storage backend type *)
type storage_backend =
  | Memory
  | Maildir of string option  (* None = use ~/Maildir, Some path = use shared base *)

(* Load TLS configuration from certificate and key files *)
let load_tls_config ~cert_file ~key_file =
  let cert_pem = In_channel.with_open_bin cert_file In_channel.input_all in
  let key_pem = In_channel.with_open_bin key_file In_channel.input_all in
  let certs = X509.Certificate.decode_pem_multiple cert_pem in
  let key = X509.Private_key.decode_pem key_pem in
  match certs, key with
  | Ok certs, Ok key ->
    let cert = `Single (certs, key) in
    (match Tls.Config.server ~certificates:cert () with
     | Ok config -> Some config
     | Error _ -> None)
  | _ -> None

(* Parse IP address *)
let parse_ipaddr host =
  match host with
  | "127.0.0.1" | "localhost" -> Eio.Net.Ipaddr.V4.loopback
  | "0.0.0.0" -> Eio.Net.Ipaddr.V4.any
  | _ ->
    match String.split_on_char '.' host with
    | [a; b; c; d] ->
      let bytes = Bytes.create 4 in
      Bytes.set bytes 0 (Char.chr (int_of_string a));
      Bytes.set bytes 1 (Char.chr (int_of_string b));
      Bytes.set bytes 2 (Char.chr (int_of_string c));
      Bytes.set bytes 3 (Char.chr (int_of_string d));
      Eio.Net.Ipaddr.of_raw (Bytes.to_string bytes)
    | _ -> Eio.Net.Ipaddr.V4.loopback

(* Run the server with memory storage - single process mode *)
let run_with_memory_single ~port ~host ~tls_config ~implicit_tls =
  let module Server = Imap_server.Make(Imap_storage.Memory_storage)(Imap_auth.Pam_auth) in
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  let storage = Imap_storage.Memory_storage.create () in
  let auth = Imap_auth.Pam_auth.create ~service_name:"imapd" in

  (* Add test user for development *)
  Imap_storage.Memory_storage.add_test_user storage ~username:"test";

  let config = {
    Imap_server.default_config with
    hostname = host;
    tls_config;
  } in
  let server = Server.create ~config ~storage ~auth in

  let tls_mode = if implicit_tls then " (implicit TLS)" else if tls_config <> None then " (STARTTLS available)" else "" in
  Eio.traceln "IMAP server starting on %s:%d (memory storage, single-process)%s" host port tls_mode;

  Eio.Switch.run @@ fun sw ->
  let ipaddr = parse_ipaddr host in
  let addr = `Tcp (ipaddr, port) in
  if implicit_tls then
    match tls_config with
    | Some tls -> Server.run_tls server ~sw ~net ~addr ~tls_config:tls ()
    | None -> failwith "TLS config required for implicit TLS"
  else
    Server.run server ~sw ~net ~addr ()

(* Run the server with Maildir storage - single process mode *)
let run_with_maildir_single ~port ~host ~tls_config ~maildir_path ~implicit_tls =
  let module Server = Imap_server.Make(Imap_storage.Maildir_storage)(Imap_auth.Pam_auth) in
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  let storage = Imap_storage.Maildir_storage.create_with_path ~base_path:maildir_path in
  let auth = Imap_auth.Pam_auth.create ~service_name:"imapd" in

  let config = {
    Imap_server.default_config with
    hostname = host;
    tls_config;
  } in
  let server = Server.create ~config ~storage ~auth in

  let tls_mode = if implicit_tls then " (implicit TLS)" else if tls_config <> None then " (STARTTLS available)" else "" in
  Eio.traceln "IMAP server starting on %s:%d (maildir: %s, single-process)%s" host port maildir_path tls_mode;

  Eio.Switch.run @@ fun sw ->
  let ipaddr = parse_ipaddr host in
  let addr = `Tcp (ipaddr, port) in
  if implicit_tls then
    match tls_config with
    | Some tls -> Server.run_tls server ~sw ~net ~addr ~tls_config:tls ()
    | None -> failwith "TLS config required for implicit TLS"
  else
    Server.run server ~sw ~net ~addr ()

(* Run the server with Maildir storage - forked mode with per-user privileges *)
let run_with_maildir_forked ~port ~host ~tls_config ~maildir_path =
  let module Server = Imap_server.Make(Imap_storage.Maildir_storage)(Imap_auth.Pam_auth) in
  let storage = match maildir_path with
    | Some path -> Imap_storage.Maildir_storage.create_with_path ~base_path:path
    | None -> Imap_storage.Maildir_storage.create_home_directory ()
  in
  let auth = Imap_auth.Pam_auth.create ~service_name:"imapd" in

  let config = {
    Imap_server.default_config with
    hostname = host;
    tls_config;
  } in
  let server = Server.create ~config ~storage ~auth in

  let storage_desc = match maildir_path with
    | Some path -> Printf.sprintf "maildir: %s" path
    | None -> "~/Maildir"
  in
  let tls_mode = if tls_config <> None then " (implicit TLS)" else "" in
  Printf.eprintf "+IMAP server starting on %s:%d (%s, fork-per-connection)%s\n%!" host port storage_desc tls_mode;

  (* run_forked uses its own accept loop, not EIO's *)
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let ipaddr = parse_ipaddr host in
  let addr = `Tcp (ipaddr, port) in
  let net = Eio.Stdenv.net _env in
  Server.run_forked server ~sw ~net ~addr ~tls_config

(* Main entry point *)
let run port host cert_file key_file backend maildir_path implicit_tls forked =
  (* Initialize cryptographic RNG for TLS *)
  Mirage_crypto_rng_unix.use_default ();

  (* Forked mode requires implicit TLS (STARTTLS not supported) *)
  if forked && not implicit_tls && (cert_file <> None || key_file <> None) then begin
    Printf.eprintf "Warning: STARTTLS not supported in forked mode. Use --tls for implicit TLS.\n%!";
  end;

  (* Check that cert and key are provided if implicit TLS is enabled *)
  if implicit_tls && (cert_file = None || key_file = None) then begin
    Printf.eprintf "Error: --cert and --key are required when using --tls\n";
    exit 1
  end;

  (* Load TLS config if cert and key provided *)
  let tls_config =
    match cert_file, key_file with
    | Some cert, Some key -> load_tls_config ~cert_file:cert ~key_file:key
    | _ -> None
  in

  (* Verify TLS config loaded successfully if implicit TLS is enabled *)
  if implicit_tls && tls_config = None then begin
    Printf.eprintf "Error: Failed to load TLS certificate or key\n";
    exit 1
  end;

  (* Forked mode only works with Maildir *)
  if forked && backend = "memory" then begin
    Printf.eprintf "Error: --fork requires --storage maildir (memory storage not supported)\n";
    exit 1
  end;

  (* Determine storage backend *)
  let storage_backend = match backend with
    | "memory" -> Memory
    | "maildir" ->
      (* In forked mode with no explicit path, use ~/Maildir (home directory) *)
      (* In single-process mode, require explicit path or use /var/mail *)
      let path = match maildir_path, forked with
        | Some p, _ -> Some p
        | None, true -> None  (* Use ~/Maildir in forked mode *)
        | None, false -> Some "/var/mail"  (* Default for non-forked *)
      in
      Maildir path
    | _ ->
      Printf.eprintf "Unknown storage backend: %s\n" backend;
      exit 1
  in

  match storage_backend, forked with
  | Memory, false -> run_with_memory_single ~port ~host ~tls_config ~implicit_tls
  | Maildir (Some path), false -> run_with_maildir_single ~port ~host ~tls_config ~maildir_path:path ~implicit_tls
  | Maildir path, true -> run_with_maildir_forked ~port ~host ~tls_config ~maildir_path:path
  | Maildir None, false -> failwith "unreachable"
  | Memory, true -> failwith "unreachable"

(* Command-line arguments *)
let port =
  let doc = "Port to listen on (default: 143 for cleartext, 993 for TLS)." in
  Arg.(value & opt int 143 & info ["p"; "port"] ~docv:"PORT" ~doc)

let host =
  let doc = "Host address to bind to." in
  Arg.(value & opt string "127.0.0.1" & info ["h"; "host"] ~docv:"HOST" ~doc)

let cert_file =
  let doc = "TLS certificate file (PEM format). Required for --tls." in
  Arg.(value & opt (some string) None & info ["cert"] ~docv:"FILE" ~doc)

let key_file =
  let doc = "TLS private key file (PEM format). Required for --tls." in
  Arg.(value & opt (some string) None & info ["key"] ~docv:"FILE" ~doc)

let backend =
  let doc = "Storage backend to use (memory or maildir)." in
  Arg.(value & opt string "memory" & info ["s"; "storage"] ~docv:"BACKEND" ~doc)

let maildir_path =
  let doc = "Base path for Maildir storage. In single-process mode, defaults to /var/mail. \
             In fork mode (--fork), defaults to using each user's home directory (~/Maildir)." in
  Arg.(value & opt (some string) None & info ["maildir-path"] ~docv:"PATH" ~doc)

let implicit_tls =
  let doc = "Enable implicit TLS (RFC 8314). TLS starts immediately on connection. Requires --cert and --key." in
  Arg.(value & flag & info ["tls"] ~doc)

let forked =
  let doc = "Fork a new process for each connection and drop privileges to the \
             authenticated user after login. Provides strong per-user isolation. \
             Requires running as root. Only works with Maildir storage." in
  Arg.(value & flag & info ["fork"] ~doc)

let cmd =
  let doc = "IMAP4rev2 server" in
  let man = [
    `S Manpage.s_description;
    `P "An IMAP4rev2 server (RFC 9051) implemented in OCaml.";
    `S Manpage.s_options;
    `S "OPERATING MODES";
    `P "$(b,Single-process) (default) - All connections handled in one process. \
        Efficient but all sessions share the same privileges.";
    `P "$(b,Fork-per-connection) (--fork) - Each connection forks a child process. \
        After authentication, the child drops privileges to the authenticated user \
        via setuid. Provides strong isolation between users. Requires running as root.";
    `S "TLS MODES";
    `P "$(b,STARTTLS) (default) - Start cleartext, upgrade to TLS via STARTTLS command. \
        Provide --cert and --key to enable STARTTLS capability. Not supported with --fork.";
    `P "$(b,Implicit TLS) (--tls) - TLS starts immediately on connection per RFC 8314. \
        Typically used on port 993. Recommended for --fork mode.";
    `S "STORAGE BACKENDS";
    `P "$(b,memory) - In-memory storage for development and testing.";
    `P "$(b,maildir) - Maildir-based storage for production use. Required for --fork mode.";
    `S Manpage.s_examples;
    `P "Development server with memory storage:";
    `Pre "  $(tname) -s memory -p 10143";
    `P "Production server with fork-per-connection isolation (recommended):";
    `Pre "  sudo $(tname) --fork -s maildir --tls --cert server.crt --key server.key -p 993";
    `Noblank;
    `P "Uses ~/Maildir for each user (traditional Unix location).";
    `P "Production server with shared mail directory:";
    `Pre "  sudo $(tname) --fork -s maildir --maildir-path /var/mail --tls --cert server.crt --key server.key -p 993";
    `P "Single-process server with STARTTLS:";
    `Pre "  $(tname) -s maildir --maildir-path /var/mail --cert server.crt --key server.key -p 143";
    `S Manpage.s_bugs;
    `P "Report bugs at https://github.com/mtelvers/imapd/issues";
  ] in
  let info = Cmd.info "imapd" ~version:"0.1.0" ~doc ~man in
  Cmd.v info Term.(const run $ port $ host $ cert_file $ key_file $ backend $ maildir_path $ implicit_tls $ forked)

let () = exit (Cmd.eval cmd)
