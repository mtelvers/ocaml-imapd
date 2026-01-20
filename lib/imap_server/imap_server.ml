(** IMAP4rev2 Server

    Implements {{:https://datatracker.ietf.org/doc/html/rfc9051}RFC 9051} state machine. *)

open Imap_types
open Imap_parser

(* Base capabilities per RFC 9051 *)
let base_capabilities_pre_tls = [
  "IMAP4rev2";
  "IMAP4rev1";  (* For compatibility *)
  "AUTH=PLAIN";
  "STARTTLS";
  "IDLE";
  "NAMESPACE";
  "UIDPLUS";
  "MOVE";
  "ENABLE";
  "LITERAL+";
  "ID";
]

let base_capabilities_post_tls = [
  "IMAP4rev2";
  "IMAP4rev1";
  "AUTH=PLAIN";
  "IDLE";
  "NAMESPACE";
  "UIDPLUS";
  "MOVE";
  "ENABLE";
  "LITERAL+";
  "ID";
]

(* Server configuration *)
type config = {
  hostname : string;
  capabilities : string list;
  greeting : string option;
  autologout_timeout : float;
  tls_config : Tls.Config.server option;
}

let default_config = {
  hostname = "localhost";
  capabilities = [];
  greeting = None;
  autologout_timeout = 1800.0;  (* 30 minutes per RFC 9051 Section 5.4 *)
  tls_config = None;
}

module Make
    (Storage : Imap_storage.STORAGE)
    (Auth : Imap_auth.AUTH) = struct

  type connection_state =
    | Not_authenticated
    | Authenticated of { username : string }
    | Selected of { username : string; mailbox : string; readonly : bool }
    | Logout

  (* Action returned by command handlers *)
  type command_action =
    | Continue
    | Upgrade_tls of string  (* tag for response *)

  type t = {
    config : config;
    storage : Storage.t;
    auth : Auth.t;
  }

  let create ~config ~storage ~auth = { config; storage; auth }

  (* Parse HEADER.FIELDS section string to extract field names *)
  (* e.g., "HEADER.FIELDS (date subject from)" -> Some ["date"; "subject"; "from"] *)
  let parse_header_fields_section section =
    let section = String.uppercase_ascii section in
    if String.length section > 15 && String.sub section 0 14 = "HEADER.FIELDS " then
      let rest = String.sub section 14 (String.length section - 14) in
      (* Extract content between parentheses *)
      let rest = String.trim rest in
      if String.length rest >= 2 && rest.[0] = '(' && rest.[String.length rest - 1] = ')' then
        let inner = String.sub rest 1 (String.length rest - 2) in
        let fields = String.split_on_char ' ' inner in
        let fields = List.filter (fun s -> String.length s > 0) fields in
        Some (List.map String.lowercase_ascii fields)
      else None
    else if String.length section > 19 && String.sub section 0 18 = "HEADER.FIELDS.NOT " then
      (* Handle HEADER.FIELDS.NOT - for now just return None, meaning full headers *)
      None
    else None

  (* Filter headers to only include specified fields *)
  (* Headers are in "Field-Name: value\r\n" format *)
  let filter_headers raw_headers field_names =
    match raw_headers with
    | None -> None
    | Some headers ->
      let field_names = List.map String.lowercase_ascii field_names in
      (* Split headers into lines, handling folded headers *)
      let lines = String.split_on_char '\n' headers in
      let lines = List.map (fun l ->
        if String.length l > 0 && l.[String.length l - 1] = '\r'
        then String.sub l 0 (String.length l - 1)
        else l
      ) lines in
      (* Group lines into headers (continuation lines start with whitespace) *)
      let rec group_headers acc current = function
        | [] ->
          let acc = if current <> "" then current :: acc else acc in
          List.rev acc
        | line :: rest ->
          if String.length line > 0 && (line.[0] = ' ' || line.[0] = '\t') then
            (* Continuation line *)
            group_headers acc (current ^ "\r\n" ^ line) rest
          else if current = "" then
            group_headers acc line rest
          else
            group_headers (current :: acc) line rest
      in
      let header_lines = group_headers [] "" lines in
      (* Filter headers by field name *)
      let filtered = List.filter (fun header ->
        match String.index_opt header ':' with
        | None -> false
        | Some idx ->
          let name = String.lowercase_ascii (String.trim (String.sub header 0 idx)) in
          List.mem name field_names
      ) header_lines in
      if filtered = [] then Some ""
      else Some (String.concat "\r\n" filtered ^ "\r\n")

  let all_capabilities t ~tls_active =
    let base = if tls_active then base_capabilities_post_tls else base_capabilities_pre_tls in
    base @ t.config.capabilities

  (* Send a response to the client *)
  let send_response flow response =
    let data = response_to_string response in
    Eio.Flow.copy_string data flow

  (* Send greeting *)
  let send_greeting t flow ~tls_active =
    let caps = all_capabilities t ~tls_active in
    let greeting = match t.config.greeting with
      | Some g -> g
      | None -> "IMAP4rev2 Service Ready"
    in
    let response = Ok {
      tag = None;
      code = Some (Code_capability caps);
      text = greeting;
    } in
    send_response flow response

  (* Process CAPABILITY command - valid in any state *)
  let handle_capability t flow tag ~tls_active =
    let caps = all_capabilities t ~tls_active in
    send_response flow (Capability_response caps);
    send_response flow (Ok { tag = Some tag; code = None; text = "CAPABILITY completed" })

  (* Process NOOP command - valid in any state *)
  let handle_noop flow tag =
    send_response flow (Ok { tag = Some tag; code = None; text = "NOOP completed" })

  (* Process ID command - RFC 2971 - valid in any state *)
  let handle_id flow tag _client_params =
    (* Return server identification *)
    let server_id = Some [
      ("name", "imapd");
      ("vendor", "OCaml IMAP");
      ("version", "0.1.0");
      ("support-url", "https://github.com/mtelvers/imapd");
    ] in
    send_response flow (Id_response server_id);
    send_response flow (Ok { tag = Some tag; code = None; text = "ID completed" })

  (* Process LOGOUT command - valid in any state *)
  let handle_logout flow tag =
    send_response flow (Bye { code = None; text = "IMAP4rev2 Server logging out" });
    send_response flow (Ok { tag = Some tag; code = None; text = "LOGOUT completed" });
    Logout

  (* Process LOGIN command - only valid in Not_authenticated state *)
  let handle_login t flow tag ~username ~password ~tls_active state =
    match state with
    | Not_authenticated ->
      (* Security: Validate username before authentication *)
      if not (Imap_types.is_safe_username username) then begin
        send_response flow (No {
          tag = Some tag;
          code = Some Code_authenticationfailed;
          text = "LOGIN failed"
        });
        state
      end else if Auth.authenticate t.auth ~username ~password then begin
        let caps = all_capabilities t ~tls_active in
        send_response flow (Ok {
          tag = Some tag;
          code = Some (Code_capability caps);
          text = "LOGIN completed"
        });
        Authenticated { username }
      end else begin
        send_response flow (No {
          tag = Some tag;
          code = Some Code_authenticationfailed;
          text = "LOGIN failed"
        });
        state
      end
    | _ ->
      send_response flow (Bad {
        tag = Some tag;
        code = None;
        text = "Command not valid in this state"
      });
      state

  (* Process SELECT/EXAMINE command - only valid in Authenticated/Selected state *)
  let handle_select t flow tag mailbox ~readonly state =
    let username = match state with
      | Authenticated { username } -> Some username
      | Selected { username; _ } -> Some username
      | _ -> None
    in
    match username with
    | None ->
      send_response flow (Bad {
        tag = Some tag;
        code = None;
        text = "Command not valid in this state"
      });
      state
    | Some username ->
      (* Security: Validate mailbox name *)
      if not (Imap_types.is_safe_mailbox_name mailbox) then begin
        send_response flow (No {
          tag = Some tag;
          code = None;
          text = "Invalid mailbox name"
        });
        Authenticated { username }
      end else
      match Storage.select_mailbox t.storage ~username mailbox ~readonly with
      | Error _ ->
        send_response flow (No {
          tag = Some tag;
          code = Some Code_nonexistent;
          text = "Mailbox does not exist"
        });
        Authenticated { username }
      | Ok mb_state ->
        (* Send untagged responses *)
        send_response flow (Flags_response mb_state.flags);
        send_response flow (Exists mb_state.exists);
        send_response flow (Ok {
          tag = None;
          code = Some (Code_permanentflags mb_state.permanent_flags);
          text = "Flags permitted"
        });
        send_response flow (Ok {
          tag = None;
          code = Some (Code_uidvalidity mb_state.uidvalidity);
          text = "UIDs valid"
        });
        send_response flow (Ok {
          tag = None;
          code = Some (Code_uidnext mb_state.uidnext);
          text = "Predicted next UID"
        });
        (* Send tagged OK *)
        let code = if readonly then Some Code_readonly else Some Code_readwrite in
        send_response flow (Ok {
          tag = Some tag;
          code;
          text = if readonly then "EXAMINE completed" else "SELECT completed"
        });
        Selected { username; mailbox; readonly }

  (* Process LIST command *)
  let handle_list t flow tag ~reference ~pattern state =
    let username = match state with
      | Authenticated { username } -> Some username
      | Selected { username; _ } -> Some username
      | _ -> None
    in
    match username with
    | None ->
      send_response flow (Bad {
        tag = Some tag;
        code = None;
        text = "Command not valid in this state"
      });
      state
    | Some username ->
      let mailboxes = Storage.list_mailboxes t.storage ~username ~reference ~pattern in
      List.iter (fun (mb : Imap_storage.mailbox_info) ->
        send_response flow (List_response {
          flags = mb.flags;
          delimiter = mb.delimiter;
          name = mb.name;
        })
      ) mailboxes;
      send_response flow (Ok { tag = Some tag; code = None; text = "LIST completed" });
      state

  (* Process STATUS command *)
  let handle_status t flow tag mailbox ~items state =
    (* Security: Validate mailbox name *)
    if not (Imap_types.is_safe_mailbox_name mailbox) then begin
      send_response flow (No { tag = Some tag; code = None; text = "Invalid mailbox name" });
      state
    end else
    let username = match state with
      | Authenticated { username } -> Some username
      | Selected { username; _ } -> Some username
      | _ -> None
    in
    match username with
    | None ->
      send_response flow (Bad {
        tag = Some tag;
        code = None;
        text = "Command not valid in this state"
      });
      state
    | Some username ->
      match Storage.status_mailbox t.storage ~username mailbox ~items with
      | Error _ ->
        send_response flow (No {
          tag = Some tag;
          code = Some Code_nonexistent;
          text = "Mailbox does not exist"
        });
        state
      | Ok results ->
        send_response flow (Status_response { mailbox; items = results });
        send_response flow (Ok { tag = Some tag; code = None; text = "STATUS completed" });
        state

  (* Process FETCH command *)
  let handle_fetch t flow tag ~sequence ~items ?(use_uid=false) state =
    match state with
    | Selected { username; mailbox; _ } ->
      let fetch_fn = if use_uid then Storage.fetch_by_uid ~uids:sequence else Storage.fetch_messages ~sequence in
      (match fetch_fn t.storage ~username ~mailbox ~items with
       | Error _ ->
         send_response flow (No { tag = Some tag; code = None; text = "FETCH failed" })
       | Ok messages ->
         List.iter (fun (msg : message) ->
           (* Build response items based on what was requested *)
           let fetch_items = List.filter_map (fun item ->
             match item with
             | Fetch_uid -> Some (Fetch_item_uid msg.uid)
             | Fetch_flags -> Some (Fetch_item_flags msg.flags)
             | Fetch_rfc822_size -> Some (Fetch_item_rfc822_size msg.size)
             | Fetch_internaldate -> Some (Fetch_item_internaldate msg.internal_date)
             | Fetch_envelope -> Option.map (fun e -> Fetch_item_envelope e) msg.envelope
             | Fetch_body ->
               (* Return body structure - use actual if available, otherwise default *)
               let body = match msg.body_structure with
                 | Some b -> b
                 | None ->
                   let fields = { params = []; content_id = None; description = None;
                                  encoding = "7BIT"; size = msg.size } in
                   { body_type = Text { subtype = "PLAIN"; fields; lines = 0L };
                     disposition = None; language = None; location = None }
               in
               Some (Fetch_item_body body)
             | Fetch_bodystructure ->
               (* Return body structure - use actual if available, otherwise default *)
               let body = match msg.body_structure with
                 | Some b -> b
                 | None ->
                   let fields = { params = []; content_id = None; description = None;
                                  encoding = "7BIT"; size = msg.size } in
                   { body_type = Text { subtype = "PLAIN"; fields; lines = 0L };
                     disposition = None; language = None; location = None }
               in
               Some (Fetch_item_bodystructure body)
             | Fetch_rfc822 | Fetch_body_section ("", None) | Fetch_body_peek ("", None) ->
               (* BODY[] or RFC822 - return full message *)
               let data = match msg.raw_headers, msg.raw_body with
                 | Some h, Some b -> Some (h ^ "\r\n\r\n" ^ b)
                 | Some h, None -> Some (h ^ "\r\n")
                 | None, Some b -> Some b
                 | None, None -> None
               in
               Some (Fetch_item_body_section { section = None; origin = None; data })
             | Fetch_body_section ("", Some (offset, count)) | Fetch_body_peek ("", Some (offset, count)) ->
               (* BODY[]<offset.count> - return partial message *)
               let full_data = match msg.raw_headers, msg.raw_body with
                 | Some h, Some b -> h ^ "\r\n\r\n" ^ b
                 | Some h, None -> h ^ "\r\n"
                 | None, Some b -> b
                 | None, None -> ""
               in
               let data =
                 if offset >= String.length full_data then Some ""
                 else
                   let available = String.length full_data - offset in
                   let len = min count available in
                   Some (String.sub full_data offset len)
               in
               Some (Fetch_item_body_section { section = None; origin = Some offset; data })
             | Fetch_rfc822_header | Fetch_body_section ("HEADER", _) | Fetch_body_peek ("HEADER", _) ->
               Some (Fetch_item_body_section { section = Some Section_header; origin = None; data = msg.raw_headers })
             | Fetch_rfc822_text | Fetch_body_section ("TEXT", None) | Fetch_body_peek ("TEXT", None) ->
               Some (Fetch_item_body_section { section = Some Section_text; origin = None; data = msg.raw_body })
             | Fetch_body_section ("TEXT", Some (offset, count)) | Fetch_body_peek ("TEXT", Some (offset, count)) ->
               (* BODY[TEXT]<offset.count> - return partial body text *)
               let full_data = match msg.raw_body with
                 | Some b -> b
                 | None -> ""
               in
               let data =
                 if offset >= String.length full_data then Some ""
                 else
                   let available = String.length full_data - offset in
                   let len = min count available in
                   Some (String.sub full_data offset len)
               in
               Some (Fetch_item_body_section { section = Some Section_text; origin = Some offset; data })
             | Fetch_body_section (s, partial) | Fetch_body_peek (s, partial) ->
               (* Handle section specifiers *)
               (match parse_header_fields_section s with
                | Some field_names ->
                  (* HEADER.FIELDS (...) - filter to requested headers *)
                  let data = filter_headers msg.raw_headers field_names in
                  Some (Fetch_item_body_section {
                    section = Some (Section_header_fields field_names);
                    origin = None;
                    data
                  })
                | None ->
                  (* Check if it's a numeric section like "1", "2", "1.2.3" *)
                  let is_numeric_section =
                    s <> "" &&
                    String.for_all (fun c -> c >= '0' && c <= '9' || c = '.') s
                  in
                  if is_numeric_section then begin
                    (* Extract specific MIME part *)
                    let full_message = match msg.raw_headers, msg.raw_body with
                      | Some h, Some b -> h ^ "\r\n\r\n" ^ b
                      | Some h, None -> h ^ "\r\n"
                      | None, Some b -> b
                      | None, None -> ""
                    in
                    let part_data = Imap_envelope.extract_mime_part full_message s in
                    let data, origin = match partial, part_data with
                      | None, d -> (d, None)
                      | Some (offset, count), Some full_data ->
                        if offset >= String.length full_data then (Some "", Some offset)
                        else
                          let available = String.length full_data - offset in
                          let len = min count available in
                          (Some (String.sub full_data offset len), Some offset)
                      | Some (offset, _), None -> (None, Some offset)
                    in
                    Some (Fetch_item_body_section {
                      section = Some (Section_part (String.split_on_char '.' s |> List.filter_map int_of_string_opt, None));
                      origin;
                      data
                    })
                  end else begin
                    (* Other section types - return full message *)
                    let data = match msg.raw_headers, msg.raw_body with
                      | Some h, Some b -> Some (h ^ "\r\n\r\n" ^ b)
                      | Some h, None -> Some (h ^ "\r\n")
                      | None, Some b -> Some b
                      | None, None -> None
                    in
                    Some (Fetch_item_body_section { section = None; origin = None; data })
                  end)
             | Fetch_binary _ | Fetch_binary_peek _ | Fetch_binary_size _ ->
               (* Binary not implemented yet *)
               None
           ) items in
           (* Always include UID in the response *)
           let fetch_items =
             if List.exists (function Fetch_item_uid _ -> true | _ -> false) fetch_items then
               fetch_items
             else
               Fetch_item_uid msg.uid :: fetch_items
           in
           send_response flow (Fetch_response { seq = msg.seq; items = fetch_items })
         ) messages;
         send_response flow (Ok { tag = Some tag; code = None; text = "FETCH completed" }));
      state
    | _ ->
      send_response flow (Bad {
        tag = Some tag;
        code = None;
        text = "Command not valid in this state"
      });
      state

  (* Process STORE command *)
  let handle_store t flow tag ~sequence ~silent ~action ~flags ?(use_uid=false) state =
    match state with
    | Selected { username; mailbox; readonly } ->
      if readonly then begin
        send_response flow (No { tag = Some tag; code = None; text = "Mailbox is read-only" });
        state
      end else begin
        let store_fn = if use_uid then Storage.store_by_uid ~uids:sequence else Storage.store_flags ~sequence in
        match store_fn t.storage ~username ~mailbox ~action ~flags with
        | Error _ ->
          send_response flow (No { tag = Some tag; code = None; text = "STORE failed" });
          state
        | Ok messages ->
          if not silent then
            List.iter (fun (msg : message) ->
              send_response flow (Fetch_response {
                seq = msg.seq;
                items = [Fetch_item_flags msg.flags]
              })
            ) messages;
          send_response flow (Ok { tag = Some tag; code = None; text = "STORE completed" });
          state
      end
    | _ ->
      send_response flow (Bad {
        tag = Some tag;
        code = None;
        text = "Command not valid in this state"
      });
      state

  (* Process EXPUNGE command *)
  let handle_expunge t flow tag state =
    match state with
    | Selected { username; mailbox; readonly } ->
      if readonly then begin
        send_response flow (No { tag = Some tag; code = None; text = "Mailbox is read-only" });
        state
      end else begin
        match Storage.expunge t.storage ~username ~mailbox with
        | Error _ ->
          send_response flow (No { tag = Some tag; code = None; text = "EXPUNGE failed" });
          state
        | Ok _uids ->
          (* Send EXPUNGE responses for each removed message *)
          send_response flow (Ok { tag = Some tag; code = None; text = "EXPUNGE completed" });
          state
      end
    | _ ->
      send_response flow (Bad {
        tag = Some tag;
        code = None;
        text = "Command not valid in this state"
      });
      state

  (* Process CLOSE command *)
  let handle_close t flow tag state =
    match state with
    | Selected { username; mailbox; readonly } ->
      (* Silently expunge if not readonly *)
      if not readonly then
        ignore (Storage.expunge t.storage ~username ~mailbox);
      send_response flow (Ok { tag = Some tag; code = None; text = "CLOSE completed" });
      Authenticated { username }
    | _ ->
      send_response flow (Bad {
        tag = Some tag;
        code = None;
        text = "Command not valid in this state"
      });
      state

  (* Process UNSELECT command *)
  let handle_unselect flow tag state =
    match state with
    | Selected { username; _ } ->
      send_response flow (Ok { tag = Some tag; code = None; text = "UNSELECT completed" });
      Authenticated { username }
    | _ ->
      send_response flow (Bad {
        tag = Some tag;
        code = None;
        text = "Command not valid in this state"
      });
      state

  (* Process CREATE command *)
  let handle_create t flow tag mailbox state =
    (* Security: Validate mailbox name *)
    if not (Imap_types.is_safe_mailbox_name mailbox) then begin
      send_response flow (No { tag = Some tag; code = None; text = "Invalid mailbox name" });
      state
    end else
    let username = match state with
      | Authenticated { username } -> Some username
      | Selected { username; _ } -> Some username
      | _ -> None
    in
    match username with
    | None ->
      send_response flow (Bad {
        tag = Some tag;
        code = None;
        text = "Command not valid in this state"
      });
      state
    | Some username ->
      match Storage.create_mailbox t.storage ~username mailbox with
      | Ok () ->
        send_response flow (Ok { tag = Some tag; code = None; text = "CREATE completed" });
        state
      | Error Imap_storage.Mailbox_already_exists ->
        send_response flow (No {
          tag = Some tag;
          code = Some Code_alreadyexists;
          text = "Mailbox already exists"
        });
        state
      | Error _ ->
        send_response flow (No { tag = Some tag; code = None; text = "CREATE failed" });
        state

  (* Process DELETE command *)
  let handle_delete t flow tag mailbox state =
    (* Security: Validate mailbox name *)
    if not (Imap_types.is_safe_mailbox_name mailbox) then begin
      send_response flow (No { tag = Some tag; code = None; text = "Invalid mailbox name" });
      state
    end else
    let username = match state with
      | Authenticated { username } -> Some username
      | Selected { username; _ } -> Some username
      | _ -> None
    in
    match username with
    | None ->
      send_response flow (Bad {
        tag = Some tag;
        code = None;
        text = "Command not valid in this state"
      });
      state
    | Some username ->
      match Storage.delete_mailbox t.storage ~username mailbox with
      | Ok () ->
        send_response flow (Ok { tag = Some tag; code = None; text = "DELETE completed" });
        state
      | Error Imap_storage.Permission_denied ->
        send_response flow (No {
          tag = Some tag;
          code = Some Code_cannot;
          text = "Cannot delete INBOX"
        });
        state
      | Error _ ->
        send_response flow (No { tag = Some tag; code = None; text = "DELETE failed" });
        state

  (* Process RENAME command *)
  let handle_rename t flow tag ~old_name ~new_name state =
    (* Security: Validate both mailbox names *)
    if not (Imap_types.is_safe_mailbox_name old_name) ||
       not (Imap_types.is_safe_mailbox_name new_name) then begin
      send_response flow (No { tag = Some tag; code = None; text = "Invalid mailbox name" });
      state
    end else
    let username = match state with
      | Authenticated { username } -> Some username
      | Selected { username; _ } -> Some username
      | _ -> None
    in
    match username with
    | None ->
      send_response flow (Bad {
        tag = Some tag;
        code = None;
        text = "Command not valid in this state"
      });
      state
    | Some username ->
      match Storage.rename_mailbox t.storage ~username ~old_name ~new_name with
      | Result.Ok () ->
        send_response flow (Ok { tag = Some tag; code = None; text = "RENAME completed" });
        state
      | Result.Error Imap_storage.Mailbox_not_found ->
        send_response flow (No {
          tag = Some tag;
          code = Some Code_nonexistent;
          text = "Mailbox does not exist"
        });
        state
      | Result.Error Imap_storage.Mailbox_already_exists ->
        send_response flow (No {
          tag = Some tag;
          code = Some Code_alreadyexists;
          text = "Target mailbox already exists"
        });
        state
      | Result.Error _ ->
        send_response flow (No { tag = Some tag; code = None; text = "RENAME failed" });
        state

  (* Process COPY command *)
  let handle_copy t flow tag ~sequence ~mailbox state =
    (* Security: Validate destination mailbox name *)
    if not (Imap_types.is_safe_mailbox_name mailbox) then begin
      send_response flow (No { tag = Some tag; code = None; text = "Invalid mailbox name" });
      state
    end else
    match state with
    | Selected { username; mailbox = src_mailbox; _ } ->
      (match Storage.copy t.storage ~username ~src_mailbox ~sequence ~dst_mailbox:mailbox with
       | Result.Error Imap_storage.Mailbox_not_found ->
         send_response flow (No {
           tag = Some tag;
           code = Some Code_trycreate;
           text = "Destination mailbox does not exist"
         })
       | Result.Error _ ->
         send_response flow (No { tag = Some tag; code = None; text = "COPY failed" })
       | Result.Ok dst_uids ->
         (* UIDPLUS: include COPYUID response code *)
         let uidvalidity = match Storage.select_mailbox t.storage ~username mailbox ~readonly:true with
           | Result.Ok mb -> mb.uidvalidity
           | Result.Error _ -> 1l
         in
         let dst_set = List.map (fun uid -> Single (Int32.to_int uid)) dst_uids in
         send_response flow (Ok {
           tag = Some tag;
           code = Some (Code_copyuid (uidvalidity, sequence, dst_set));
           text = "COPY completed"
         }));
      state
    | _ ->
      send_response flow (Bad {
        tag = Some tag;
        code = None;
        text = "Command not valid in this state"
      });
      state

  (* Process MOVE command - RFC 6851 *)
  let handle_move t flow tag ~sequence ~mailbox state =
    (* Security: Validate destination mailbox name *)
    if not (Imap_types.is_safe_mailbox_name mailbox) then begin
      send_response flow (No { tag = Some tag; code = None; text = "Invalid mailbox name" });
      state
    end else
    match state with
    | Selected { username; mailbox = src_mailbox; readonly } ->
      if readonly then begin
        send_response flow (No { tag = Some tag; code = None; text = "Mailbox is read-only" });
        state
      end else begin
        match Storage.move t.storage ~username ~src_mailbox ~sequence ~dst_mailbox:mailbox with
        | Result.Error Imap_storage.Mailbox_not_found ->
          send_response flow (No {
            tag = Some tag;
            code = Some Code_trycreate;
            text = "Destination mailbox does not exist"
          });
          state
        | Result.Error _ ->
          send_response flow (No { tag = Some tag; code = None; text = "MOVE failed" });
          state
        | Result.Ok dst_uids ->
          (* UIDPLUS: include COPYUID response code for MOVE as well *)
          let uidvalidity = match Storage.select_mailbox t.storage ~username mailbox ~readonly:true with
            | Result.Ok mb -> mb.uidvalidity
            | Result.Error _ -> 1l
          in
          let dst_set = List.map (fun uid -> Single (Int32.to_int uid)) dst_uids in
          send_response flow (Ok {
            tag = Some tag;
            code = Some (Code_copyuid (uidvalidity, sequence, dst_set));
            text = "MOVE completed"
          });
          state
      end
    | _ ->
      send_response flow (Bad {
        tag = Some tag;
        code = None;
        text = "Command not valid in this state"
      });
      state

  (* Process SEARCH command *)
  let handle_search t flow tag ~charset:_ ~criteria state =
    match state with
    | Selected { username; mailbox; _ } ->
      (match Storage.search t.storage ~username ~mailbox ~criteria with
       | Result.Error _ ->
         send_response flow (No { tag = Some tag; code = None; text = "SEARCH failed" })
       | Result.Ok uids ->
         (* Send ESEARCH response per RFC 9051 *)
         let results = if List.length uids > 0 then
           [Esearch_count (List.length uids); Esearch_all (List.map (fun uid -> Single (Int32.to_int uid)) uids)]
         else
           [Esearch_count 0]
         in
         send_response flow (Esearch { tag = Some tag; uid = false; results });
         send_response flow (Ok { tag = Some tag; code = None; text = "SEARCH completed" }));
      state
    | _ ->
      send_response flow (Bad {
        tag = Some tag;
        code = None;
        text = "Command not valid in this state"
      });
      state

  (* Process APPEND command *)
  let handle_append t flow tag ~mailbox ~flags ~date ~message state =
    (* Security: Validate mailbox name *)
    if not (Imap_types.is_safe_mailbox_name mailbox) then begin
      send_response flow (No { tag = Some tag; code = None; text = "Invalid mailbox name" });
      state
    end else
    let username = match state with
      | Authenticated { username } -> Some username
      | Selected { username; _ } -> Some username
      | _ -> None
    in
    match username with
    | None ->
      send_response flow (Bad {
        tag = Some tag;
        code = None;
        text = "Command not valid in this state"
      });
      state
    | Some username ->
      match Storage.append t.storage ~username ~mailbox ~flags ~date ~message with
      | Result.Error Imap_storage.Mailbox_not_found ->
        send_response flow (No {
          tag = Some tag;
          code = Some Code_trycreate;
          text = "Mailbox does not exist"
        });
        state
      | Result.Error _ ->
        send_response flow (No { tag = Some tag; code = None; text = "APPEND failed" });
        state
      | Result.Ok uid ->
        (* Get UIDVALIDITY for response *)
        let uidvalidity = match Storage.select_mailbox t.storage ~username mailbox ~readonly:true with
          | Result.Ok mb -> mb.uidvalidity
          | Result.Error _ -> 1l
        in
        send_response flow (Ok {
          tag = Some tag;
          code = Some (Code_appenduid (uidvalidity, uid));
          text = "APPEND completed"
        });
        state

  (* Process NAMESPACE command - RFC 2342 *)
  let handle_namespace flow tag state =
    match state with
    | Authenticated _ | Selected _ ->
      send_response flow (Namespace_response {
        personal = Some [{ prefix = ""; delimiter = Some '/' }];
        other = None;
        shared = None;
      });
      send_response flow (Ok { tag = Some tag; code = None; text = "NAMESPACE completed" });
      state
    | _ ->
      send_response flow (Bad {
        tag = Some tag;
        code = None;
        text = "Command not valid in this state"
      });
      state

  (* Process ENABLE command - RFC 5161 *)
  let handle_enable flow tag ~capabilities state =
    match state with
    | Authenticated _ ->
      (* Filter to capabilities we actually support *)
      let enabled = List.filter (fun cap ->
        let cap_upper = String.uppercase_ascii cap in
        cap_upper = "IMAP4REV2" || cap_upper = "UTF8=ACCEPT"
      ) capabilities in
      if List.length enabled > 0 then
        send_response flow (Enabled enabled);
      send_response flow (Ok { tag = Some tag; code = None; text = "ENABLE completed" });
      state
    | _ ->
      send_response flow (Bad {
        tag = Some tag;
        code = None;
        text = "ENABLE only valid in authenticated state before SELECT"
      });
      state

  (* Process SUBSCRIBE/UNSUBSCRIBE - simplified, just succeed *)
  let handle_subscribe flow tag _mailbox state =
    match state with
    | Authenticated _ | Selected _ ->
      send_response flow (Ok { tag = Some tag; code = None; text = "SUBSCRIBE completed" });
      state
    | _ ->
      send_response flow (Bad {
        tag = Some tag;
        code = None;
        text = "Command not valid in this state"
      });
      state

  let handle_unsubscribe flow tag _mailbox state =
    match state with
    | Authenticated _ | Selected _ ->
      send_response flow (Ok { tag = Some tag; code = None; text = "UNSUBSCRIBE completed" });
      state
    | _ ->
      send_response flow (Bad {
        tag = Some tag;
        code = None;
        text = "Command not valid in this state"
      });
      state

  (* Process IDLE command - RFC 2177
     Polls mailbox for changes every 2 seconds and sends EXISTS/RECENT updates *)
  let handle_idle t flow tag read_line_fn ~clock state =
    match state with
    | Selected { username; mailbox; _ } ->
      send_response flow (Continuation (Some "idling"));

      (* Get initial message count *)
      let get_message_count () =
        match Storage.select_mailbox t.storage ~username mailbox ~readonly:true with
        | Ok mb_state -> mb_state.exists
        | Error _ -> 0
      in
      let last_count = ref (get_message_count ()) in

      (* Poll interval in seconds *)
      let poll_interval = 2.0 in

      (* Buffer for reading input *)
      let input_buf = Buffer.create 64 in
      let cs = Cstruct.create 1 in

      (* Main IDLE loop using Switch.run for fresh cancellation context *)
      let rec idle_loop () =
        let read_result =
          Eio.Switch.run @@ fun _sw ->
          try
            let rec read_char () : string =
              let n = Eio.Flow.single_read flow cs in
              if n > 0 then begin
                let c = Cstruct.get_char cs 0 in
                Buffer.add_char input_buf c;
                if c = '\n' then begin
                  let line = Buffer.contents input_buf in
                  Buffer.clear input_buf;
                  line
                end else
                  read_char ()
              end else
                read_char ()
            in
            `Line (Eio.Time.with_timeout_exn clock poll_interval read_char)
          with
          | Eio.Time.Timeout -> `Timeout
          | End_of_file -> `Closed
        in
        match read_result with
        | `Line line ->
          Eio.traceln "IDLE: received line: %s" (String.trim line);
          let trimmed = String.trim (String.uppercase_ascii line) in
          if trimmed = "DONE" then
            send_response flow (Ok { tag = Some tag; code = None; text = "IDLE terminated" })
          else
            idle_loop ()  (* Ignore other input, keep idling *)
        | `Closed ->
          Eio.traceln "IDLE: connection closed";
          ()  (* Exit IDLE *)
        | `Timeout ->
          (* Timeout occurred - check for mailbox changes *)
          let current_count = get_message_count () in
          Eio.traceln "IDLE: polling, last=%d current=%d" !last_count current_count;
          if current_count > !last_count then begin
            (* New messages arrived - send EXISTS update *)
            Eio.traceln "IDLE: sending EXISTS %d" current_count;
            send_response flow (Exists current_count);
            last_count := current_count
          end;
          idle_loop ()
      in
      Eio.traceln "IDLE: starting with initial count %d" !last_count;
      idle_loop ();
      state
    | Authenticated _ ->
      (* IDLE in authenticated state - just wait for DONE *)
      send_response flow (Continuation (Some "idling"));
      let rec wait_for_done () =
        match read_line_fn () with
        | None -> ()  (* Connection closed *)
        | Some line ->
          let trimmed = String.trim (String.uppercase_ascii line) in
          if trimmed = "DONE" then
            send_response flow (Ok { tag = Some tag; code = None; text = "IDLE terminated" })
          else
            wait_for_done ()
      in
      wait_for_done ();
      state
    | _ ->
      send_response flow (Bad {
        tag = Some tag;
        code = None;
        text = "Command not valid in this state"
      });
      state

  (* Main command dispatcher *)
  let rec handle_command t flow ~read_line_fn ~tls_active ~clock cmd state =
    let tag = cmd.tag in
    match cmd.command with
    | Capability -> handle_capability t flow tag ~tls_active; (state, Continue)
    | Noop -> handle_noop flow tag; (state, Continue)
    | Id params -> handle_id flow tag params; (state, Continue)
    | Logout -> (handle_logout flow tag, Continue)
    | Login { username; password } -> (handle_login t flow tag ~username ~password ~tls_active state, Continue)
    | Select mailbox -> (handle_select t flow tag mailbox ~readonly:false state, Continue)
    | Examine mailbox -> (handle_select t flow tag mailbox ~readonly:true state, Continue)
    | List { reference; pattern } -> (handle_list t flow tag ~reference ~pattern state, Continue)
    | Status { mailbox; items } -> (handle_status t flow tag mailbox ~items state, Continue)
    | Fetch { sequence; items } -> (handle_fetch t flow tag ~sequence ~items state, Continue)
    | Store { sequence; silent; action; flags } -> (handle_store t flow tag ~sequence ~silent ~action ~flags state, Continue)
    | Expunge -> (handle_expunge t flow tag state, Continue)
    | Close -> (handle_close t flow tag state, Continue)
    | Unselect -> (handle_unselect flow tag state, Continue)
    | Create mailbox -> (handle_create t flow tag mailbox state, Continue)
    | Delete mailbox -> (handle_delete t flow tag mailbox state, Continue)
    | Rename { old_name; new_name } -> (handle_rename t flow tag ~old_name ~new_name state, Continue)
    | Copy { sequence; mailbox } -> (handle_copy t flow tag ~sequence ~mailbox state, Continue)
    | Move { sequence; mailbox } -> (handle_move t flow tag ~sequence ~mailbox state, Continue)
    | Search { charset; criteria } -> (handle_search t flow tag ~charset ~criteria state, Continue)
    | Append { mailbox; flags; date; message } -> (handle_append t flow tag ~mailbox ~flags ~date ~message state, Continue)
    | Namespace -> (handle_namespace flow tag state, Continue)
    | Enable caps -> (handle_enable flow tag ~capabilities:caps state, Continue)
    | Subscribe mailbox -> (handle_subscribe flow tag mailbox state, Continue)
    | Unsubscribe mailbox -> (handle_unsubscribe flow tag mailbox state, Continue)
    | Idle -> (handle_idle t flow tag read_line_fn ~clock state, Continue)
    | Uid uid_cmd -> (handle_uid_command t flow tag ~read_line_fn uid_cmd state, Continue)
    | Starttls ->
      (match t.config.tls_config with
       | None ->
         send_response flow (Bad { tag = Some tag; code = None; text = "STARTTLS not available" });
         (state, Continue)
       | Some _ when tls_active ->
         send_response flow (Bad { tag = Some tag; code = None; text = "TLS already active" });
         (state, Continue)
       | Some _ when state <> Not_authenticated ->
         send_response flow (Bad { tag = Some tag; code = None; text = "STARTTLS only valid before authentication" });
         (state, Continue)
       | Some _ ->
         (* Signal to connection handler to upgrade *)
         (state, Upgrade_tls tag))
    | Authenticate _ ->
      send_response flow (No { tag = Some tag; code = None; text = "Use LOGIN instead" });
      (state, Continue)

  (* Handle UID prefixed commands *)
  and handle_uid_command t flow tag ~read_line_fn:_ uid_cmd state =
    match uid_cmd with
    | Uid_fetch { sequence; items } ->
      (* For UID FETCH, sequence is UIDs not sequence numbers *)
      handle_fetch t flow tag ~sequence ~items ~use_uid:true state
    | Uid_store { sequence; silent; action; flags } ->
      handle_store t flow tag ~sequence ~silent ~action ~flags ~use_uid:true state
    | Uid_copy { sequence; mailbox } ->
      handle_copy t flow tag ~sequence ~mailbox state
    | Uid_move { sequence; mailbox } ->
      handle_move t flow tag ~sequence ~mailbox state
    | Uid_search { charset; criteria } ->
      handle_search t flow tag ~charset ~criteria state
    | Uid_expunge _sequence ->
      (* UID EXPUNGE only expunges messages in the given UID set *)
      handle_expunge t flow tag state

  (* Maximum line length to prevent DoS attacks via memory exhaustion.
     RFC 9051 Section 4 recommends supporting lines up to 8192 octets. *)
  let max_line_length = 65536

  (* Read a line from the client *)
  let read_line flow =
    let buf = Buffer.create 256 in
    let cs = Cstruct.create 1 in
    let rec loop () =
      try
        (* Security: Prevent memory exhaustion from unbounded line length *)
        if Buffer.length buf > max_line_length then
          None  (* Reject overly long lines *)
        else
          let n = Eio.Flow.single_read flow cs in
          if n = 0 then
            None
          else begin
            let c = Cstruct.get_char cs 0 in
            Buffer.add_char buf c;
            if c = '\n' && Buffer.length buf >= 2 &&
               Buffer.nth buf (Buffer.length buf - 2) = '\r' then
              Some (Buffer.contents buf)
            else
              loop ()
          end
      with End_of_file ->
        if Buffer.length buf > 0 then Some (Buffer.contents buf) else None
    in
    loop ()

  (* Main command loop - returns when connection should close or upgrade TLS *)
  let rec command_loop t flow state ~tls_active ~clock =
    let read_line_fn () = read_line flow in
    match state with
    | Logout -> `Done
    | _ ->
      match read_line flow with
      | None -> `Done  (* Connection closed *)
      | Some line ->
        (* Debug: log received command *)
        Eio.traceln "IMAP < %s" (String.trim line);
        match parse_command line with
        | Error msg ->
          Eio.traceln "IMAP ! Parse error: %s" msg;
          send_response flow (Bad {
            tag = None;
            code = None;
            text = "Invalid command syntax"
          });
          command_loop t flow state ~tls_active ~clock
        | Result.Ok cmd ->
          let (new_state, action) = handle_command t flow ~read_line_fn ~tls_active ~clock cmd state in
          match action with
          | Continue -> command_loop t flow new_state ~tls_active ~clock
          | Upgrade_tls tag -> `Upgrade_tls (tag, new_state)

  (* Internal connection handler with TLS state *)
  let handle_connection_internal t (flow : _ Eio.Flow.two_way) ~tls_active ~send_greeting:should_greet ~initial_state ~clock =
    (* Send greeting only for new connections *)
    if should_greet then send_greeting t flow ~tls_active;
    command_loop t flow initial_state ~tls_active ~clock

  (* Connection handler for cleartext connections (may upgrade to TLS) *)
  let handle_connection t flow _addr ~clock =
    match handle_connection_internal t flow ~tls_active:false ~send_greeting:true ~initial_state:Not_authenticated ~clock with
    | `Done -> ()
    | `Upgrade_tls (tag, state) ->
      (* Upgrade to TLS *)
      match t.config.tls_config with
      | None ->
        send_response flow (Bad { tag = Some tag; code = None; text = "TLS not configured" })
      | Some tls_config ->
        (* Send OK before upgrading *)
        send_response flow (Ok { tag = Some tag; code = None; text = "Begin TLS negotiation" });
        (* Upgrade the connection to TLS *)
        let tls_flow = Tls_eio.server_of_flow tls_config flow in
        (* Continue with TLS-wrapped flow, preserving state (which is Not_authenticated after STARTTLS) *)
        (* Per RFC 3501, STARTTLS does not send a new greeting, session continues *)
        ignore (handle_connection_internal t (tls_flow :> _ Eio.Flow.two_way) ~tls_active:true ~send_greeting:false ~initial_state:state ~clock)

  (* Connection handler for already-TLS connections (implicit TLS on port 993) *)
  let handle_connection_tls t tls_flow _addr ~clock =
    ignore (handle_connection_internal t (tls_flow :> _ Eio.Flow.two_way) ~tls_active:true ~send_greeting:true ~initial_state:Not_authenticated ~clock)

  (* Run server on cleartext port - single process mode (no privilege separation) *)
  let run t ~sw ~net ~addr ~clock ?(after_bind = fun () -> ()) () =
    let socket = Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:128 addr in
    after_bind ();
    let rec accept_loop () =
      Eio.Net.accept_fork socket ~sw
        ~on_error:(fun exn -> Eio.traceln "Connection error: %a" Fmt.exn exn)
        (fun flow addr -> handle_connection t flow addr ~clock);
      accept_loop ()
    in
    accept_loop ()

  (* Run server on TLS port - single process mode (no privilege separation) *)
  let run_tls t ~sw ~net ~addr ~tls_config ~clock ?(after_bind = fun () -> ()) () =
    let socket = Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:128 addr in
    after_bind ();
    let rec accept_loop () =
      Eio.Net.accept_fork socket ~sw
        ~on_error:(fun exn -> Eio.traceln "Connection error: %a" Fmt.exn exn)
        (fun flow addr ->
           let tls_flow = Tls_eio.server_of_flow tls_config flow in
           handle_connection_tls t tls_flow addr ~clock);
      accept_loop ()
    in
    accept_loop ()

  (* Drop privileges to the authenticated user *)
  let drop_to_user username =
    try
      let pw = Unix.getpwnam username in
      (* Set supplementary groups first *)
      Unix.initgroups username pw.Unix.pw_gid;
      (* Set GID before UID (can't change GID after dropping root) *)
      Unix.setgid pw.Unix.pw_gid;
      Unix.setuid pw.Unix.pw_uid;
      true
    with
    | Not_found -> false
    | Unix.Unix_error _ -> false

  (* Fork-based connection handler for privilege separation.
     Each connection runs in its own process as the authenticated user. *)
  let handle_connection_forked t flow _addr ~tls_active ~clock =
    send_greeting t flow ~tls_active;
    (* Authentication loop - runs as root *)
    let rec auth_loop () =
      match read_line flow with
      | None -> ()  (* Connection closed *)
      | Some line ->
        (* Debug: log received command *)
        Eio.traceln "IMAP < %s" (String.trim line);
        match parse_command line with
        | Error msg ->
          Eio.traceln "IMAP ! Parse error: %s" msg;
          send_response flow (Bad { tag = None; code = None; text = "Invalid command syntax" });
          auth_loop ()
        | Result.Ok cmd ->
          match cmd.command with
          | Login { username; password } ->
            if not (Imap_types.is_safe_username username) then begin
              send_response flow (No {
                tag = Some cmd.tag;
                code = Some Code_authenticationfailed;
                text = "LOGIN failed"
              });
              auth_loop ()
            end else if Auth.authenticate t.auth ~username ~password then begin
              (* Authentication succeeded - drop privileges to this user *)
              if drop_to_user username then begin
                let caps = all_capabilities t ~tls_active in
                send_response flow (Ok {
                  tag = Some cmd.tag;
                  code = Some (Code_capability caps);
                  text = "LOGIN completed"
                });
                (* Continue session as authenticated user *)
                let state = Authenticated { username } in
                ignore (command_loop t flow state ~tls_active ~clock)
              end else begin
                (* Failed to drop privileges *)
                send_response flow (No {
                  tag = Some cmd.tag;
                  code = Some Code_authenticationfailed;
                  text = "LOGIN failed"
                });
                auth_loop ()
              end
            end else begin
              send_response flow (No {
                tag = Some cmd.tag;
                code = Some Code_authenticationfailed;
                text = "LOGIN failed"
              });
              auth_loop ()
            end
          | Capability ->
            handle_capability t flow cmd.tag ~tls_active;
            auth_loop ()
          | Noop ->
            handle_noop flow cmd.tag;
            auth_loop ()
          | Id params ->
            handle_id flow cmd.tag params;
            auth_loop ()
          | Logout ->
            ignore (handle_logout flow cmd.tag);
            ()  (* Exit *)
          | Starttls ->
            (* STARTTLS not supported in forked mode - would need to pass TLS state *)
            send_response flow (Bad { tag = Some cmd.tag; code = None; text = "STARTTLS not supported in this mode" });
            auth_loop ()
          | _ ->
            send_response flow (Bad {
              tag = Some cmd.tag;
              code = None;
              text = "Please authenticate first"
            });
            auth_loop ()
    in
    auth_loop ()

  (* Run server with fork-per-connection privilege separation.
     Requires running as root. Each connection forks and drops to the authenticated user. *)
  let run_forked t ~sw:_ ~net:_ ~addr ~tls_config =
    (* Extract port and address for raw socket operations *)
    let port, bind_addr = match addr with
      | `Tcp (ip, port) ->
        let addr_str =
          if ip = Eio.Net.Ipaddr.V4.loopback then "127.0.0.1"
          else if ip = Eio.Net.Ipaddr.V4.any then "0.0.0.0"
          else
            (* Format IP address to string *)
            Format.asprintf "%a" Eio.Net.Ipaddr.pp ip
        in
        (port, Unix.inet_addr_of_string addr_str)
      | _ -> failwith "Only TCP addresses supported"
    in
    (* Create listening socket *)
    let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.setsockopt sock Unix.SO_REUSEADDR true;
    Unix.bind sock (Unix.ADDR_INET (bind_addr, port));
    Unix.listen sock 128;

    (* Reap zombie children *)
    Sys.set_signal Sys.sigchld (Sys.Signal_handle (fun _ ->
      try while fst (Unix.waitpid [Unix.WNOHANG] (-1)) > 0 do () done
      with Unix.Unix_error (Unix.ECHILD, _, _) -> ()
    ));

    (* Accept loop - handle EINTR from signal handlers *)
    let rec accept_with_retry () =
      try Unix.accept sock
      with Unix.Unix_error (Unix.EINTR, _, _) -> accept_with_retry ()
    in
    while true do
      let client_sock, _client_addr = accept_with_retry () in
      match Unix.fork () with
      | 0 ->
        (* Child process *)
        Unix.close sock;  (* Close listening socket in child *)
        (* Run EIO for this connection *)
        Eio_main.run @@ fun env ->
        let clock = Eio.Stdenv.clock env in
        Eio.Switch.run @@ fun sw ->
        let flow = Eio_unix.Net.import_socket_stream ~sw ~close_unix:true client_sock in
        (match tls_config with
         | None ->
           handle_connection_forked t flow () ~tls_active:false ~clock
         | Some tls_cfg ->
           let tls_flow = Tls_eio.server_of_flow tls_cfg flow in
           handle_connection_forked t (tls_flow :> _ Eio.Flow.two_way) () ~tls_active:true ~clock);
        exit 0
      | _pid ->
        (* Parent process *)
        Unix.close client_sock  (* Close client socket in parent *)
    done
end
