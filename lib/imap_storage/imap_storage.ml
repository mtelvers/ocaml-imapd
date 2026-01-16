(** IMAP Storage Backends

    Implements storage for {{:https://datatracker.ietf.org/doc/html/rfc9051}RFC 9051}. *)

open Imap_types

(* Storage errors *)
type error =
  | Mailbox_not_found
  | Mailbox_already_exists
  | Message_not_found
  | Permission_denied
  | Storage_error of string
  | Quota_exceeded

let error_to_string = function
  | Mailbox_not_found -> "Mailbox not found"
  | Mailbox_already_exists -> "Mailbox already exists"
  | Message_not_found -> "Message not found"
  | Permission_denied -> "Permission denied"
  | Storage_error msg -> Printf.sprintf "Storage error: %s" msg
  | Quota_exceeded -> "Quota exceeded"

(* Mailbox information *)
type mailbox_info = {
  name : mailbox_name;
  delimiter : char option;
  flags : list_flag list;
}

(* Storage backend signature *)
module type STORAGE = sig
  type t

  val create : unit -> t

  val list_mailboxes :
    t -> username:string -> reference:string -> pattern:string -> mailbox_info list

  val create_mailbox : t -> username:string -> mailbox_name -> (unit, error) result
  val delete_mailbox : t -> username:string -> mailbox_name -> (unit, error) result
  val rename_mailbox : t -> username:string -> old_name:mailbox_name -> new_name:mailbox_name -> (unit, error) result
  val select_mailbox : t -> username:string -> mailbox_name -> readonly:bool -> (mailbox_state, error) result
  val status_mailbox : t -> username:string -> mailbox_name -> items:status_item list -> ((status_item * int64) list, error) result

  val fetch_messages : t -> username:string -> mailbox:mailbox_name -> sequence:sequence_set -> items:fetch_item list -> (message list, error) result
  val fetch_by_uid : t -> username:string -> mailbox:mailbox_name -> uids:sequence_set -> items:fetch_item list -> (message list, error) result
  val store_flags : t -> username:string -> mailbox:mailbox_name -> sequence:sequence_set -> action:store_action -> flags:flag list -> (message list, error) result
  val expunge : t -> username:string -> mailbox:mailbox_name -> (uid list, error) result
  val append : t -> username:string -> mailbox:mailbox_name -> flags:flag list -> date:string option -> message:string -> (uid, error) result
  val copy : t -> username:string -> src_mailbox:mailbox_name -> sequence:sequence_set -> dst_mailbox:mailbox_name -> (uid list, error) result
  val move : t -> username:string -> src_mailbox:mailbox_name -> sequence:sequence_set -> dst_mailbox:mailbox_name -> (uid list, error) result
  val search : t -> username:string -> mailbox:mailbox_name -> criteria:search_key -> (uid list, error) result
end

(* ===== In-Memory Storage ===== *)

module Memory_storage = struct
  (* Internal mailbox representation *)
  type mailbox = {
    mutable messages : message list;
    mutable uidvalidity : uidvalidity;
    mutable uidnext : uid;
    mutable flags : flag list;
  }

  (* User data *)
  type user_data = {
    mutable mailboxes : (mailbox_name, mailbox) Hashtbl.t;
    mutable subscriptions : mailbox_name list;
  }

  type t = {
    users : (string, user_data) Hashtbl.t;
    mutable lock : unit;  (* Placeholder for future mutex *)
  }

  let create () = {
    users = Hashtbl.create 16;
    lock = ();
  }

  let get_user t ~username =
    match Hashtbl.find_opt t.users username with
    | Some u -> u
    | None ->
      let u = {
        mailboxes = Hashtbl.create 8;
        subscriptions = [];
      } in
      Hashtbl.add t.users username u;
      u

  let ensure_inbox user =
    if not (Hashtbl.mem user.mailboxes "INBOX") then begin
      let inbox = {
        messages = [];
        uidvalidity = Int32.of_float (Unix.time ());
        uidnext = 1l;
        flags = [System Seen; System Answered; System Flagged; System Deleted; System Draft];
      } in
      Hashtbl.add user.mailboxes "INBOX" inbox
    end

  let add_test_user t ~username =
    let user = get_user t ~username in
    ensure_inbox user

  let add_test_message t ~username ~mailbox ~message =
    let user = get_user t ~username in
    match Hashtbl.find_opt user.mailboxes mailbox with
    | Some mb ->
      mb.messages <- mb.messages @ [message];
      if message.uid >= mb.uidnext then
        mb.uidnext <- Int32.succ message.uid
    | None -> ()

  (* Pattern matching for LIST command *)
  let matches_pattern ~pattern name =
    if pattern = "*" then true
    else if pattern = "%" then not (String.contains name '/')
    else
      (* Simple prefix matching - full glob support would be more complex *)
      let pattern_len = String.length pattern in
      if pattern_len > 0 && pattern.[pattern_len - 1] = '*' then
        let prefix = String.sub pattern 0 (pattern_len - 1) in
        String.length name >= String.length prefix &&
        String.sub name 0 (String.length prefix) = prefix
      else
        name = pattern

  let list_mailboxes t ~username ~reference:_ ~pattern =
    let user = get_user t ~username in
    ensure_inbox user;
    Hashtbl.fold (fun name _mb acc ->
      if matches_pattern ~pattern name then
        { name; delimiter = Some '/'; flags = [] } :: acc
      else acc
    ) user.mailboxes []

  let create_mailbox t ~username name =
    let name = normalize_mailbox_name name in
    let user = get_user t ~username in
    if Hashtbl.mem user.mailboxes name then
      Result.Error Mailbox_already_exists
    else begin
      let mb = {
        messages = [];
        uidvalidity = Int32.of_float (Unix.time ());
        uidnext = 1l;
        flags = [System Seen; System Answered; System Flagged; System Deleted; System Draft];
      } in
      Hashtbl.add user.mailboxes name mb;
      Result.Ok ()
    end

  let delete_mailbox t ~username name =
    let name = normalize_mailbox_name name in
    if is_inbox name then
      Result.Error Permission_denied  (* Cannot delete INBOX *)
    else
      let user = get_user t ~username in
      if Hashtbl.mem user.mailboxes name then begin
        Hashtbl.remove user.mailboxes name;
        Result.Ok ()
      end else
        Result.Error Mailbox_not_found

  let rename_mailbox t ~username ~old_name ~new_name =
    let old_name = normalize_mailbox_name old_name in
    let new_name = normalize_mailbox_name new_name in
    if is_inbox old_name then
      Result.Error Permission_denied  (* Cannot rename INBOX directly *)
    else
      let user = get_user t ~username in
      match Hashtbl.find_opt user.mailboxes old_name with
      | None -> Result.Error Mailbox_not_found
      | Some mb ->
        if Hashtbl.mem user.mailboxes new_name then
          Result.Error Mailbox_already_exists
        else begin
          Hashtbl.remove user.mailboxes old_name;
          Hashtbl.add user.mailboxes new_name mb;
          Result.Ok ()
        end

  let select_mailbox t ~username name ~readonly =
    let name = normalize_mailbox_name name in
    let user = get_user t ~username in
    ensure_inbox user;
    match Hashtbl.find_opt user.mailboxes name with
    | None -> Result.Error Mailbox_not_found
    | Some mb ->
      Result.Ok {
        name;
        exists = List.length mb.messages;
        uidvalidity = mb.uidvalidity;
        uidnext = mb.uidnext;
        flags = mb.flags;
        permanent_flags = mb.flags;
        readonly;
      }

  let status_mailbox t ~username name ~items =
    let name = normalize_mailbox_name name in
    let user = get_user t ~username in
    match Hashtbl.find_opt user.mailboxes name with
    | None -> Result.Error Mailbox_not_found
    | Some mb ->
      let results = List.map (fun item ->
        let value = match item with
          | Status_messages -> Int64.of_int (List.length mb.messages)
          | Status_uidnext -> Int64.of_int32 mb.uidnext
          | Status_uidvalidity -> Int64.of_int32 mb.uidvalidity
          | Status_unseen ->
            Int64.of_int (List.length (List.filter (fun (m : message) ->
              not (List.mem (System Seen) m.flags)
            ) mb.messages))
          | Status_deleted ->
            Int64.of_int (List.length (List.filter (fun (m : message) ->
              List.mem (System Deleted) m.flags
            ) mb.messages))
          | Status_size ->
            List.fold_left (fun acc (m : message) -> Int64.add acc m.size) 0L mb.messages
        in
        (item, value)
      ) items in
      Result.Ok results

  (* Check if sequence number matches a range *)
  let seq_in_range seq range max_seq =
    match range with
    | Single n -> seq = n
    | Range (a, b) -> seq >= a && seq <= b
    | From n -> seq >= n && seq <= max_seq
    | All -> seq <= max_seq

  let seq_matches sequence seq max_seq =
    List.exists (fun range -> seq_in_range seq range max_seq) sequence

  let fetch_messages t ~username ~mailbox ~sequence ~items:_ =
    let mailbox = normalize_mailbox_name mailbox in
    let user = get_user t ~username in
    match Hashtbl.find_opt user.mailboxes mailbox with
    | None -> Result.Error Mailbox_not_found
    | Some mb ->
      let max_seq = List.length mb.messages in
      let results = List.filteri (fun i _ ->
        seq_matches sequence (i + 1) max_seq
      ) mb.messages in
      Result.Ok results

  let fetch_by_uid t ~username ~mailbox ~uids ~items:_ =
    let mailbox = normalize_mailbox_name mailbox in
    let user = get_user t ~username in
    match Hashtbl.find_opt user.mailboxes mailbox with
    | None -> Result.Error Mailbox_not_found
    | Some mb ->
      let uid_matches uid =
        List.exists (fun range ->
          match range with
          | Single n -> Int32.to_int uid = n
          | Range (a, b) -> Int32.to_int uid >= a && Int32.to_int uid <= b
          | From n -> Int32.to_int uid >= n
          | All -> true
        ) uids
      in
      let results = List.filter (fun m -> uid_matches m.uid) mb.messages in
      Result.Ok results

  let apply_flags_action action existing new_flags =
    match action with
    | Store_set -> new_flags
    | Store_add ->
      List.fold_left (fun acc f ->
        if List.mem f acc then acc else f :: acc
      ) existing new_flags
    | Store_remove ->
      List.filter (fun f -> not (List.mem f new_flags)) existing

  let store_flags t ~username ~mailbox ~sequence ~action ~flags =
    let mailbox = normalize_mailbox_name mailbox in
    let user = get_user t ~username in
    match Hashtbl.find_opt user.mailboxes mailbox with
    | None -> Result.Error Mailbox_not_found
    | Some mb ->
      let max_seq = List.length mb.messages in
      let updated = List.mapi (fun i (m : message) ->
        if seq_matches sequence (i + 1) max_seq then
          { m with flags = apply_flags_action action m.flags flags }
        else m
      ) mb.messages in
      mb.messages <- updated;
      let results = List.filteri (fun i _ ->
        seq_matches sequence (i + 1) max_seq
      ) updated in
      Result.Ok results

  let expunge t ~username ~mailbox =
    let mailbox = normalize_mailbox_name mailbox in
    let user = get_user t ~username in
    match Hashtbl.find_opt user.mailboxes mailbox with
    | None -> Result.Error Mailbox_not_found
    | Some mb ->
      let deleted, remaining = List.partition (fun (m : message) ->
        List.mem (System Deleted) m.flags
      ) mb.messages in
      mb.messages <- remaining;
      (* Renumber remaining messages *)
      mb.messages <- List.mapi (fun i (m : message) -> { m with seq = i + 1 }) mb.messages;
      Result.Ok (List.map (fun (m : message) -> m.uid) deleted)

  let current_date () =
    let tm = Unix.gmtime (Unix.time ()) in
    Printf.sprintf "%02d-%s-%04d %02d:%02d:%02d +0000"
      tm.Unix.tm_mday
      (match tm.Unix.tm_mon with
       | 0 -> "Jan" | 1 -> "Feb" | 2 -> "Mar" | 3 -> "Apr"
       | 4 -> "May" | 5 -> "Jun" | 6 -> "Jul" | 7 -> "Aug"
       | 8 -> "Sep" | 9 -> "Oct" | 10 -> "Nov" | _ -> "Dec")
      (1900 + tm.Unix.tm_year)
      tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec

  let append t ~username ~mailbox ~flags ~date ~message =
    let mailbox = normalize_mailbox_name mailbox in
    let user = get_user t ~username in
    ensure_inbox user;
    match Hashtbl.find_opt user.mailboxes mailbox with
    | None -> Result.Error Mailbox_not_found
    | Some mb ->
      let uid = mb.uidnext in
      mb.uidnext <- Int32.succ mb.uidnext;
      let msg = {
        uid;
        seq = List.length mb.messages + 1;
        flags;
        internal_date = (match date with Some d -> d | None -> current_date ());
        size = Int64.of_int (String.length message);
        envelope = None;
        body_structure = None;
        raw_headers = None;
        raw_body = Some message;
      } in
      mb.messages <- mb.messages @ [msg];
      Result.Ok uid

  let copy t ~username ~src_mailbox ~sequence ~dst_mailbox =
    let src_mailbox = normalize_mailbox_name src_mailbox in
    let dst_mailbox = normalize_mailbox_name dst_mailbox in
    let user = get_user t ~username in
    match Hashtbl.find_opt user.mailboxes src_mailbox,
          Hashtbl.find_opt user.mailboxes dst_mailbox with
    | None, _ -> Result.Error Mailbox_not_found
    | _, None -> Result.Error Mailbox_not_found
    | Some src_mb, Some dst_mb ->
      let max_seq = List.length src_mb.messages in
      let to_copy = List.filteri (fun i _ ->
        seq_matches sequence (i + 1) max_seq
      ) src_mb.messages in
      let new_uids = List.map (fun m ->
        let uid = dst_mb.uidnext in
        dst_mb.uidnext <- Int32.succ dst_mb.uidnext;
        let new_msg = {
          m with
          uid;
          seq = List.length dst_mb.messages + 1;
        } in
        dst_mb.messages <- dst_mb.messages @ [new_msg];
        uid
      ) to_copy in
      Result.Ok new_uids

  let move t ~username ~src_mailbox ~sequence ~dst_mailbox =
    match copy t ~username ~src_mailbox ~sequence ~dst_mailbox with
    | Result.Error e -> Result.Error e
    | Result.Ok uids ->
      (* Mark source messages as deleted and expunge *)
      let src_mailbox = normalize_mailbox_name src_mailbox in
      let user = get_user t ~username in
      (match Hashtbl.find_opt user.mailboxes src_mailbox with
       | None -> ()
       | Some src_mb ->
         let max_seq = List.length src_mb.messages in
         src_mb.messages <- List.filteri (fun i _ ->
           not (seq_matches sequence (i + 1) max_seq)
         ) src_mb.messages;
         src_mb.messages <- List.mapi (fun i (m : message) -> { m with seq = i + 1 }) src_mb.messages);
      Result.Ok uids

  let search t ~username ~mailbox ~criteria =
    let mailbox = normalize_mailbox_name mailbox in
    let user = get_user t ~username in
    match Hashtbl.find_opt user.mailboxes mailbox with
    | None -> Result.Error Mailbox_not_found
    | Some mb ->
      let rec matches (m : message) = function
        | Search_all -> true
        | Search_seen -> List.mem (System Seen) m.flags
        | Search_unseen -> not (List.mem (System Seen) m.flags)
        | Search_answered -> List.mem (System Answered) m.flags
        | Search_unanswered -> not (List.mem (System Answered) m.flags)
        | Search_deleted -> List.mem (System Deleted) m.flags
        | Search_undeleted -> not (List.mem (System Deleted) m.flags)
        | Search_flagged -> List.mem (System Flagged) m.flags
        | Search_unflagged -> not (List.mem (System Flagged) m.flags)
        | Search_draft -> List.mem (System Draft) m.flags
        | Search_new -> not (List.mem (System Seen) m.flags)  (* Simplified *)
        | Search_old -> List.mem (System Seen) m.flags  (* Simplified *)
        | Search_not k -> not (matches m k)
        | Search_or (k1, k2) -> matches m k1 || matches m k2
        | Search_and ks -> List.for_all (matches m) ks
        | Search_larger n -> m.size > n
        | Search_smaller n -> m.size < n
        | Search_uid seqs ->
          List.exists (fun range ->
            match range with
            | Single n -> Int32.to_int m.uid = n
            | Range (a, b) -> Int32.to_int m.uid >= a && Int32.to_int m.uid <= b
            | From n -> Int32.to_int m.uid >= n
            | All -> true
          ) seqs
        | _ -> true  (* TODO: Implement remaining search keys *)
      in
      let results = List.filter_map (fun (m : message) ->
        if matches m criteria then Some m.uid else None
      ) mb.messages in
      Result.Ok results
end

(* ===== Maildir Storage ===== *)

(** Maildir storage backend.

    Implements the Maildir format as specified by D.J. Bernstein.
    See {{:https://cr.yp.to/proto/maildir.html}Maildir specification}.

    Directory structure:
    - {i base_path}/{i username}/ - User's INBOX
    - {i base_path}/{i username}/.{i folder}/ - Subfolders

    Each mailbox contains:
    - cur/ - Messages that have been seen by MUA
    - new/ - Newly delivered messages
    - tmp/ - Temporary files during delivery

    Filename format: {i unique}:2,{i flags}
    - unique: {i timestamp}.{i pid}.{i hostname}
    - flags: D=Draft, F=Flagged, R=Replied, S=Seen, T=Trashed *)

module Maildir_storage = struct
  type path_mode =
    | Shared_base of string  (* Shared base path: /var/mail/<username> *)
    | Home_directory          (* User home: ~<username>/Maildir *)

  type t = {
    path_mode : path_mode;
    hostname : string;
    mutable delivery_counter : int;
  }

  (* Helper: check if string ends with suffix *)
  let ends_with ~suffix s =
    let len = String.length suffix in
    String.length s >= len && String.sub s (String.length s - len) len = suffix

  (* UID mapping file stores: filename -> uid *)
  type uid_map = {
    mutable next_uid : int32;
    mutable uidvalidity : int32;
    entries : (string, int32) Hashtbl.t;  (* filename -> uid *)
  }

  let create () = {
    path_mode = Home_directory;
    hostname = Unix.gethostname ();
    delivery_counter = 0;
  }

  let create_with_path ~base_path = {
    path_mode = Shared_base base_path;
    hostname = Unix.gethostname ();
    delivery_counter = 0;
  }

  let create_home_directory () = {
    path_mode = Home_directory;
    hostname = Unix.gethostname ();
    delivery_counter = 0;
  }

  let user_path t ~username =
    match t.path_mode with
    | Shared_base base_path -> Filename.concat base_path username
    | Home_directory ->
      try
        let pw = Unix.getpwnam username in
        Filename.concat pw.Unix.pw_dir "Maildir"
      with Not_found ->
        (* Fallback if user lookup fails *)
        Filename.concat "/var/mail" username

  (* Recursive mkdir -p - declared early for ensure_user *)
  let rec mkdir_p path =
    if not (Sys.file_exists path) then begin
      mkdir_p (Filename.dirname path);
      try Sys.mkdir path 0o700 with Sys_error _ -> ()
    end

  let ensure_maildir_structure path =
    mkdir_p path;
    mkdir_p (Filename.concat path "cur");
    mkdir_p (Filename.concat path "new");
    mkdir_p (Filename.concat path "tmp")

  (* Ensure user's INBOX exists *)
  let ensure_user t ~username =
    let inbox_path = user_path t ~username in
    ensure_maildir_structure inbox_path

  let mailbox_path t ~username ~mailbox =
    let base = user_path t ~username in
    if is_inbox mailbox then base
    else Filename.concat base ("." ^ String.map (fun c -> if c = '/' then '.' else c) mailbox)

  (* UID map file - use a name that won't be confused with a mailbox *)
  let uid_map_path path = Filename.concat path ".imapd-uidmap"

  (* Parse Maildir flags from filename info part *)
  let parse_maildir_flags info =
    let flags = ref [] in
    String.iter (fun c ->
      match c with
      | 'S' -> flags := System Seen :: !flags
      | 'R' -> flags := System Answered :: !flags
      | 'F' -> flags := System Flagged :: !flags
      | 'T' -> flags := System Deleted :: !flags
      | 'D' -> flags := System Draft :: !flags
      | _ -> ()
    ) info;
    !flags

  (* Convert IMAP flags to Maildir flag string *)
  let flags_to_maildir_string flags =
    let buf = Buffer.create 8 in
    (* Maildir flags must be in ASCII order: DFPRST *)
    if List.mem (System Draft) flags then Buffer.add_char buf 'D';
    if List.mem (System Flagged) flags then Buffer.add_char buf 'F';
    (* P = Passed (forwarded), not in IMAP *)
    if List.mem (System Answered) flags then Buffer.add_char buf 'R';
    if List.mem (System Seen) flags then Buffer.add_char buf 'S';
    if List.mem (System Deleted) flags then Buffer.add_char buf 'T';
    Buffer.contents buf

  (* Parse filename to extract base name and flags *)
  let parse_filename filename =
    match String.index_opt filename ':' with
    | None -> (filename, [])
    | Some colon_pos ->
      let base = String.sub filename 0 colon_pos in
      let rest = String.sub filename (colon_pos + 1) (String.length filename - colon_pos - 1) in
      if String.length rest >= 2 && rest.[0] = '2' && rest.[1] = ',' then
        let info = String.sub rest 2 (String.length rest - 2) in
        (base, parse_maildir_flags info)
      else
        (base, [])

  (* Build filename with flags *)
  let build_filename base flags =
    let flag_str = flags_to_maildir_string flags in
    if flag_str = "" then base ^ ":2,"
    else base ^ ":2," ^ flag_str

  (* Generate unique filename for new message *)
  let generate_unique_name t =
    let timestamp = Unix.gettimeofday () in
    let sec = int_of_float timestamp in
    let usec = int_of_float ((timestamp -. float_of_int sec) *. 1000000.0) in
    t.delivery_counter <- t.delivery_counter + 1;
    Printf.sprintf "%d.M%dP%dQ%d.%s" sec usec (Unix.getpid ()) t.delivery_counter t.hostname

  (* Load or create UID map for a mailbox *)
  let load_uid_map path =
    let map_file = uid_map_path path in
    if Sys.file_exists map_file then begin
      let ic = open_in_bin map_file in
      try
        let next_uid = Int32.of_string (input_line ic) in
        let uidvalidity = Int32.of_string (input_line ic) in
        let entries = Hashtbl.create 256 in
        (try
          while true do
            let line = input_line ic in
            match String.index_opt line '\t' with
            | Some tab ->
              let filename = String.sub line 0 tab in
              let uid = Int32.of_string (String.sub line (tab + 1) (String.length line - tab - 1)) in
              Hashtbl.add entries filename uid
            | None -> ()
          done
        with End_of_file -> ());
        close_in ic;
        { next_uid; uidvalidity; entries }
      with _ ->
        close_in ic;
        { next_uid = 1l; uidvalidity = Int32.of_float (Unix.time ()); entries = Hashtbl.create 256 }
    end else
      { next_uid = 1l; uidvalidity = Int32.of_float (Unix.time ()); entries = Hashtbl.create 256 }

  let save_uid_map path map =
    let map_file = uid_map_path path in
    let oc = open_out_bin map_file in
    Printf.fprintf oc "%ld\n%ld\n" map.next_uid map.uidvalidity;
    Hashtbl.iter (fun filename uid ->
      Printf.fprintf oc "%s\t%ld\n" filename uid
    ) map.entries;
    close_out oc

  (* Get or assign UID for a filename *)
  let get_or_assign_uid map filename =
    let base, _ = parse_filename filename in
    match Hashtbl.find_opt map.entries base with
    | Some uid -> uid
    | None ->
      let uid = map.next_uid in
      map.next_uid <- Int32.succ map.next_uid;
      Hashtbl.add map.entries base uid;
      uid

  (* List all messages in a maildir *)
  let list_messages path =
    let messages = ref [] in
    let add_from_dir subdir in_new =
      let dir = Filename.concat path subdir in
      if Sys.file_exists dir then begin
        let entries = Sys.readdir dir in
        Array.iter (fun filename ->
          if filename.[0] <> '.' then
            messages := (Filename.concat dir filename, filename, in_new) :: !messages
        ) entries
      end
    in
    add_from_dir "new" true;
    add_from_dir "cur" false;
    !messages

  (* Read message from file *)
  let read_message_file filepath =
    try
      let ic = open_in_bin filepath in
      let size = in_channel_length ic in
      let content = really_input_string ic size in
      close_in ic;
      Some content
    with _ -> None

  (* Parse internal date from message or use file mtime *)
  let get_internal_date filepath =
    try
      let stats = Unix.stat filepath in
      let tm = Unix.gmtime stats.Unix.st_mtime in
      Printf.sprintf "%02d-%s-%04d %02d:%02d:%02d +0000"
        tm.Unix.tm_mday
        (match tm.Unix.tm_mon with
         | 0 -> "Jan" | 1 -> "Feb" | 2 -> "Mar" | 3 -> "Apr"
         | 4 -> "May" | 5 -> "Jun" | 6 -> "Jul" | 7 -> "Aug"
         | 8 -> "Sep" | 9 -> "Oct" | 10 -> "Nov" | _ -> "Dec")
        (1900 + tm.Unix.tm_year)
        tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
    with _ -> Memory_storage.current_date ()

  (* Check if a file is a metadata file that should not be listed as a mailbox *)
  let is_metadata_file name =
    ends_with ~suffix:"-uidmap" name ||
    ends_with ~suffix:".uidmap" name ||
    name = ".imapd-uidmap" ||
    name = ".subscriptions"

  let list_mailboxes t ~username ~reference:_ ~pattern =
    let base = user_path t ~username in
    if not (Sys.file_exists base) then
      (* Create user directory with INBOX *)
      (ensure_maildir_structure base;
       if Memory_storage.matches_pattern ~pattern "INBOX" then
         [{ name = "INBOX"; delimiter = Some '/'; flags = [] }]
       else [])
    else begin
      let entries = Sys.readdir base in
      let mailboxes = Array.fold_left (fun acc entry ->
        if String.length entry > 0 && entry.[0] = '.' && entry <> ".." then
          (* Skip hidden files that aren't mailboxes *)
          if String.length entry > 1 && entry.[1] <> '.' then
            (* Skip metadata files *)
            if is_metadata_file entry then acc
            else
              (* Only include directories (mailboxes), not regular files *)
              let entry_path = Filename.concat base entry in
              if Sys.is_directory entry_path then
                let name = String.sub entry 1 (String.length entry - 1) in
                let name = String.map (fun c -> if c = '.' then '/' else c) name in
                if Memory_storage.matches_pattern ~pattern name then
                  { name; delimiter = Some '/'; flags = [] } :: acc
                else acc
              else acc
          else acc
        else acc
      ) [] entries in
      (* Always include INBOX if it matches *)
      if Memory_storage.matches_pattern ~pattern "INBOX" then
        { name = "INBOX"; delimiter = Some '/'; flags = [] } :: mailboxes
      else mailboxes
    end

  let create_mailbox t ~username name =
    let name = normalize_mailbox_name name in
    let path = mailbox_path t ~username ~mailbox:name in
    if Sys.file_exists path then
      Result.Error Mailbox_already_exists
    else begin
      try
        ensure_maildir_structure path;
        Result.Ok ()
      with Sys_error msg -> Result.Error (Storage_error msg)
    end

  let delete_mailbox t ~username name =
    let name = normalize_mailbox_name name in
    if is_inbox name then
      Result.Error Permission_denied
    else
      let path = mailbox_path t ~username ~mailbox:name in
      if not (Sys.file_exists path) then
        Result.Error Mailbox_not_found
      else begin
        try
          (* Remove all files in subdirectories first *)
          List.iter (fun subdir ->
            let dir = Filename.concat path subdir in
            if Sys.file_exists dir then begin
              Array.iter (fun f ->
                Sys.remove (Filename.concat dir f)
              ) (Sys.readdir dir);
              Sys.rmdir dir
            end
          ) ["cur"; "new"; "tmp"];
          (* Remove UID map if exists *)
          let uid_file = uid_map_path path in
          if Sys.file_exists uid_file then Sys.remove uid_file;
          (* Remove mailbox directory *)
          Sys.rmdir path;
          Result.Ok ()
        with Sys_error msg -> Result.Error (Storage_error msg)
      end

  let rename_mailbox t ~username ~old_name ~new_name =
    let old_name = normalize_mailbox_name old_name in
    let new_name = normalize_mailbox_name new_name in
    if is_inbox old_name then
      Result.Error Permission_denied
    else
      let old_path = mailbox_path t ~username ~mailbox:old_name in
      let new_path = mailbox_path t ~username ~mailbox:new_name in
      if not (Sys.file_exists old_path) then
        Result.Error Mailbox_not_found
      else if Sys.file_exists new_path then
        Result.Error Mailbox_already_exists
      else begin
        try
          Sys.rename old_path new_path;
          Result.Ok ()
        with Sys_error msg -> Result.Error (Storage_error msg)
      end

  let select_mailbox t ~username name ~readonly =
    let name = normalize_mailbox_name name in
    let path = mailbox_path t ~username ~mailbox:name in
    if not (Sys.file_exists path) then begin
      (* Auto-create INBOX *)
      if is_inbox name then begin
        ensure_maildir_structure path;
        let map = load_uid_map path in
        save_uid_map path map;
        Result.Ok {
          name;
          exists = 0;
          uidvalidity = map.uidvalidity;
          uidnext = map.next_uid;
          flags = [System Seen; System Answered; System Flagged; System Deleted; System Draft];
          permanent_flags = [System Seen; System Answered; System Flagged; System Deleted; System Draft];
          readonly;
        }
      end else
        Result.Error Mailbox_not_found
    end else begin
      let messages = list_messages path in
      let map = load_uid_map path in
      (* Assign UIDs to any new messages *)
      List.iter (fun (_, filename, _) ->
        ignore (get_or_assign_uid map filename)
      ) messages;
      save_uid_map path map;
      Result.Ok {
        name;
        exists = List.length messages;
        uidvalidity = map.uidvalidity;
        uidnext = map.next_uid;
        flags = [System Seen; System Answered; System Flagged; System Deleted; System Draft];
        permanent_flags = [System Seen; System Answered; System Flagged; System Deleted; System Draft];
        readonly;
      }
    end

  let status_mailbox t ~username name ~items =
    let name = normalize_mailbox_name name in
    let path = mailbox_path t ~username ~mailbox:name in
    if not (Sys.file_exists path) then
      Result.Error Mailbox_not_found
    else begin
      let messages = list_messages path in
      let map = load_uid_map path in
      let total_size = ref 0L in
      let unseen_count = ref 0 in
      List.iter (fun (filepath, filename, in_new) ->
        let _, flags = parse_filename filename in
        if in_new || not (List.mem (System Seen) flags) then
          incr unseen_count;
        try
          let stats = Unix.stat filepath in
          total_size := Int64.add !total_size (Int64.of_int stats.Unix.st_size)
        with _ -> ()
      ) messages;
      let results = List.map (fun item ->
        let value = match item with
          | Status_messages -> Int64.of_int (List.length messages)
          | Status_uidnext -> Int64.of_int32 map.next_uid
          | Status_uidvalidity -> Int64.of_int32 map.uidvalidity
          | Status_unseen -> Int64.of_int !unseen_count
          | Status_deleted -> 0L  (* Would need to scan for T flag *)
          | Status_size -> !total_size
        in
        (item, value)
      ) items in
      Result.Ok results
    end

  let fetch_messages t ~username ~mailbox ~sequence ~items:_ =
    let mailbox = normalize_mailbox_name mailbox in
    let path = mailbox_path t ~username ~mailbox in
    if not (Sys.file_exists path) then
      Result.Error Mailbox_not_found
    else begin
      let messages = list_messages path in
      let map = load_uid_map path in
      let max_seq = List.length messages in
      let results = List.mapi (fun i (filepath, filename, _in_new) ->
        let seq = i + 1 in
        if Memory_storage.seq_matches sequence seq max_seq then begin
          let _base, flags = parse_filename filename in
          let uid = get_or_assign_uid map filename in
          let size = try
            let stats = Unix.stat filepath in
            Int64.of_int stats.Unix.st_size
          with _ -> 0L in
          Some {
            uid;
            seq;
            flags;
            internal_date = get_internal_date filepath;
            size;
            envelope = None;
            body_structure = None;
            raw_headers = None;
            raw_body = (match read_message_file filepath with Some s -> Some s | None -> None);
          }
        end else None
      ) messages in
      save_uid_map path map;
      Result.Ok (List.filter_map Fun.id results)
    end

  let fetch_by_uid t ~username ~mailbox ~uids ~items:_ =
    let mailbox = normalize_mailbox_name mailbox in
    let path = mailbox_path t ~username ~mailbox in
    if not (Sys.file_exists path) then
      Result.Error Mailbox_not_found
    else begin
      let messages = list_messages path in
      let map = load_uid_map path in
      let uid_matches uid =
        List.exists (fun range ->
          match range with
          | Single n -> Int32.to_int uid = n
          | Range (a, b) -> Int32.to_int uid >= a && Int32.to_int uid <= b
          | From n -> Int32.to_int uid >= n
          | All -> true
        ) uids
      in
      let results = List.mapi (fun i (filepath, filename, _in_new) ->
        let uid = get_or_assign_uid map filename in
        if uid_matches uid then begin
          let _, flags = parse_filename filename in
          let size = try
            let stats = Unix.stat filepath in
            Int64.of_int stats.Unix.st_size
          with _ -> 0L in
          Some {
            uid;
            seq = i + 1;
            flags;
            internal_date = get_internal_date filepath;
            size;
            envelope = None;
            body_structure = None;
            raw_headers = None;
            raw_body = read_message_file filepath;
          }
        end else None
      ) messages in
      save_uid_map path map;
      Result.Ok (List.filter_map Fun.id results)
    end

  let store_flags t ~username ~mailbox ~sequence ~action ~flags =
    let mailbox = normalize_mailbox_name mailbox in
    let path = mailbox_path t ~username ~mailbox in
    if not (Sys.file_exists path) then
      Result.Error Mailbox_not_found
    else begin
      let messages = list_messages path in
      let map = load_uid_map path in
      let max_seq = List.length messages in
      let results = List.mapi (fun i (filepath, filename, _in_new) ->
        let seq = i + 1 in
        if Memory_storage.seq_matches sequence seq max_seq then begin
          let base, old_flags = parse_filename filename in
          let new_flags = Memory_storage.apply_flags_action action old_flags flags in
          let uid = get_or_assign_uid map filename in
          (* Rename file with new flags *)
          let new_filename = build_filename base new_flags in
          let dir = Filename.dirname filepath in
          (* Move to cur/ if in new/ *)
          let new_dir = if ends_with ~suffix:"/new" dir then
            String.sub dir 0 (String.length dir - 4) ^ "/cur"
          else dir in
          let new_filepath = Filename.concat new_dir new_filename in
          (try
            if filepath <> new_filepath then
              Sys.rename filepath new_filepath
          with _ -> ());
          let size = try
            let stats = Unix.stat new_filepath in
            Int64.of_int stats.Unix.st_size
          with _ -> 0L in
          Some {
            uid;
            seq;
            flags = new_flags;
            internal_date = get_internal_date new_filepath;
            size;
            envelope = None;
            body_structure = None;
            raw_headers = None;
            raw_body = None;
          }
        end else None
      ) messages in
      save_uid_map path map;
      Result.Ok (List.filter_map Fun.id results)
    end

  let expunge t ~username ~mailbox =
    let mailbox = normalize_mailbox_name mailbox in
    let path = mailbox_path t ~username ~mailbox in
    if not (Sys.file_exists path) then
      Result.Error Mailbox_not_found
    else begin
      let messages = list_messages path in
      let map = load_uid_map path in
      let deleted_uids = ref [] in
      List.iter (fun (filepath, filename, _) ->
        let base, flags = parse_filename filename in
        if List.mem (System Deleted) flags then begin
          let uid = get_or_assign_uid map filename in
          deleted_uids := uid :: !deleted_uids;
          (* Remove from UID map *)
          Hashtbl.remove map.entries base;
          (* Delete file *)
          (try Sys.remove filepath with _ -> ())
        end
      ) messages;
      save_uid_map path map;
      Result.Ok (List.rev !deleted_uids)
    end

  let append t ~username ~mailbox ~flags ~date:_ ~message =
    let mailbox = normalize_mailbox_name mailbox in
    let path = mailbox_path t ~username ~mailbox in
    (* Create INBOX if it doesn't exist, error for other mailboxes *)
    if not (Sys.file_exists path) then begin
      if is_inbox mailbox then
        ensure_maildir_structure path
      else
        (* Return error for non-existent non-INBOX mailboxes *)
        ()
    end;
    if not (Sys.file_exists path) then
      Result.Error Mailbox_not_found
    else
      try
        let map = load_uid_map path in
        let unique_name = generate_unique_name t in
        let filename = build_filename unique_name flags in
        (* Write to tmp first, then move to new or cur *)
        let tmp_path = Filename.concat (Filename.concat path "tmp") filename in
        let oc = open_out_bin tmp_path in
        output_string oc message;
        close_out oc;
        (* Move to cur if Seen flag set, otherwise new *)
        let dest_dir = if List.mem (System Seen) flags then "cur" else "new" in
        let dest_path = Filename.concat (Filename.concat path dest_dir) filename in
        Sys.rename tmp_path dest_path;
        (* Assign UID *)
        let uid = map.next_uid in
        map.next_uid <- Int32.succ map.next_uid;
        Hashtbl.add map.entries unique_name uid;
        save_uid_map path map;
        Result.Ok uid
      with Sys_error msg -> Result.Error (Storage_error msg)

  let copy t ~username ~src_mailbox ~sequence ~dst_mailbox =
    let src_mailbox = normalize_mailbox_name src_mailbox in
    let dst_mailbox = normalize_mailbox_name dst_mailbox in
    let src_path = mailbox_path t ~username ~mailbox:src_mailbox in
    let dst_path = mailbox_path t ~username ~mailbox:dst_mailbox in
    if not (Sys.file_exists src_path) then
      Result.Error Mailbox_not_found
    else if not (Sys.file_exists dst_path) then
      Result.Error Mailbox_not_found
    else begin
      let messages = list_messages src_path in
      let src_map = load_uid_map src_path in
      let dst_map = load_uid_map dst_path in
      let max_seq = List.length messages in
      let new_uids = ref [] in
      List.iteri (fun i (filepath, filename, _) ->
        let seq = i + 1 in
        if Memory_storage.seq_matches sequence seq max_seq then begin
          match read_message_file filepath with
          | Some content ->
            let _, flags = parse_filename filename in
            let unique_name = generate_unique_name t in
            let new_filename = build_filename unique_name flags in
            let dest_dir = if List.mem (System Seen) flags then "cur" else "new" in
            let dest_path = Filename.concat (Filename.concat dst_path dest_dir) new_filename in
            let oc = open_out_bin dest_path in
            output_string oc content;
            close_out oc;
            let uid = dst_map.next_uid in
            dst_map.next_uid <- Int32.succ dst_map.next_uid;
            Hashtbl.add dst_map.entries unique_name uid;
            new_uids := uid :: !new_uids
          | None -> ()
        end
      ) messages;
      save_uid_map src_path src_map;
      save_uid_map dst_path dst_map;
      Result.Ok (List.rev !new_uids)
    end

  let move t ~username ~src_mailbox ~sequence ~dst_mailbox =
    match copy t ~username ~src_mailbox ~sequence ~dst_mailbox with
    | Result.Error e -> Result.Error e
    | Result.Ok new_uids ->
      (* Mark source messages as deleted and expunge *)
      let src_mailbox = normalize_mailbox_name src_mailbox in
      let src_path = mailbox_path t ~username ~mailbox:src_mailbox in
      let messages = list_messages src_path in
      let max_seq = List.length messages in
      List.iteri (fun i (filepath, _filename, _) ->
        let seq = i + 1 in
        if Memory_storage.seq_matches sequence seq max_seq then
          (try Sys.remove filepath with _ -> ())
      ) messages;
      Result.Ok new_uids

  let search t ~username ~mailbox ~criteria =
    let mailbox = normalize_mailbox_name mailbox in
    let path = mailbox_path t ~username ~mailbox in
    if not (Sys.file_exists path) then
      Result.Error Mailbox_not_found
    else begin
      let messages = list_messages path in
      let map = load_uid_map path in
      let rec matches (filepath, filename, in_new) = function
        | Search_all -> true
        | Search_seen ->
          let _, flags = parse_filename filename in
          not in_new && List.mem (System Seen) flags
        | Search_unseen ->
          let _, flags = parse_filename filename in
          in_new || not (List.mem (System Seen) flags)
        | Search_flagged ->
          let _, flags = parse_filename filename in
          List.mem (System Flagged) flags
        | Search_unflagged ->
          let _, flags = parse_filename filename in
          not (List.mem (System Flagged) flags)
        | Search_deleted ->
          let _, flags = parse_filename filename in
          List.mem (System Deleted) flags
        | Search_undeleted ->
          let _, flags = parse_filename filename in
          not (List.mem (System Deleted) flags)
        | Search_answered ->
          let _, flags = parse_filename filename in
          List.mem (System Answered) flags
        | Search_unanswered ->
          let _, flags = parse_filename filename in
          not (List.mem (System Answered) flags)
        | Search_draft ->
          let _, flags = parse_filename filename in
          List.mem (System Draft) flags
        | Search_not k -> not (matches (filepath, filename, in_new) k)
        | Search_or (k1, k2) ->
          matches (filepath, filename, in_new) k1 || matches (filepath, filename, in_new) k2
        | Search_and ks -> List.for_all (matches (filepath, filename, in_new)) ks
        | Search_larger n ->
          (try
            let stats = Unix.stat filepath in
            Int64.of_int stats.Unix.st_size > n
          with _ -> false)
        | Search_smaller n ->
          (try
            let stats = Unix.stat filepath in
            Int64.of_int stats.Unix.st_size < n
          with _ -> false)
        | Search_uid seqs ->
          let uid = get_or_assign_uid map filename in
          List.exists (fun range ->
            match range with
            | Single n -> Int32.to_int uid = n
            | Range (a, b) -> Int32.to_int uid >= a && Int32.to_int uid <= b
            | From n -> Int32.to_int uid >= n
            | All -> true
          ) seqs
        | _ -> true  (* Simplified: other criteria match all *)
      in
      let results = List.filter_map (fun (filepath, filename, in_new) ->
        if matches (filepath, filename, in_new) criteria then
          Some (get_or_assign_uid map filename)
        else None
      ) messages in
      save_uid_map path map;
      Result.Ok results
    end
end
