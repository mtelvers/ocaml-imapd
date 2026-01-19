(** IMAP Grammar - RFC 9051 Section 9 ABNF

    Menhir grammar for IMAP4rev2 protocol parsing.
    See {{:https://datatracker.ietf.org/doc/html/rfc9051#section-9}RFC 9051 Section 9}. *)

%{
open Imap_types
%}

(* Tokens *)
%token EOF
%token SP CRLF
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE
%token STAR PLUS MINUS COLON COMMA DOT LANGLE RANGLE

(* Literals and strings *)
%token <int64> NUMBER
%token <string> ATOM
%token <string> QUOTED_STRING
%token <int64> LITERAL_START
%token <int64> LITERAL_START_PLUS
%token NIL

(* System flags *)
%token FLAG_SEEN FLAG_ANSWERED FLAG_FLAGGED FLAG_DELETED FLAG_DRAFT FLAG_RECENT
%token <string> FLAG_EXTENSION
%token FLAG_PERM_ALL

(* Commands - Any state *)
%token CAPABILITY NOOP LOGOUT ID

(* Commands - Not authenticated *)
%token STARTTLS LOGIN AUTHENTICATE

(* Commands - Authenticated *)
%token ENABLE SELECT EXAMINE CREATE DELETE RENAME
%token SUBSCRIBE UNSUBSCRIBE LIST NAMESPACE STATUS APPEND IDLE

(* Commands - Selected *)
%token CLOSE UNSELECT EXPUNGE SEARCH FETCH STORE COPY MOVE UID

(* Fetch attributes *)
%token ENVELOPE FLAGS INTERNALDATE RFC822 BODY BODYSTRUCTURE BINARY
%token ALL FAST FULL

(* Status attributes *)
%token MESSAGES UIDNEXT UIDVALIDITY UNSEEN DELETED_STATUS SIZE

(* Search keys *)
%token ANSWERED BCC BEFORE CC DRAFT FLAGGED FROM
%token HEADER KEYWORD LARGER NEW NOT OLD ON OR
%token SEEN SENTBEFORE SENTON SENTSINCE SINCE SMALLER
%token SUBJECT TEXT TO UNANSWERED UNDELETED UNDRAFT UNFLAGGED UNKEYWORD

(* Other keywords *)
%token CHARSET MIME PEEK HEADER_FIELDS HEADER_FIELDS_NOT SILENT
%token RETURN SUBSCRIBED CHILDREN REMOTE RECURSIVEMATCH DONE

(* Entry point *)
%start <Imap_types.tagged_command> command
%start <Imap_types.response> response_parser

%%

(* === Basic types === *)

(* astring = 1*ASTRING-CHAR / string *)
astring:
  | s = ATOM { s }
  | s = QUOTED_STRING { s }
  ;

(* string = quoted / literal - literal handled specially *)
imap_string:
  | s = QUOTED_STRING { s }
  ;

(* nstring = string / nil *)
nstring:
  | NIL { None }
  | s = imap_string { Some s }
  | s = ATOM { Some s }
  ;

(* number = 1*DIGIT *)
number:
  | n = NUMBER { n }
  ;

(* nz-number = digit-nz *DIGIT *)
nz_number:
  | n = NUMBER { if n = 0L then failwith "Expected non-zero number" else n }
  ;

(* tag = 1*<any ASTRING-CHAR except "+"> - allows dots, colons, etc. for Apple Mail compatibility *)
tag:
  | t = ATOM { t }
  | n = NUMBER { Int64.to_string n }
  | parts = tag_parts { String.concat "" parts }
  ;

(* Helper for compound tags like "1.169" or "a.b.c" *)
tag_parts:
  | p1 = tag_part; DOT; rest = tag_parts_rest { p1 :: "." :: rest }
  | p1 = tag_part; COLON; rest = tag_parts_rest { p1 :: ":" :: rest }
  | p1 = tag_part; MINUS; rest = tag_parts_rest { p1 :: "-" :: rest }
  ;

tag_parts_rest:
  | p = tag_part { [p] }
  | p = tag_part; DOT; rest = tag_parts_rest { p :: "." :: rest }
  | p = tag_part; COLON; rest = tag_parts_rest { p :: ":" :: rest }
  | p = tag_part; MINUS; rest = tag_parts_rest { p :: "-" :: rest }
  ;

tag_part:
  | t = ATOM { t }
  | n = NUMBER { Int64.to_string n }
  ;

(* mailbox = "INBOX" / astring *)
mailbox:
  | s = astring { s }
  ;

(* userid / password = astring *)
userid:
  | s = astring { s }
  ;

password:
  | s = astring { s }
  ;

(* === Sequence sets - RFC 9051 Section 9 === *)

(* Simplified sequence parsing to avoid LR conflicts *)
(* seq-number can be a number or * *)
seq_num_value:
  | n = nz_number { Some (Int64.to_int n) }
  | STAR { None }
  ;

(* A sequence element is either a single value or a range *)
(* We inline the range check to avoid reduce/reduce conflicts *)
seq_element:
  | a = seq_num_value COLON b = seq_num_value
    {
      match a, b with
      | Some a, Some b -> Range (a, b)
      | Some a, None -> From a
      | None, Some b -> Range (1, b)
      | None, None -> All
    }
  | n = seq_num_value
    {
      match n with
      | Some n -> Single n
      | None -> All
    }
  ;

sequence_set:
  | e = seq_element { [e] }
  | e = seq_element COMMA rest = sequence_set { e :: rest }
  ;

(* === Flags === *)

(* flag = "\Answered" / "\Flagged" / "\Deleted" / "\Seen" / "\Draft" / flag-keyword / flag-extension *)
flag:
  | FLAG_SEEN { System Seen }
  | FLAG_ANSWERED { System Answered }
  | FLAG_FLAGGED { System Flagged }
  | FLAG_DELETED { System Deleted }
  | FLAG_DRAFT { System Draft }
  | FLAG_RECENT { System Seen }  (* \Recent is obsolete, map to Seen *)
  | s = FLAG_EXTENSION { Keyword ("\\" ^ s) }
  | s = ATOM { Keyword s }
  ;

(* flag-list = "(" [flag *(SP flag)] ")" *)
flag_list:
  | LPAREN RPAREN { [] }
  | LPAREN f = flag fs = flag_list_rest RPAREN { f :: fs }
  ;

flag_list_rest:
  | { [] }
  | SP f = flag fs = flag_list_rest { f :: fs }
  ;

(* flag-perm = flag / "\*" *)
flag_perm:
  | f = flag { f }
  | FLAG_PERM_ALL { Keyword "\\*" }
  ;

(* === Fetch items === *)

(* section-spec for BODY[...] *)
section_spec:
  | HEADER { "HEADER" }
  | HEADER_FIELDS SP LPAREN hs = header_list RPAREN { "HEADER.FIELDS (" ^ String.concat " " hs ^ ")" }
  | HEADER_FIELDS_NOT SP LPAREN hs = header_list RPAREN { "HEADER.FIELDS.NOT (" ^ String.concat " " hs ^ ")" }
  | TEXT { "TEXT" }
  | MIME { "MIME" }
  | n = section_part { n }
  ;

section_part:
  | n = nz_number { Int64.to_string n }
  | n = nz_number DOT rest = section_part { Int64.to_string n ^ "." ^ rest }
  ;

header_list:
  | h = header_name { [h] }
  | h = header_name SP hs = header_list { h :: hs }
  ;

(* Header field names can contain hyphens, e.g., "message-id", "content-type" *)
header_name:
  | s = ATOM { s }
  | s = QUOTED_STRING { s }
  | parts = hyphenated_name { String.concat "-" parts }
  ;

hyphenated_name:
  | h = ATOM MINUS rest = hyphenated_name_rest { h :: rest }
  ;

hyphenated_name_rest:
  | h = ATOM { [h] }
  | h = ATOM MINUS rest = hyphenated_name_rest { h :: rest }
  ;

(* partial = "<" number "." nz-number ">" *)
partial:
  | LANGLE start = number DOT count = nz_number RANGLE { Some (Int64.to_int start, Int64.to_int count) }
  ;

partial_opt:
  | { None }
  | p = partial { p }
  ;

(* fetch-att = "ENVELOPE" / "FLAGS" / "INTERNALDATE" / ... *)
fetch_att:
  | ENVELOPE { Fetch_envelope }
  | FLAGS { Fetch_flags }
  | INTERNALDATE { Fetch_internaldate }
  | RFC822 { Fetch_rfc822 }
  | RFC822 DOT SIZE { Fetch_rfc822_size }
  | RFC822 DOT HEADER { Fetch_rfc822_header }
  | RFC822 DOT TEXT { Fetch_rfc822_text }
  | BODY { Fetch_body }
  | BODYSTRUCTURE { Fetch_bodystructure }
  | BODY LBRACKET RBRACKET p = partial_opt { Fetch_body_section ("", p) }
  | BODY LBRACKET s = section_spec RBRACKET p = partial_opt { Fetch_body_section (s, p) }
  | BODY DOT PEEK LBRACKET RBRACKET p = partial_opt { Fetch_body_peek ("", p) }
  | BODY DOT PEEK LBRACKET s = section_spec RBRACKET p = partial_opt { Fetch_body_peek (s, p) }
  | BINARY LBRACKET s = section_part RBRACKET p = partial_opt { Fetch_binary (s, p) }
  | BINARY DOT PEEK LBRACKET s = section_part RBRACKET p = partial_opt { Fetch_binary_peek (s, p) }
  | BINARY DOT SIZE LBRACKET s = section_part RBRACKET { Fetch_binary_size s }
  | UID { Fetch_uid }
  ;

(* fetch-macro = "ALL" / "FAST" / "FULL" *)
fetch_macro:
  | ALL { [Fetch_flags; Fetch_internaldate; Fetch_rfc822_size; Fetch_envelope] }
  | FAST { [Fetch_flags; Fetch_internaldate; Fetch_rfc822_size] }
  | FULL { [Fetch_flags; Fetch_internaldate; Fetch_rfc822_size; Fetch_envelope; Fetch_body] }
  ;

fetch_att_list:
  | f = fetch_att { [f] }
  | f = fetch_att SP fs = fetch_att_list { f :: fs }
  ;

fetch_items:
  | m = fetch_macro { m }
  | f = fetch_att { [f] }
  | LPAREN fs = fetch_att_list RPAREN { fs }
  ;

(* === Store action === *)

(* store-att-flags = (["+" / "-"] "FLAGS" [".SILENT"]) SP (flag-list / (flag *(SP flag))) *)
store_action:
  | FLAGS { (Store_set, false) }
  | FLAGS DOT SILENT { (Store_set, true) }
  | PLUS FLAGS { (Store_add, false) }
  | PLUS FLAGS DOT SILENT { (Store_add, true) }
  | MINUS FLAGS { (Store_remove, false) }
  | MINUS FLAGS DOT SILENT { (Store_remove, true) }
  ;

store_flags:
  | fl = flag_list { fl }
  | f = flag fs = store_flag_rest { f :: fs }
  ;

store_flag_rest:
  | { [] }
  | SP f = flag fs = store_flag_rest { f :: fs }
  ;

(* === Search criteria - RFC 9051 Section 9 === *)

search_key:
  | ALL { Search_all }
  | ANSWERED { Search_answered }
  | DELETED_STATUS { Search_deleted }
  | FLAGGED { Search_flagged }
  | NEW { Search_new }
  | OLD { Search_old }
  | SEEN { Search_seen }
  | UNANSWERED { Search_unanswered }
  | UNDELETED { Search_undeleted }
  | UNFLAGGED { Search_unflagged }
  | UNSEEN { Search_unseen }
  | DRAFT { Search_draft }
  | UNDRAFT { Search_undraft }
  | BCC SP s = astring { Search_bcc s }
  | BEFORE SP s = astring { Search_before s }
  | BODY SP s = astring { Search_body s }
  | CC SP s = astring { Search_cc s }
  | FROM SP s = astring { Search_from s }
  | HEADER SP h = astring SP v = astring { Search_header (h, v) }
  | KEYWORD SP s = ATOM { Search_keyword s }
  | LARGER SP n = number { Search_larger n }
  | NOT SP k = search_key { Search_not k }
  | ON SP s = astring { Search_on s }
  | OR SP k1 = search_key SP k2 = search_key { Search_or (k1, k2) }
  | SENTBEFORE SP s = astring { Search_sentbefore s }
  | SENTON SP s = astring { Search_senton s }
  | SENTSINCE SP s = astring { Search_sentsince s }
  | SINCE SP s = astring { Search_since s }
  | SMALLER SP n = number { Search_smaller n }
  | SUBJECT SP s = astring { Search_subject s }
  | TEXT SP s = astring { Search_text s }
  | TO SP s = astring { Search_to s }
  | UID SP seq = sequence_set { Search_uid seq }
  | UNKEYWORD SP s = ATOM { Search_unkeyword s }
  | seq = sequence_set { Search_sequence_set seq }
  | LPAREN k = search_key RPAREN { k }
  ;

search_program:
  | k = search_key { (None, k) }
  | CHARSET SP c = astring SP k = search_key { (Some c, k) }
  ;

(* === Status attributes === *)

status_att:
  | MESSAGES { Status_messages }
  | UIDNEXT { Status_uidnext }
  | UIDVALIDITY { Status_uidvalidity }
  | UNSEEN { Status_unseen }
  | DELETED_STATUS { Status_deleted }
  | SIZE { Status_size }
  ;

status_att_list:
  | s = status_att { [s] }
  | s = status_att SP ss = status_att_list { s :: ss }
  ;

(* === Date-time === *)

date_time:
  | s = QUOTED_STRING { s }
  ;

(* === Commands === *)

(* id-params-list = string SP nstring *(SP string SP nstring) *)
id_params:
  | NIL { None }
  | LPAREN RPAREN { Some [] }
  | LPAREN pairs = id_pairs RPAREN { Some pairs }
  ;

id_pairs:
  | k = imap_string SP v = nstring { [(k, match v with Some s -> s | None -> "")] }
  | k = imap_string SP v = nstring SP rest = id_pairs { (k, match v with Some s -> s | None -> "") :: rest }
  ;

(* command-any = "CAPABILITY" / "LOGOUT" / "NOOP" / id *)
command_any:
  | CAPABILITY { Capability }
  | LOGOUT { Logout }
  | NOOP { Noop }
  | ID SP params = id_params { Id params }
  ;

(* command-nonauth = login / authenticate / "STARTTLS" *)
command_nonauth:
  | LOGIN SP u = userid SP p = password { Login { username = u; password = p } }
  | AUTHENTICATE SP m = ATOM { Authenticate { mechanism = m; initial_response = None } }
  | AUTHENTICATE SP m = ATOM SP ir = astring { Authenticate { mechanism = m; initial_response = Some ir } }
  | STARTTLS { Starttls }
  ;

(* command-auth - authenticated state commands *)
command_auth:
  | ENABLE SP caps = enable_caps { Enable caps }
  | SELECT SP mb = mailbox { Select mb }
  | EXAMINE SP mb = mailbox { Examine mb }
  | CREATE SP mb = mailbox { Create mb }
  | DELETE SP mb = mailbox { Delete mb }
  | RENAME SP old_mb = mailbox SP new_mb = mailbox { Rename { old_name = old_mb; new_name = new_mb } }
  | SUBSCRIBE SP mb = mailbox { Subscribe mb }
  | UNSUBSCRIBE SP mb = mailbox { Unsubscribe mb }
  | LIST SP ref = astring SP pat = list_mailbox { List { reference = ref; pattern = pat } }
  | NAMESPACE { Namespace }
  | STATUS SP mb = mailbox SP LPAREN atts = status_att_list RPAREN { Status { mailbox = mb; items = atts } }
  | APPEND SP mb = mailbox SP fl = flag_list SP dt = date_time SP msg = append_message
    { Append { mailbox = mb; flags = fl; date = Some dt; message = msg } }
  | APPEND SP mb = mailbox SP fl = flag_list SP msg = append_message
    { Append { mailbox = mb; flags = fl; date = None; message = msg } }
  | APPEND SP mb = mailbox SP msg = append_message
    { Append { mailbox = mb; flags = []; date = None; message = msg } }
  | IDLE { Idle }
  ;

enable_caps:
  | c = ATOM { [c] }
  | c = ATOM SP cs = enable_caps { c :: cs }
  ;

list_mailbox:
  | s = astring { s }
  | s = QUOTED_STRING { s }
  ;

append_message:
  | s = QUOTED_STRING { s }
  | LITERAL_START { "" }  (* Placeholder - literal data read separately *)
  | LITERAL_START_PLUS { "" }
  ;

(* command-select - selected state commands *)
command_select:
  | CLOSE { Close }
  | UNSELECT { Unselect }
  | EXPUNGE { Expunge }
  | SEARCH SP prog = search_program { let (charset, criteria) = prog in Search { charset; criteria } }
  | FETCH SP seq = sequence_set SP items = fetch_items { Fetch { sequence = seq; items } }
  | STORE SP seq = sequence_set SP act = store_action SP fl = store_flags
    { let (action, silent) = act in Store { sequence = seq; silent; action; flags = fl } }
  | COPY SP seq = sequence_set SP mb = mailbox { Copy { sequence = seq; mailbox = mb } }
  | MOVE SP seq = sequence_set SP mb = mailbox { Move { sequence = seq; mailbox = mb } }
  ;

(* UID prefix commands *)
uid_command:
  | FETCH SP seq = sequence_set SP items = fetch_items
    { Uid (Uid_fetch { sequence = seq; items }) }
  | STORE SP seq = sequence_set SP act = store_action SP fl = store_flags
    { let (action, silent) = act in Uid (Uid_store { sequence = seq; silent; action; flags = fl }) }
  | COPY SP seq = sequence_set SP mb = mailbox
    { Uid (Uid_copy { sequence = seq; mailbox = mb }) }
  | MOVE SP seq = sequence_set SP mb = mailbox
    { Uid (Uid_move { sequence = seq; mailbox = mb }) }
  | SEARCH SP prog = search_program
    { let (charset, criteria) = prog in Uid (Uid_search { charset; criteria }) }
  | EXPUNGE SP seq = sequence_set
    { Uid (Uid_expunge seq) }
  ;

(* Main command: tag SP command CRLF *)
command_body:
  | c = command_any { c }
  | c = command_nonauth { c }
  | c = command_auth { c }
  | c = command_select { c }
  | UID SP c = uid_command { c }
  ;

command:
  | t = tag SP c = command_body CRLF { { tag = t; command = c } }
  | t = tag SP c = command_body EOF { { tag = t; command = c } }  (* Allow missing CRLF at EOF *)
  ;

(* === Response parsing (for completeness) === *)

response_parser:
  | EOF { Bye { code = None; text = "Connection closed" } }
  ;
