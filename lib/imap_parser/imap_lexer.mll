(** IMAP Lexer - RFC 9051 Section 9 ABNF

    Implements lexical analysis for IMAP4rev2 protocol.
    See {{:https://datatracker.ietf.org/doc/html/rfc9051#section-9}RFC 9051 Section 9}. *)

{
open Imap_grammar

exception Lexer_error of string

(* Buffer for accumulating quoted string contents *)
let string_buffer = Buffer.create 256

(* Track position for error messages *)
let current_pos lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  Printf.sprintf "line %d, column %d"
    pos.Lexing.pos_lnum
    (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)

(* Case-insensitive keyword matching *)
let keyword_table = Hashtbl.create 64
let () =
  List.iter (fun (kw, tok) -> Hashtbl.add keyword_table (String.uppercase_ascii kw) tok)
    [
      (* Any state commands *)
      ("CAPABILITY", CAPABILITY);
      ("NOOP", NOOP);
      ("LOGOUT", LOGOUT);
      ("ID", ID);
      (* Not authenticated commands *)
      ("STARTTLS", STARTTLS);
      ("LOGIN", LOGIN);
      ("AUTHENTICATE", AUTHENTICATE);
      (* Authenticated commands *)
      ("ENABLE", ENABLE);
      ("SELECT", SELECT);
      ("EXAMINE", EXAMINE);
      ("CREATE", CREATE);
      ("DELETE", DELETE);
      ("RENAME", RENAME);
      ("SUBSCRIBE", SUBSCRIBE);
      ("UNSUBSCRIBE", UNSUBSCRIBE);
      ("LIST", LIST);
      ("NAMESPACE", NAMESPACE);
      ("STATUS", STATUS);
      ("APPEND", APPEND);
      ("IDLE", IDLE);
      (* Selected state commands *)
      ("CLOSE", CLOSE);
      ("UNSELECT", UNSELECT);
      ("EXPUNGE", EXPUNGE);
      ("SEARCH", SEARCH);
      ("FETCH", FETCH);
      ("STORE", STORE);
      ("COPY", COPY);
      ("MOVE", MOVE);
      ("UID", UID);
      (* Fetch attributes *)
      ("ENVELOPE", ENVELOPE);
      ("FLAGS", FLAGS);
      ("INTERNALDATE", INTERNALDATE);
      ("RFC822", RFC822);
      ("BODY", BODY);
      ("BODYSTRUCTURE", BODYSTRUCTURE);
      ("BINARY", BINARY);
      ("ALL", ALL);
      ("FAST", FAST);
      ("FULL", FULL);
      (* Status attributes *)
      ("MESSAGES", MESSAGES);
      ("UIDNEXT", UIDNEXT);
      ("UIDVALIDITY", UIDVALIDITY);
      ("UNSEEN", UNSEEN);
      ("DELETED", DELETED_STATUS);
      ("SIZE", SIZE);
      (* Search keys *)
      ("ANSWERED", ANSWERED);
      ("BCC", BCC);
      ("BEFORE", BEFORE);
      ("CC", CC);
      ("DRAFT", DRAFT);
      ("FLAGGED", FLAGGED);
      ("FROM", FROM);
      ("HEADER", HEADER);
      ("KEYWORD", KEYWORD);
      ("LARGER", LARGER);
      ("NEW", NEW);
      ("NOT", NOT);
      ("OLD", OLD);
      ("ON", ON);
      ("OR", OR);
      ("SEEN", SEEN);
      ("SENTBEFORE", SENTBEFORE);
      ("SENTON", SENTON);
      ("SENTSINCE", SENTSINCE);
      ("SINCE", SINCE);
      ("SMALLER", SMALLER);
      ("SUBJECT", SUBJECT);
      ("TEXT", TEXT);
      ("TO", TO);
      ("UNANSWERED", UNANSWERED);
      ("UNDELETED", UNDELETED);
      ("UNDRAFT", UNDRAFT);
      ("UNFLAGGED", UNFLAGGED);
      ("UNKEYWORD", UNKEYWORD);
      (* Other *)
      ("NIL", NIL);
      ("CHARSET", CHARSET);
      ("MIME", MIME);
      ("PEEK", PEEK);
      ("HEADER.FIELDS", HEADER_FIELDS);
      ("HEADER.FIELDS.NOT", HEADER_FIELDS_NOT);
      ("SILENT", SILENT);
      ("RETURN", RETURN);
      ("SUBSCRIBED", SUBSCRIBED);
      ("CHILDREN", CHILDREN);
      ("REMOTE", REMOTE);
      ("RECURSIVEMATCH", RECURSIVEMATCH);
      ("DONE", DONE);
    ]

let lookup_keyword s =
  let upper = String.uppercase_ascii s in
  try Hashtbl.find keyword_table upper
  with Not_found -> ATOM s
}

(* Character classes per RFC 9051 *)
let digit = ['0'-'9']
let nz_digit = ['1'-'9']
let alpha = ['A'-'Z' 'a'-'z']
let sp = ' '
let crlf = "\r\n"

(* ATOM-CHAR: any CHAR except atom-specials and grammar-significant chars *)
(* atom-specials = "(" / ")" / "{" / SP / CTL / list-wildcards / quoted-specials / resp-specials *)
(* list-wildcards = "%" / "*" *)
(* quoted-specials = DQUOTE / "\\" *)
(* resp-specials = "]" *)
(* Also exclude : , < > which are grammar tokens for sequences and partials *)
let atom_char = [^ '(' ')' '{' '}' ' ' '\x00'-'\x1f' '\x7f' '%' '*' '"' '\\' ']' '[' ':' ',' '<' '>' '+' '-' '.']

(* ASTRING-CHAR: ATOM-CHAR / resp-specials (allows ']') *)
let astring_char = [^ '(' ')' '{' '}' ' ' '\x00'-'\x1f' '\x7f' '%' '*' '"' '\\' ':' ',' '<' '>' '+' '-' '.']

(* TAG: 1*<any ASTRING-CHAR except "+"> *)
let tag_char = [^ '(' ')' '{' '}' ' ' '\x00'-'\x1f' '\x7f' '%' '*' '"' '\\' '+' ':' ',' '<' '>' '-' '.']

rule token = parse
  (* Whitespace - SP is significant in IMAP *)
  | sp        { SP }
  | crlf      { CRLF }

  (* Special characters *)
  | '('       { LPAREN }
  | ')'       { RPAREN }
  | '['       { LBRACKET }
  | ']'       { RBRACKET }
  | '{'       { LBRACE }
  | '}'       { RBRACE }
  | '*'       { STAR }
  | '+'       { PLUS }
  | '-'       { MINUS }
  | ':'       { COLON }
  | ','       { COMMA }
  | '.'       { DOT }
  | '<'       { LANGLE }
  | '>'       { RANGLE }

  (* System flags - must come before atom *)
  | '\\' (['A'-'Z' 'a'-'z']+ as flag)
    {
      match String.uppercase_ascii flag with
      | "SEEN" -> FLAG_SEEN
      | "ANSWERED" -> FLAG_ANSWERED
      | "FLAGGED" -> FLAG_FLAGGED
      | "DELETED" -> FLAG_DELETED
      | "DRAFT" -> FLAG_DRAFT
      | "RECENT" -> FLAG_RECENT
      | _ -> FLAG_EXTENSION flag
    }
  | "\\*"     { FLAG_PERM_ALL }

  (* Numbers *)
  | nz_digit digit* as n
    {
      try NUMBER (Int64.of_string n)
      with Failure _ -> raise (Lexer_error ("Number too large: " ^ n))
    }
  | '0'       { NUMBER 0L }

  (* Quoted string *)
  | '"'
    {
      Buffer.clear string_buffer;
      quoted_string lexbuf;
      QUOTED_STRING (Buffer.contents string_buffer)
    }

  (* Literal - returns the count, caller must read the data *)
  | '{' (digit+ as n) '}' crlf
    {
      try LITERAL_START (Int64.of_string n)
      with Failure _ -> raise (Lexer_error ("Literal size too large: " ^ n))
    }

  (* Literal with + for non-synchronizing - RFC 7888 *)
  | '{' (digit+ as n) '+' '}' crlf
    {
      try LITERAL_START_PLUS (Int64.of_string n)
      with Failure _ -> raise (Lexer_error ("Literal size too large: " ^ n))
    }

  (* Atoms and keywords *)
  | atom_char+ as s
    { lookup_keyword s }

  (* End of input *)
  | eof       { EOF }

  (* Unknown character *)
  | _ as c
    { raise (Lexer_error (Printf.sprintf "Unexpected character '%c' at %s" c (current_pos lexbuf))) }

(* Quoted string contents - handles escapes *)
and quoted_string = parse
  | '"'       { () }
  | '\\' '"'  { Buffer.add_char string_buffer '"'; quoted_string lexbuf }
  | '\\' '\\' { Buffer.add_char string_buffer '\\'; quoted_string lexbuf }
  | '\\' _    { raise (Lexer_error "Invalid escape sequence in quoted string") }
  | [^ '"' '\\' '\r' '\n']+ as s
    { Buffer.add_string string_buffer s; quoted_string lexbuf }
  | eof       { raise (Lexer_error "Unterminated quoted string") }
  | _         { raise (Lexer_error "Invalid character in quoted string") }

{
(** Read exactly n bytes for a literal *)
let read_literal lexbuf n =
  let n = Int64.to_int n in
  let buf = Bytes.create n in
  for i = 0 to n - 1 do
    let c = Lexing.lexeme_char lexbuf 0 in
    Bytes.set buf i c;
    ignore (Lexing.lexeme lexbuf)
  done;
  Bytes.to_string buf
}
