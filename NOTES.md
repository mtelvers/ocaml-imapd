# IMAP Server Development Notes

## Project Location
- Local development: `/home/mtelvers/imapd`
- Server deployment: `/opt/ocaml-imapd` on `mail.tunbury.xyz`
- GitHub repo: `https://github.com/mtelvers/ocaml-imapd`

## Deployment Workflow
1. Make changes locally
2. Build: `dune build`
3. Commit: `git add . && git commit -m "message"`
4. Push: `git push`
5. Deploy on server:
   ```bash
   ssh mail.tunbury.xyz "cd /opt/ocaml-imapd && git pull && opam exec -- dune build bin/main.exe && sudo systemctl restart imapd"
   ```

## Key Files
- `lib/imap_parser/imap_lexer.mll` - Lexer (tokens)
- `lib/imap_parser/imap_grammar.mly` - Menhir parser grammar
- `lib/imap_parser/imap_parser.ml` - Parser interface and response serialization
- `lib/imap_server/imap_server.ml` - Server logic, command handlers
- `lib/imap_storage/` - Maildir storage backend
- `lib/imap_types/` - Core types
- `bin/main.exe` - Server entry point

## Checking Logs
```bash
ssh mail.tunbury.xyz "sudo journalctl -u imapd --since '5 minutes ago' --no-pager | tail -100"
```

## Checking Maildir
```bash
ssh mail.tunbury.xyz "ls -laR /home/mtelvers/Maildir/"
```

## Testing with openssl
```bash
ssh mail.tunbury.xyz "openssl s_client -connect localhost:993 -quiet"
```
Then send IMAP commands like:
```
a LOGIN mtelvers testpassword
a SELECT INBOX
a FETCH 1:* (UID FLAGS)
a LOGOUT
```

## Recent Fixes (for context)
1. **HEADER.FIELDS parsing** - Added keywords (ID, LIST, etc.) to `name_part` rule in grammar to handle header field names containing IMAP keywords
2. **Section specifier serialization** - Fixed `imap_parser.ml` to output `BODY[HEADER.FIELDS (...)]` instead of `BODY[]`
3. **Partial FETCH for BODY[]** - Implemented `BODY[]<offset.count>` partial fetches
4. **Message reconstruction** - Fixed to use `\r\n\r\n` (blank line) between headers and body
5. **LIST % wildcard** - Added PERCENT token to lexer and grammar
6. **Partial FETCH for BODY[TEXT]** - Implemented `BODY[TEXT]<offset.count>` partial fetches

## Current Status
- Apple Mail can connect and authenticate
- Messages can be fetched and displayed
- Delete/move to Deleted Messages works
- Working on ensuring all FETCH variants work correctly

## systemd Service
- Service name: `imapd`
- Service file: `/etc/systemd/system/imapd.service`
- Runs on port 993 with implicit TLS
- Fork-per-connection mode
- Maildir: `/home/mtelvers/Maildir`
