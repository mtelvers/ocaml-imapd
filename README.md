# imapd

An IMAP4rev2 server implemented in OCaml.

Implements [RFC 9051](https://datatracker.ietf.org/doc/html/rfc9051) (IMAP4rev2) with support for implicit TLS per [RFC 8314](https://datatracker.ietf.org/doc/html/rfc8314).

## Features

- **IMAP4rev2** (RFC 9051) with IMAP4rev1 compatibility
- **Fork-per-connection** privilege separation (like UW-IMAP)
- **Implicit TLS** on port 993 (RFC 8314)
- **STARTTLS** upgrade for cleartext connections
- **PAM authentication** using system accounts
- **Maildir storage** with traditional `~/Maildir` layout
- **In-memory storage** for development and testing

### Supported Extensions

| Extension | RFC | Description |
|-----------|-----|-------------|
| IDLE | [RFC 2177](https://datatracker.ietf.org/doc/html/rfc2177) | Real-time notifications |
| NAMESPACE | [RFC 2342](https://datatracker.ietf.org/doc/html/rfc2342) | Mailbox namespaces |
| ID | [RFC 2971](https://datatracker.ietf.org/doc/html/rfc2971) | Server identification |
| UIDPLUS | [RFC 4315](https://datatracker.ietf.org/doc/html/rfc4315) | UID responses for COPY/APPEND |
| ENABLE | [RFC 5161](https://datatracker.ietf.org/doc/html/rfc5161) | Capability negotiation |
| MOVE | [RFC 6851](https://datatracker.ietf.org/doc/html/rfc6851) | Atomic move operation |
| LITERAL+ | [RFC 7888](https://datatracker.ietf.org/doc/html/rfc7888) | Non-synchronizing literals |

## Installation

### Prerequisites

- OCaml 5.0+
- opam
- PAM development headers (`libpam0g-dev` on Debian/Ubuntu)

### Building

```bash
opam install . --deps-only
dune build
```

### Running Tests

```bash
dune test
```

## Usage

### Development Server

```bash
# In-memory storage on port 10143
imapd -s memory -p 10143
```

### Production Server (Recommended)

```bash
# Fork-per-connection with implicit TLS
# Uses ~/Maildir for each user
sudo imapd --fork -s maildir --tls \
  --cert /etc/ssl/certs/mail.crt \
  --key /etc/ssl/private/mail.key \
  -p 993
```

### Single-Process with STARTTLS

```bash
# Cleartext with STARTTLS upgrade
imapd -s maildir --maildir-path /var/mail \
  --cert server.crt --key server.key \
  -p 143
```

## Operating Modes

### Single-Process (default)

All connections handled in one process. Efficient but all sessions share the same privileges. Suitable for development or trusted environments.

### Fork-per-Connection (`--fork`)

Each connection forks a child process. After successful authentication, the child drops privileges to the authenticated user via `setuid`. This provides strong isolation between users.

- Requires running as root
- Only works with Maildir storage
- STARTTLS not supported (use implicit TLS)

## Storage Backends

### Memory

In-memory storage for development and testing. Data is not persisted.

```bash
imapd -s memory
```

### Maildir

Production storage using the [Maildir format](https://cr.yp.to/proto/maildir.html).

**With shared base path:**
```bash
imapd -s maildir --maildir-path /var/mail
# Mail stored at /var/mail/<username>/
```

**With home directories (fork mode default):**
```bash
sudo imapd --fork -s maildir
# Mail stored at ~<username>/Maildir/
```

## Command-Line Options

| Option | Description |
|--------|-------------|
| `-p`, `--port` | Port to listen on (default: 143) |
| `-h`, `--host` | Host address to bind to (default: 127.0.0.1) |
| `-s`, `--storage` | Storage backend: `memory` or `maildir` |
| `--maildir-path` | Base path for Maildir storage |
| `--tls` | Enable implicit TLS (requires `--cert` and `--key`) |
| `--cert` | TLS certificate file (PEM format) |
| `--key` | TLS private key file (PEM format) |
| `--fork` | Fork per connection with privilege separation |

## Architecture

```
imapd/
├── lib/
│   ├── imap_types/     # Core IMAP types (RFC 9051)
│   ├── imap_parser/    # Menhir parser + Faraday serializer
│   ├── imap_auth/      # PAM authentication
│   ├── imap_storage/   # Memory and Maildir backends
│   └── imap_server/    # Connection handler and state machine
├── bin/
│   └── main.ml         # CLI entry point
└── test/               # Alcotest test suite
```

### Connection State Machine

```
┌─────────────────────┐
│ Not Authenticated   │ ← Initial state
├─────────────────────┤
│ CAPABILITY, NOOP    │
│ STARTTLS, LOGIN     │
│ LOGOUT              │
└─────────┬───────────┘
          │ LOGIN success
          ▼
┌─────────────────────┐
│ Authenticated       │
├─────────────────────┤
│ SELECT, EXAMINE     │
│ CREATE, DELETE      │
│ LIST, STATUS        │
│ APPEND, IDLE        │
└─────────┬───────────┘
          │ SELECT success
          ▼
┌─────────────────────┐
│ Selected            │
├─────────────────────┤
│ FETCH, STORE        │
│ SEARCH, COPY, MOVE  │
│ EXPUNGE, CLOSE      │
└─────────────────────┘
```

## Security

- **Privilege separation**: Fork mode drops to authenticated user via `setuid`
- **Path traversal protection**: Username and mailbox names are validated
- **DoS mitigation**: Maximum line length enforced (64KB)
- **TLS**: Implicit TLS recommended for production

## Testing with a Client

```bash
# Start development server
imapd -s memory -p 10143 &

# Connect with OpenSSL (cleartext for testing)
telnet localhost 10143

# Or with TLS
openssl s_client -connect localhost:993
```

Example session:
```
* OK [CAPABILITY IMAP4rev2 ...] IMAP4rev2 Service Ready
a001 LOGIN username password
a001 OK [CAPABILITY ...] LOGIN completed
a002 SELECT INBOX
* FLAGS (\Seen \Answered \Flagged \Deleted \Draft)
* 0 EXISTS
* OK [UIDVALIDITY 1234567890] UIDs valid
a002 OK [READ-WRITE] SELECT completed
a003 LOGOUT
* BYE IMAP4rev2 Server logging out
a003 OK LOGOUT completed
```

## License

[License to be determined]

## Contributing

Report bugs at https://github.com/mtelvers/imapd/issues
