(lang dune 3.0)

(name imapd)

(generate_opam_files true)

(using menhir 2.1)

(source
 (github mtelvers/imapd))

(authors "Mark Maybury")

(maintainers "Mark Maybury")

(license ISC)

(documentation https://github.com/mtelvers/imapd)

(package
 (name imapd)
 (synopsis "IMAP4rev2 server implemented in OCaml with EIO")
 (description
  "A modular IMAP4rev2 server (RFC 9051) implemented in OCaml using EIO for networking. Features pluggable storage backends (Memory, Maildir) and PAM authentication.")
 (depends
  (ocaml (>= 5.1))
  (dune (>= 3.0))
  (menhir (>= 20230608))
  (eio (>= 1.0))
  (eio_main (>= 1.0))
  (tls-eio (>= 0.15))
  (tls (>= 0.15))
  (faraday (>= 0.8))
  (cmdliner (>= 1.2))
  (alcotest (and (>= 1.7) :with-test))))
