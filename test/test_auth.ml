(** Tests for imap_auth module *)

open Imap_auth

let test_mock_auth_add_user () =
  let auth = Mock_auth.create ~service_name:"test" in
  Mock_auth.add_user auth ~username:"alice" ~password:"secret";
  Alcotest.(check bool) "auth success" true
    (Mock_auth.authenticate auth ~username:"alice" ~password:"secret");
  Alcotest.(check bool) "auth fail wrong pass" false
    (Mock_auth.authenticate auth ~username:"alice" ~password:"wrong");
  Alcotest.(check bool) "auth fail unknown user" false
    (Mock_auth.authenticate auth ~username:"bob" ~password:"secret")

let test_mock_auth_remove_user () =
  let auth = Mock_auth.create ~service_name:"test" in
  Mock_auth.add_user auth ~username:"alice" ~password:"secret";
  Alcotest.(check bool) "auth before remove" true
    (Mock_auth.authenticate auth ~username:"alice" ~password:"secret");
  Mock_auth.remove_user auth ~username:"alice";
  Alcotest.(check bool) "auth after remove" false
    (Mock_auth.authenticate auth ~username:"alice" ~password:"secret")

let test_mock_auth_update_password () =
  let auth = Mock_auth.create ~service_name:"test" in
  Mock_auth.add_user auth ~username:"alice" ~password:"old";
  Mock_auth.add_user auth ~username:"alice" ~password:"new";
  Alcotest.(check bool) "old password fails" false
    (Mock_auth.authenticate auth ~username:"alice" ~password:"old");
  Alcotest.(check bool) "new password works" true
    (Mock_auth.authenticate auth ~username:"alice" ~password:"new")

let test_pam_available () =
  (* PAM should be available if compiled with PAM support *)
  let available = Pam_auth.is_available () in
  Alcotest.(check bool) "PAM available" true available

let () =
  let open Alcotest in
  run "imap_auth" [
    "mock_auth", [
      test_case "add_user and authenticate" `Quick test_mock_auth_add_user;
      test_case "remove_user" `Quick test_mock_auth_remove_user;
      test_case "update password" `Quick test_mock_auth_update_password;
    ];
    "pam_auth", [
      test_case "is_available" `Quick test_pam_available;
    ];
  ]
