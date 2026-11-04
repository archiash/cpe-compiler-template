open OUnit2
open CpecmuCompiler.A3_typechecking
open CpecmuCompiler.Ast
open CpecmuCompiler.Typechecker

let dummy_pos = (Lexing.dummy_pos, Lexing.dummy_pos)

let tests = "test suite for sample typechecking" >::: [
  "2+3" >:: (fun _ -> assert_equal true (typecheck_string "2+3"));
  "True && False" >:: (fun _ -> assert_equal true (typecheck_string "True && False"));
  "1+True" >:: (fun _ -> assert_equal false (typecheck_string "1+True"));
  "1 || True" >:: (fun _ -> assert_equal false (typecheck_string "1 || True"));
  "two-pass: forward reference functions" >:: (fun _ ->
    (* def f x = g x; def g y = y; f 42 *)
    let f_decl : unit prog =
      DefFunc {
        prop = ();
        pos = dummy_pos;
        name = "f";
        args = ["x"];
        body = App {
          prop = ();
          pos = dummy_pos;
          func = Var { prop = (); pos = dummy_pos; name = "g" };
          arg = Var { prop = (); pos = dummy_pos; name = "x" };
        };
      }
    in
    let g_decl : unit prog =
      DefFunc {
        prop = ();
        pos = dummy_pos;
        name = "g";
        args = ["y"];
        body = Var { prop = (); pos = dummy_pos; name = "y" };
      }
    in
    let call_f : unit prog =
      App {
        prop = ();
        pos = dummy_pos;
        func = Var { prop = (); pos = dummy_pos; name = "f" };
        arg = IntLit { prop = (); pos = dummy_pos; value = 42 };
      }
    in
    let mutual_prog =
      Prog {
        prop = ();
        pos = dummy_pos;
        decls = [f_decl; g_decl; call_f];
      }
    in
    assert_equal true (typecheck mutual_prog));
  "two-pass: duplicate function definitions" >:: (fun _ ->
    let f1 : unit prog =
      DefFunc {
        prop = ();
        pos = dummy_pos;
        name = "f";
        args = ["x"];
        body = Var { prop = (); pos = dummy_pos; name = "x" };
      }
    in
    let f2 : unit prog =
      DefFunc {
        prop = ();
        pos = dummy_pos;
        name = "f";
        args = ["y"];
        body = Var { prop = (); pos = dummy_pos; name = "y" };
      }
    in
    let dup_prog =
      Prog {
        prop = ();
        pos = dummy_pos;
        decls = [f1; f2];
      }
    in
    assert_equal false (typecheck dup_prog));
]

let _ = run_test_tt_main tests
