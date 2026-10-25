open OUnit2
open CpecmuCompiler.A3_typechecking
open CpecmuCompiler.Ast
open CpecmuCompiler.Typechecker

let dummy_pos = (Lexing.dummy_pos, Lexing.dummy_pos)

let tests = "test suite for sample typechecking" >::: [
  "2+3" >:: (fun _ -> assert_equal true (typecheck_string "2+3"));
  "true && false" >:: (fun _ -> assert_equal true (typecheck_string "true && false"));
  "1+true" >:: (fun _ -> assert_equal false (typecheck_string "1+true"));
  "1 || true" >:: (fun _ -> assert_equal false (typecheck_string "1 || true"));
  (* AST direct tests for If, Var, Let *)
  "if true then 1 else 2" >:: (fun _ ->
    let ast = If {
      prop = (); pos = dummy_pos;
      condition = BoolLit { prop = (); pos = dummy_pos; value = true };
      consequent = IntLit { prop = (); pos = dummy_pos; value = 1 };
      alternative = IntLit { prop = (); pos = dummy_pos; value = 2 };
    } in
    assert_equal true (typecheck ast));
  "if 1 then 1 else 2 (condition not bool)" >:: (fun _ ->
    let ast = If {
      prop = (); pos = dummy_pos;
      condition = IntLit { prop = (); pos = dummy_pos; value = 1 };
      consequent = IntLit { prop = (); pos = dummy_pos; value = 1 };
      alternative = IntLit { prop = (); pos = dummy_pos; value = 2 };
    } in
    assert_equal false (typecheck ast));
  "if true then 1 else false (branch mismatch)" >:: (fun _ ->
    let ast = If {
      prop = (); pos = dummy_pos;
      condition = BoolLit { prop = (); pos = dummy_pos; value = true };
      consequent = IntLit { prop = (); pos = dummy_pos; value = 1 };
      alternative = BoolLit { prop = (); pos = dummy_pos; value = false };
    } in
    assert_equal false (typecheck ast));
  "let x = 5 in x" >:: (fun _ ->
    let ast = Let {
      prop = (); pos = dummy_pos;
      name = "x";
      value = IntLit { prop = (); pos = dummy_pos; value = 5 };
      body = Var { prop = (); pos = dummy_pos; name = "x" };
    } in
    assert_equal true (typecheck ast));
  "unbound var x" >:: (fun _ ->
    let ast = Var { prop = (); pos = dummy_pos; name = "x" } in
    assert_equal false (typecheck ast));
  "let x = true in if x then 1 else 2" >:: (fun _ ->
    let ast = Let {
      prop = (); pos = dummy_pos;
      name = "x";
      value = BoolLit { prop = (); pos = dummy_pos; value = true };
      body = If {
        prop = (); pos = dummy_pos;
        condition = Var { prop = (); pos = dummy_pos; name = "x" };
        consequent = IntLit { prop = (); pos = dummy_pos; value = 1 };
        alternative = IntLit { prop = (); pos = dummy_pos; value = 2 };
      };
    } in
    assert_equal true (typecheck ast));
]

let _ = run_test_tt_main tests
