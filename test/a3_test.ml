open OUnit2
open CpecmuCompiler.A3_typechecking

let tests = "test suite for sample typechecking" >::: [
<<<<<<< HEAD
  "2+3" >:: (fun _ -> assert_equal true (typecheck_string "2+3"));
  "true && false" >:: (fun _ -> assert_equal true (typecheck_string "true && false"));
  "1+true" >:: (fun _ -> assert_equal false (typecheck_string "1+true"));
  "1 || true" >:: (fun _ -> assert_equal false (typecheck_string "1 || true"));
=======
  "2+3" >:: (fun _ ->
    let p = BinExpr { prop = (); pos = dummy_pos; op = Add; left = IntLit { prop = (); pos = dummy_pos; value = 2 }; right = IntLit { prop = (); pos = dummy_pos; value = 3 } } in
    assert_equal true (typecheck p));

  "True && False" >:: (fun _ ->
    let p = BinExpr { prop = (); pos = dummy_pos; op = LAnd; left = BoolLit { prop = (); pos = dummy_pos; value = true }; right = BoolLit { prop = (); pos = dummy_pos; value = false } } in
    assert_equal true (typecheck p));

  "1+True" >:: (fun _ ->
    let p = BinExpr { prop = (); pos = dummy_pos; op = Add; left = IntLit { prop = (); pos = dummy_pos; value = 1 }; right = BoolLit { prop = (); pos = dummy_pos; value = true } } in
    assert_equal false (typecheck p));

  "1 || True" >:: (fun _ ->
    let p = BinExpr { prop = (); pos = dummy_pos; op = LOr; left = IntLit { prop = (); pos = dummy_pos; value = 1 }; right = BoolLit { prop = (); pos = dummy_pos; value = true } } in
    assert_equal false (typecheck p));

  "two-pass: forward reference functions" >:: (fun _ ->
    (* def f (x : Integer) : Integer = g x; def g (y : Integer) : Integer = y; f 42 *)
    let f_decl : unit prog =
      DefFuncTyped {
        prop = ();
        pos = dummy_pos;
        name = "f";
        params = [ParamTyped ("x", TyBase "Integer")];
        return_type = Some (TyBase "Integer");
        body = App {
          prop = ();
          pos = dummy_pos;
          func = Var { prop = (); pos = dummy_pos; name = "g" };
          arg = Var { prop = (); pos = dummy_pos; name = "x" };
        };
      }
    in
    let g_decl : unit prog =
      DefFuncTyped {
        prop = ();
        pos = dummy_pos;
        name = "g";
        params = [ParamTyped ("y", TyBase "Integer")];
        return_type = Some (TyBase "Integer");
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
      DefFuncTyped {
        prop = ();
        pos = dummy_pos;
        name = "f";
        params = [ParamTyped ("x", TyBase "Integer")];
        return_type = Some (TyBase "Integer");
        body = Var { prop = (); pos = dummy_pos; name = "x" };
      }
    in
    let f2 : unit prog =
      DefFuncTyped {
        prop = ();
        pos = dummy_pos;
        name = "f";
        params = [ParamTyped ("y", TyBase "Integer")];
        return_type = Some (TyBase "Integer");
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

  "top-level in-order variables allowed" >:: (fun _ ->
    let p = Prog {
      prop = ();
      pos = dummy_pos;
      decls = [
        DefFuncTyped { prop = (); pos = dummy_pos; name = "x"; params = []; return_type = Some (TyBase "Integer"); body = IntLit { prop = (); pos = dummy_pos; value = 10 } };
        DefFuncTyped { prop = (); pos = dummy_pos; name = "y"; params = []; return_type = Some (TyBase "Integer"); body = BinExpr { prop = (); pos = dummy_pos; op = Add; left = Var { prop = (); pos = dummy_pos; name = "x" }; right = IntLit { prop = (); pos = dummy_pos; value = 1 } } };
        Var { prop = (); pos = dummy_pos; name = "y" };
      ];
    } in
    assert_equal true (typecheck p));

  "top-level variable forward reference rejected" >:: (fun _ ->
    let p = Prog {
      prop = ();
      pos = dummy_pos;
      decls = [
        DefFuncTyped { prop = (); pos = dummy_pos; name = "x"; params = []; return_type = Some (TyBase "Integer"); body = BinExpr { prop = (); pos = dummy_pos; op = Add; left = Var { prop = (); pos = dummy_pos; name = "y" }; right = IntLit { prop = (); pos = dummy_pos; value = 1 } } };
        DefFuncTyped { prop = (); pos = dummy_pos; name = "y"; params = []; return_type = Some (TyBase "Integer"); body = IntLit { prop = (); pos = dummy_pos; value = 10 } };
      ];
    } in
    assert_equal false (typecheck p));

  "duplicate variable in pattern rejected" >:: (fun _ ->
    let p = Match {
      prop = ();
      pos = dummy_pos;
      expr = EmptyList { prop = (); pos = dummy_pos };
      cases = [
        (PatCons {
           prop = ();
           pos = dummy_pos;
           head = PatVar { prop = (); pos = dummy_pos; name = "x" };
           tail = PatVar { prop = (); pos = dummy_pos; name = "x" };
         },
         IntLit { prop = (); pos = dummy_pos; value = 0 });
      ];
    } in
    assert_equal false (typecheck p));

  "pairs and pair pattern matching" >:: (fun _ ->
    (* match (1, True) with | (a, b) -> a *)
    let p = Match {
      prop = ();
      pos = dummy_pos;
      expr = CpecmuCompiler.Ast.Pair {
        prop = ();
        pos = dummy_pos;
        fst = IntLit { prop = (); pos = dummy_pos; value = 1 };
        snd = BoolLit { prop = (); pos = dummy_pos; value = true };
      };
      cases = [
        (PatPair {
           prop = ();
           pos = dummy_pos;
           fst = PatVar { prop = (); pos = dummy_pos; name = "a" };
           snd = PatVar { prop = (); pos = dummy_pos; name = "b" };
         },
         Var { prop = (); pos = dummy_pos; name = "a" });
      ];
    } in
    assert_equal true (typecheck p));

  "duplicate variable in pair pattern rejected" >:: (fun _ ->
    let p = Match {
      prop = ();
      pos = dummy_pos;
      expr = CpecmuCompiler.Ast.Pair {
        prop = ();
        pos = dummy_pos;
        fst = IntLit { prop = (); pos = dummy_pos; value = 1 };
        snd = IntLit { prop = (); pos = dummy_pos; value = 2 };
      };
      cases = [
        (PatPair {
           prop = ();
           pos = dummy_pos;
           fst = PatVar { prop = (); pos = dummy_pos; name = "x" };
           snd = PatVar { prop = (); pos = dummy_pos; name = "x" };
         },
         Var { prop = (); pos = dummy_pos; name = "x" });
      ];
    } in
    assert_equal false (typecheck p));

  "lambda expressions typecheck" >:: (fun _ ->
    (* \(x : Integer) : Integer => x + 1 *)
    let lam = Lambda {
      prop = ();
      pos = dummy_pos;
      params = [ParamTyped ("x", TyBase "Integer")];
      return_type = Some (TyBase "Integer");
      body = BinExpr {
        prop = ();
        pos = dummy_pos;
        op = Add;
        left = Var { prop = (); pos = dummy_pos; name = "x" };
        right = IntLit { prop = (); pos = dummy_pos; value = 1 };
      };
    } in
    let call = App {
      prop = ();
      pos = dummy_pos;
      func = lam;
      arg = IntLit { prop = (); pos = dummy_pos; value = 10 };
    } in
    assert_equal true (typecheck call));

  "let bindings with multiple variables" >:: (fun _ ->
    (* let a = 1 and b = 2 in a + b *)
    let p = LetBindings {
      prop = ();
      pos = dummy_pos;
      bindings = [
        LetVarBind { name = "a"; return_type = Some (TyBase "Integer"); value = IntLit { prop = (); pos = dummy_pos; value = 1 } };
        LetVarBind { name = "b"; return_type = Some (TyBase "Integer"); value = IntLit { prop = (); pos = dummy_pos; value = 2 } };
      ];
      body = BinExpr {
        prop = ();
        pos = dummy_pos;
        op = Add;
        left = Var { prop = (); pos = dummy_pos; name = "a" };
        right = Var { prop = (); pos = dummy_pos; name = "b" };
      };
    } in
    assert_equal true (typecheck p));

  "function composition of valid functions" >:: (fun _ ->
    (* (f . g) where f : Integer -> Integer, g : Integer -> Integer *)
    let f = Lambda {
      prop = ();
      pos = dummy_pos;
      params = [ParamTyped ("x", TyBase "Integer")];
      return_type = Some (TyBase "Integer");
      body = Var { prop = (); pos = dummy_pos; name = "x" };
    } in
    let comp = BinExpr {
      prop = ();
      pos = dummy_pos;
      op = Compose;
      left = f;
      right = f;
    } in
    assert_equal true (typecheck comp));

  "function composition of non-function rejected" >:: (fun _ ->
    (* 42 . f MUST FAIL *)
    let f = Lambda {
      prop = ();
      pos = dummy_pos;
      params = [ParamTyped ("x", TyBase "Integer")];
      return_type = Some (TyBase "Integer");
      body = Var { prop = (); pos = dummy_pos; name = "x" };
    } in
    let bad_comp = BinExpr {
      prop = ();
      pos = dummy_pos;
      op = Compose;
      left = IntLit { prop = (); pos = dummy_pos; value = 42 };
      right = f;
    } in
    assert_equal false (typecheck bad_comp));

  "dollar operator with non-function rejected" >:: (fun _ ->
    (* 42 $ 10 MUST FAIL *)
    let bad_dollar = BinExpr {
      prop = ();
      pos = dummy_pos;
      op = Dollar;
      left = IntLit { prop = (); pos = dummy_pos; value = 42 };
      right = IntLit { prop = (); pos = dummy_pos; value = 10 };
    } in
    assert_equal false (typecheck bad_dollar));
>>>>>>> 88901e8 (enforced strict function types for composition and application)
]

let _ = run_test_tt_main tests