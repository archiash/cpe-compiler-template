open OUnit2
open CpecmuCompiler.Ast
open CpecmuCompiler.Typechecker
open CpecmuCompiler.A3_typechecking

let dummy_pos = (Lexing.dummy_pos, Lexing.dummy_pos)

let tests = "test suite for sample typechecking" >::: [
  "2+3 string" >:: (fun _ -> assert_equal true (typecheck_string "2+3"));
  "True && False string" >:: (fun _ -> assert_equal true (typecheck_string "True && False"));
  "1+True string" >:: (fun _ -> assert_equal false (typecheck_string "1+True"));
  "1 || True string" >:: (fun _ -> assert_equal false (typecheck_string "1 || True"));

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
    let f_decl : unit prog =
      DefFunc {
        prop = ();
        pos = dummy_pos;
        name = "f";
        params = [ParamTyped ("x", TyBase "Integer")];
        return_type = TyBase "Integer";
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
        params = [ParamTyped ("y", TyBase "Integer")];
        return_type = TyBase "Integer";
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
        params = [ParamTyped ("x", TyBase "Integer")];
        return_type = TyBase "Integer";
        body = Var { prop = (); pos = dummy_pos; name = "x" };
      }
    in
    let f2 : unit prog =
      DefFunc {
        prop = ();
        pos = dummy_pos;
        name = "f";
        params = [ParamTyped ("y", TyBase "Integer")];
        return_type = TyBase "Integer";
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
        DefFunc { prop = (); pos = dummy_pos; name = "x"; params = []; return_type = TyBase "Integer"; body = IntLit { prop = (); pos = dummy_pos; value = 10 } };
        DefFunc { prop = (); pos = dummy_pos; name = "y"; params = []; return_type = TyBase "Integer"; body = BinExpr { prop = (); pos = dummy_pos; op = Add; left = Var { prop = (); pos = dummy_pos; name = "x" }; right = IntLit { prop = (); pos = dummy_pos; value = 1 } } };
        Var { prop = (); pos = dummy_pos; name = "y" };
      ];
    } in
    assert_equal true (typecheck p));

  "top-level variable forward reference rejected" >:: (fun _ ->
    let p = Prog {
      prop = ();
      pos = dummy_pos;
      decls = [
        DefFunc { prop = (); pos = dummy_pos; name = "x"; params = []; return_type = TyBase "Integer"; body = BinExpr { prop = (); pos = dummy_pos; op = Add; left = Var { prop = (); pos = dummy_pos; name = "y" }; right = IntLit { prop = (); pos = dummy_pos; value = 1 } } };
        DefFunc { prop = (); pos = dummy_pos; name = "y"; params = []; return_type = TyBase "Integer"; body = IntLit { prop = (); pos = dummy_pos; value = 10 } };
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
    let lam = Lambda {
      prop = ();
      pos = dummy_pos;
      params = [ParamTyped ("x", TyBase "Integer")];
      return_type = TyBase "Integer";
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
    let p = Let {
      prop = ();
      pos = dummy_pos;
      bindings = [
        LetVarBind { name = "a"; value = IntLit { prop = (); pos = dummy_pos; value = 1 } };
        LetVarBind { name = "b"; value = IntLit { prop = (); pos = dummy_pos; value = 2 } };
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
    let f = Lambda {
      prop = ();
      pos = dummy_pos;
      params = [ParamTyped ("x", TyBase "Integer")];
      return_type = TyBase "Integer";
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
    let f = Lambda {
      prop = ();
      pos = dummy_pos;
      params = [ParamTyped ("x", TyBase "Integer")];
      return_type = TyBase "Integer";
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
    let bad_dollar = BinExpr {
      prop = ();
      pos = dummy_pos;
      op = Dollar;
      left = IntLit { prop = (); pos = dummy_pos; value = 42 };
      right = IntLit { prop = (); pos = dummy_pos; value = 10 };
    } in
    assert_equal false (typecheck bad_dollar));

  "polymorphic identity function instantiated with different types" >:: (fun _ ->
    let id_fn = DefFunc {
      prop = ();
      pos = dummy_pos;
      name = "id";
      params = [ParamTyped ("x", TyBase "a")];
      return_type = TyBase "a";
      body = Var { prop = (); pos = dummy_pos; name = "x" };
    } in
    let call_int = DefFunc {
      prop = ();
      pos = dummy_pos;
      name = "v1";
      params = [];
      return_type = TyBase "Integer";
      body = App {
        prop = ();
        pos = dummy_pos;
        func = Var { prop = (); pos = dummy_pos; name = "id" };
        arg = IntLit { prop = (); pos = dummy_pos; value = 42 };
      };
    } in
    let call_bool = DefFunc {
      prop = ();
      pos = dummy_pos;
      name = "v2";
      params = [];
      return_type = TyBase "Bool";
      body = App {
        prop = ();
        pos = dummy_pos;
        func = Var { prop = (); pos = dummy_pos; name = "id" };
        arg = BoolLit { prop = (); pos = dummy_pos; value = true };
      };
    } in
    let prog = Prog {
      prop = ();
      pos = dummy_pos;
      decls = [id_fn; call_int; call_bool];
    } in
    assert_equal true (typecheck prog));

  "rigid type variable cannot be returned as concrete type" >:: (fun _ ->
    let bad_fn = DefFunc {
      prop = ();
      pos = dummy_pos;
      name = "bad";
      params = [ParamTyped ("x", TyBase "a")];
      return_type = TyBase "Integer";
      body = Var { prop = (); pos = dummy_pos; name = "x" };
    } in
    assert_equal false (typecheck bad_fn));

  "polymorphic pair projection and type matching" >:: (fun _ ->
    let fst_fn = DefFunc {
      prop = ();
      pos = dummy_pos;
      name = "first";
      params = [ParamTyped ("p", TyPair (TyBase "a", TyBase "b"))];
      return_type = TyBase "a";
      body = Match {
        prop = ();
        pos = dummy_pos;
        expr = Var { prop = (); pos = dummy_pos; name = "p" };
        cases = [
          (PatPair {
             prop = ();
             pos = dummy_pos;
             fst = PatVar { prop = (); pos = dummy_pos; name = "x" };
             snd = PatWildcard { prop = (); pos = dummy_pos };
           },
           Var { prop = (); pos = dummy_pos; name = "x" });
        ];
      };
    } in
    let call = DefFunc {
      prop = ();
      pos = dummy_pos;
      name = "val";
      params = [];
      return_type = TyBase "Integer";
      body = App {
        prop = ();
        pos = dummy_pos;
        func = Var { prop = (); pos = dummy_pos; name = "first" };
        arg = CpecmuCompiler.Ast.Pair {
          prop = ();
          pos = dummy_pos;
          fst = IntLit { prop = (); pos = dummy_pos; value = 1 };
          snd = BoolLit { prop = (); pos = dummy_pos; value = true };
        };
      };
    } in
    let prog = Prog { prop = (); pos = dummy_pos; decls = [fst_fn; call] } in
    assert_equal true (typecheck prog));

  "conflicting polymorphic type variables rejected" >:: (fun _ ->
    let same_fn = DefFunc {
      prop = ();
      pos = dummy_pos;
      name = "same";
      params = [ParamTyped ("p", TyPair (TyBase "a", TyBase "a"))];
      return_type = TyBase "Bool";
      body = BoolLit { prop = (); pos = dummy_pos; value = true };
    } in
    let bad_call = DefFunc {
      prop = ();
      pos = dummy_pos;
      name = "val";
      params = [];
      return_type = TyBase "Bool";
      body = App {
        prop = ();
        pos = dummy_pos;
        func = Var { prop = (); pos = dummy_pos; name = "same" };
        arg = CpecmuCompiler.Ast.Pair {
          prop = ();
          pos = dummy_pos;
          fst = IntLit { prop = (); pos = dummy_pos; value = 1 };
          snd = BoolLit { prop = (); pos = dummy_pos; value = true };
        };
      };
    } in
    let prog = Prog { prop = (); pos = dummy_pos; decls = [same_fn; bad_call] } in
    assert_equal false (typecheck prog));

  "polymorphic composition with dollar operator" >:: (fun _ ->
    let id_fn = DefFunc {
      prop = ();
      pos = dummy_pos;
      name = "id";
      params = [ParamTyped ("x", TyBase "a")];
      return_type = TyBase "a";
      body = Var { prop = (); pos = dummy_pos; name = "x" };
    } in
    let call_dollar = DefFunc {
      prop = ();
      pos = dummy_pos;
      name = "val";
      params = [];
      return_type = TyBase "Integer";
      body = BinExpr {
        prop = ();
        pos = dummy_pos;
        op = Dollar;
        left = Var { prop = (); pos = dummy_pos; name = "id" };
        right = IntLit { prop = (); pos = dummy_pos; value = 99 };
      };
    } in
    let prog = Prog { prop = (); pos = dummy_pos; decls = [id_fn; call_dollar] } in
    assert_equal true (typecheck prog));
]

let _ = run_test_tt_main tests
