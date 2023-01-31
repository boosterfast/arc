(* This module implements an AST desugaring lowering. *)
open Utils
open Ir1
open Error

module Ctx = struct
  type t = {
    graph: Graph.t;          (* Graph of top-level item declarations *)
    next_def_uid: Gen.t;     (* Function uid counter *)
    next_type_uid: Gen.t;    (* Type uid counter *)
    next_generic_uid: Gen.t; (* Generic uid counter *)
    next_expr_uid: Gen.t;    (* Expression uid counter *)
    vstack: vscope list;     (* Stack of scopes for value parameters *)
    gstack: gscope list;     (* Stack of scopes for type parameters *)
    path: path;              (* Current path *)
    ir1: definition list;    (* Final output of the lowering *)
    astack: ascope list;     (* Stack of anonymous variables *)
  }
  and mut = MVar | MVal
  and vscope = { vsubsts: (name * (name * mut)) list; stmts: stmt list }
  and gscope = { gsubsts: (name * name) list }
  and ascope = { asubsts: name list }
  and definition = path * item

  let rec make graph = {
    graph;
    next_def_uid = Gen.make ();
    next_type_uid = Gen.make ();
    next_generic_uid = Gen.make ();
    next_expr_uid = Gen.make ();
    vstack = [];
    gstack = [];
    astack = [];
    path = [];
    ir1 = [];
  }

  and add_item xs i ctx = { ctx with ir1 = (xs, i)::ctx.ir1 }

  and new_expr f ctx =
    let (t, ctx) = ctx |> fresh_t in
    (f t, ctx)

  and new_pat f ctx =
    let (t, ctx) = ctx |> fresh_t in
    (f t, ctx)

  and fresh_ts n ctx =
    repeat fresh_t n ctx

  and fresh_t ctx =
    let (n, next_type_uid) = ctx.next_type_uid |> Gen.fresh in
    let ctx = { ctx with next_type_uid } in
    let x = Printf.sprintf "%d" n in
    let t = TVar x in
    (t, ctx)

  and fresh_xs n ctx =
    repeat fresh_x n ctx

  and fresh_x ctx =
    let (n, next_expr_uid) = ctx.next_expr_uid |> Gen.fresh in
    let x = Printf.sprintf "x%d" n in
    let ctx = { ctx with next_expr_uid; } in
    (x, ctx)

  and fresh_ps n ctx =
    let (xs, ctx) = fresh_xs n ctx in
    let (ts, ctx) = fresh_ts n ctx in
    (zip xs ts, ctx)

  and fresh_f ctx =
    let (n, next_def_uid) = ctx.next_def_uid |> Gen.fresh in
    let x = Printf.sprintf "f%d" n in
    let ctx = { ctx with next_def_uid; } in
    ([x], ctx)

  and fresh_g ctx =
    let (n, next_generic_uid) = ctx.next_generic_uid |> Gen.fresh in
    let g = Printf.sprintf "T%d" n in
    let ctx = { ctx with next_generic_uid; } in
    (g, ctx)

  and push_vscope ctx =
    { ctx with vstack = {vsubsts=[];stmts=[]}::ctx.vstack }

  and pop_vscope ctx =
    ((hd ctx.vstack).stmts |> rev, { ctx with vstack = tl ctx.vstack })

  and pop_vscope_to_block e ctx =
    let (ss, ctx) = pop_vscope ctx in
    ((ss, e), ctx)

  and add_stmts ss ctx =
    match ctx.vstack with
    | h::t -> { ctx with vstack = { h with stmts = ss @ h.stmts}::t }
    | [] -> unreachable ()

  and add_stmt_expr e ctx =
    match ctx.vstack with
    | h::t -> { ctx with vstack = { h with stmts = (SExpr e)::h.stmts}::t }
    | [] -> unreachable ()

  and push_gscope ctx =
    { ctx with gstack = {gsubsts=[]}::ctx.gstack }

  and pop_gscope (ctx:t) = { ctx with gstack = tl ctx.gstack }

  and push_ascope (ctx:t) = { ctx with astack = { asubsts=[] }::ctx.astack }

  and pop_ascope (ctx:t) =
    let vs = (hd ctx.astack).asubsts in
    let ctx = { ctx with astack = tl ctx.astack } in
    (vs, ctx)

  and add_anon (ctx:t) =
    match ctx.astack with
    | h::t ->
        let (x, ctx) = ctx |> fresh_x in
        let ctx = { ctx with astack = { asubsts=x::h.asubsts }::t } in
        (x, ctx)
    | [] -> unreachable ()

  (* Returns a name path *)
  and item_path x ctx = x::ctx.path |> rev

  and bind_gname g (ctx:t) =
    match ctx.gstack with
    | h::t ->
        let (g', ctx) = ctx |> fresh_g in
        let gstack = { gsubsts = (g, g')::h.gsubsts }::t in
        let ctx = { ctx with gstack } in
        (g', ctx)
    | [] -> unreachable ()

  and bind_vname v m (ctx:t) =
    match ctx.vstack with
    | h::t ->
        let (v', ctx) = ctx |> fresh_x in
        let vstack = {h with vsubsts = (v, (v', m))::h.vsubsts}::t in
        let ctx = { ctx with vstack } in
        (v', ctx)
    | [] -> unreachable ()

  (* Finds a value variable. Note that this implementation allows shadowing
     since we look at the most recently bound variables first. *)
  and find_vname_opt v (ctx:t) =
    ctx.vstack |> List.find_map (fun vscope -> vscope.vsubsts |> List.assoc_opt v)

  and find_vname info v ctx =
    match ctx |> find_vname_opt v with
    | Some v -> v
    | None ->
        if !Args.verbose then begin
          Printf.eprintf "Currently bound variables: ";
          debug_vstack ctx
        end;
        raise (Error.NamingError (info, "Undefined variable `" ^ v ^ "`"))

  and debug_vstack ctx =
    let rec debug_vstack vstack =
      match vstack with
      | h::t ->
          h.vsubsts |> List.iter (function
            | (x0, (x1, MVal)) -> Printf.eprintf "%s -> var %s " x0 x1
            | (x0, (x1, MVar)) -> Printf.eprintf "%s -> val %s " x0 x1
          );
          debug_vstack t
      | [] -> ()
    in
    debug_vstack ctx.vstack

  (* Finds a generic variable *)
  and find_gname_opt g (ctx:t) =
    ctx.gstack |> List.find_map (fun gscope -> gscope.gsubsts |> List.assoc_opt g)

  and find_gname info g ctx =
    match ctx |> find_gname_opt g with
    | Some g -> g
    | None -> raise (Error.NamingError (info, "Undefined generic `" ^ g ^ "`"))

  and push_namespace x ctx =
    let ctx = { ctx with path = x::ctx.path } in
    (ctx.path |> rev, ctx)

  and pop_namespace ctx = { ctx with path = ctx.path |> List.tl }

  and resolve_path_opt xs ctx =
    match xs with
    | Ast.PAbs xs -> ctx.graph |> Graph.resolve_path xs
    | Ast.PRel xs -> ctx.graph |> Graph.resolve_path ((rev ctx.path) @ xs)

  and resolve_path info xs ctx =
    match resolve_path_opt xs ctx with
    | Some ((xs, d)) -> (xs, d)
    | None ->
        if !Args.verbose then begin
          Printf.eprintf "%s" (String.concat "::" ctx.path);
        end;
        raise (Error.NamingError (info, "Path is not bound to anything: " ^ (Print.Ast.path_to_str xs)))

  (* Returns set of currently visible variables *)
  and visible ctx =
     ctx.vstack |> foldl (fun acc vscope -> vscope.vsubsts |> foldl (fun acc v -> v::acc) acc) []

end

let rec ast_to_ir1 graph ast =
  let ctx = Ctx.make graph in
  let ctx = ast |> foldl (fun ctx i -> lower_item i ctx) ctx in
  let ir1 = ctx.ir1 |> rev in
  ir1

and is_async efs =
  efs |> List.mem "async"

and lower_item i ctx =
  match i with
  (* | Ast.INoop _ -> ctx *)
  | Ast.IVal (info, d, x, t, e) ->
      let xs = ctx |> Ctx.item_path x in
      let (t, ctx) = lower_type_or_fresh t ctx in
      let ctx = ctx |> Ctx.push_vscope in
      let (e, ctx) = lower_expr e ctx in
      let (b, ctx) = ctx |> Ctx.pop_vscope_to_block e in
      ctx |> Ctx.add_item xs (IVal (info, d, t, b))
  | Ast.IAbstractDef (info, d, a, x, gs, ts, t, efs, bs) ->
      let x = Ast.def_name x in
      let xs = ctx |> Ctx.item_path x in
      let ctx = ctx |> Ctx.push_gscope in
      let (gs, ctx) = gs |> mapm lower_generic ctx in
      let (ts, ctx) = ts |> mapm lower_type ctx in
      let (t, ctx) = lower_type_or_unit t ctx in
      let (bs, ctx) = bs |> mapm (lower_bound info) ctx in
      let ctx = ctx |> Ctx.pop_gscope in
      let ctx = ctx |> Ctx.add_item xs (IAbstractDef (info, d, a, gs, ts, t, efs, bs)) in
      if is_async efs then
        ctx
      else
        let (vs, ctx) = ctx |> Ctx.fresh_xs (List.length ts) in
        let ps = zip vs ts in
        ctx |> add_indirect_def info x gs ps t bs
  | Ast.IAbstractType (info, d, a, x, gs, bs) ->
      let xs = ctx |> Ctx.item_path x in
      let ctx = ctx |> Ctx.push_gscope in
      let (gs, ctx) = gs |> mapm lower_generic ctx in
      let (bs, ctx) = bs |> mapm (lower_bound info) ctx in
      let ctx = ctx |> Ctx.pop_gscope in
      ctx |> Ctx.add_item xs (IAbstractType (info, d, a, gs, bs))
  | Ast.IDef (info, d, x, gs, ps, t, bs, b) ->
      let x = Ast.def_name x in
      let xs = ctx |> Ctx.item_path x in
      let ctx = ctx |> Ctx.push_gscope in
      let ctx = ctx |> Ctx.push_vscope in
      let (gs, ctx) = gs |> mapm lower_generic ctx in
      let (ps, ctx) = ps |> mapm lower_pat ctx in
      let (t, ctx) = ctx |> lower_type_or_fresh t in
      let (bs, ctx) = bs |> mapm (lower_bound info) ctx in
      let ((ss0, e), ctx) = ctx |> lower_block b in
      let (ss1, ctx) = ctx |> Ctx.pop_vscope in
      let b = (ss1 @ ss0, e) in
      let ctx = ctx |> Ctx.pop_gscope in
      if ps <> [] then
        let (vs, ctx) = ps |> mapm (fun _ ctx -> Ctx.fresh_x ctx) ctx in
        let ts = ps |> map Ir1.typeof_pat in
        let (e, ctx) = ctx |> vars_to_expr_record info vs in
        let (p, ctx) = ctx |> patterns_to_record info ps in
        let (e, ctx) = ctx |> Ctx.new_expr (fun t -> EMatch (info, t, e, [(p, b)])) in
        let vts = zip vs ts in
        ctx |> Ctx.add_item xs (IDef (info, d, gs, vts, t, bs, ([], e)))
            |> add_indirect_def info x gs vts t bs
      else
        ctx |> Ctx.add_item xs (IDef (info, d, gs, [], t, bs, b))
            |> add_indirect_def info x gs [] t bs
  | Ast.IType (info, d, x, gs, t, bs) ->
      let xs = ctx |> Ctx.item_path x in
      let ctx = ctx |> Ctx.push_gscope in
      let (gs, ctx) = gs |> mapm lower_generic ctx in
      let (t, ctx) = lower_type t ctx in
      let (bs, ctx) = bs |> mapm (lower_bound info) ctx in
      let ctx = ctx |> Ctx.pop_gscope in
      ctx |> Ctx.add_item xs (IType (info, d, gs, t, bs))
  | Ast.IMod (_, _, x, is) ->
      let (_, ctx) = ctx |> Ctx.push_namespace x in
      let ctx = is |> foldl (fun ctx i -> lower_item i ctx) ctx in
      let ctx = ctx |> Ctx.pop_namespace in
      ctx
  | Ast.IUse _ -> ctx
  | Ast.IClass (_loc, _d, _x, _gs, _bs, _decls) -> todo ()
  | Ast.IInstance (_loc, _d, _gs, _xs, _ts, _bs, _defs) -> todo ()
  | Ast.IFrom (_, _, _, _) -> todo ()

and lower_bound info (xs, ts) ctx =
  let (ts, ctx) = ts |> mapm lower_type ctx in
  begin match ctx |> Ctx.resolve_path info xs with
  | (xs, Graph.NItem _) -> ((xs, ts), ctx)
  |  _ -> raise (Error.NamingError (info, "Expected class, found something else."))
  end

and vars_to_exprs info ps ctx =
  ps |> mapm (fun x ctx -> ctx |> Ctx.new_expr (fun t -> EVar (info, t, x))) ctx

and vars_to_patterns info ps ctx =
  ps |> mapm (fun x ctx -> ctx |> Ctx.new_expr (fun t -> PVar (info, t, x))) ctx

and var_to_expr info v ctx = ctx |> Ctx.new_expr (fun t -> EVar (info, t, v))

and var_to_generic g = TGeneric g

and vars_to_expr_record info xs ctx =
  let (es, ctx) = ctx |> vars_to_exprs info xs in
  ctx |> exprs_to_record info es

and exprs_to_record info es ctx =
  let xes = indexes_to_fields es in
  ctx |> Ctx.new_expr (fun t -> ERecord (info, t, (xes, None)))

and types_to_record ts =
  let fs = indexes_to_fields ts in
  (TRecord (fs |> fields_to_rows TRowEmpty))

and patterns_to_record info ps ctx =
  let xps = indexes_to_fields ps in
  ctx |> Ctx.new_pat (fun t -> PRecord (info, t, (xps, None)))

and indirect_name x = Printf.sprintf "%s_indirect" x

and add_indirect_def info x gs vts t bs ctx =
  let xs_direct = ctx |> Ctx.item_path x in
  let xs = ctx |> Ctx.item_path (indirect_name x) in
  let (es, ctx) = vts |> map fst |> mapm (var_to_expr info) ctx in
  let vts = vts @ [("_", TRecord TRowEmpty)] in
  let ts = gs |> map (function g -> TGeneric g) in
  let (t1, ctx) = ctx |> Ctx.fresh_t in
  let e = ECallItem (info, t1, xs_direct, ts, es) in
  let b = ([], e) in
  ctx |> Ctx.add_item xs (IDef (info, [], gs, vts, t, bs, b))

and lower_generic x ctx =
  let (x, ctx) = ctx |> Ctx.bind_gname x in
  (x, ctx)

and lower_sink (x, t) ctx =
  let (x, ctx) = ctx |> Ctx.bind_vname x MVal in
  let (t, ctx) = lower_type_or_fresh t ctx in
  ((x, t), ctx)

and lower_param p ctx =
  let (p, ctx) = lower_pat p ctx in
  (p, ctx)

(* Arg expressions can contain underscores. These are captured in `ascope`. *)
and lower_expr_arg e ctx =
  let info = Ast.expr_info e in
  let ctx = ctx |> Ctx.push_ascope in
  let ctx = ctx |> Ctx.push_vscope in
  let (e, ctx) = lower_expr e ctx in
  let (es, ctx) = ctx |> Ctx.pop_vscope in
  let ctx = ctx |> Ctx.add_stmts es in
  let (vs, ctx) = ctx |> Ctx.pop_ascope in
  if vs = [] then
      (e, ctx)
  else
      let (vts, ctx) = vs |> mapm (fun v ctx ->
          let (t, ctx) = ctx |> Ctx.fresh_t in
          ((v, t), ctx)
      ) ctx in
      let (xs, ctx) = ctx |> Ctx.fresh_f in
      let (t, ctx) = ctx |> Ctx.fresh_t in
      let (x_env, ctx) = ctx |> Ctx.fresh_x in
      let (t_env, ctx) = ctx |> Ctx.fresh_t in
      let xt_env = (x_env, t_env) in
      let ctx = ctx |> Ctx.add_item xs (IDef (info, [], [], vts @ [xt_env], t, [], ([], e))) in
      let (ef, ctx) = ctx |> Ctx.new_expr (fun t -> EItem (info, t, xs, [])) in
      let (er, ctx) = ctx |> empty_expr_env info in
      let fs = [("f", ef); ("r", er)] in
      ctx |> Ctx.new_expr (fun t -> ERecord (info, t, (fs, None)))

and lower_indirect_call info e es ctx =
  let (e, ctx) = lower_expr e ctx in
  let (es, ctx) = es |> mapm lower_expr_arg ctx in
  let (ef, ctx) = ctx |> Ctx.new_expr (fun t -> EAccessRecord (info, t, e, "f")) in
  let (er, ctx) = ctx |> Ctx.new_expr (fun t -> EAccessRecord (info, t, e, "r")) in
  ctx |> Ctx.new_expr (fun t -> ECallExpr (info, t, ef, es @ [er]))

and lower_direct_call is_operator info xs ts es ctx =
  let (es, ctx) =
    if is_operator then
      es |> mapm lower_expr ctx
    else
      es |> mapm lower_expr_arg ctx
  in
  ctx |> Ctx.new_expr (fun t -> ECallItem (info, t, xs, ts, es))

and lower_async_call info xs ts es ctx =
  let (es, ctx) = es |> mapm lower_expr_arg ctx in
  ctx |> Ctx.new_expr (fun t -> ECallItem (info, t, xs, ts, es))

and lower_type_args xs ts gs ctx =
  let n = gs |> List.length in
  match List.length ts with
  | m when m = n -> ts |> mapm lower_type ctx
  | m when m = 0 -> ctx |> Ctx.fresh_ts n
  | m -> panic (Printf.sprintf "Path `%s` has wrong number of type arguments, expected %d but found %d" (Print.path_to_str xs) n m)

and lower_call is_operator info e args ctx =
  match e with
  | Ast.EPath (_, xs, ts) ->
      let resolve_call_path xs ctx =
        begin match ctx |> Ctx.resolve_path info xs with
        | (xs, Graph.NItem IDef (_, _, _, gs, _, _, _, _))
        | (xs, Graph.NItem IAbstractDef (_, _, _, _, gs, _, _, _, _)) ->
            let (ts, ctx) = lower_type_args xs ts gs ctx in
            lower_direct_call is_operator info xs ts args ctx
        | _ ->
            unreachable ()
        end
      in
      begin match xs with
      | Ast.PRel [x] when ts = [] ->
          begin match ctx |> Ctx.find_vname_opt x with
          | Some (_, Ctx.MVal) -> lower_indirect_call info e args ctx
          | Some (v, Ctx.MVar) ->
              let (e, ctx) = ctx |> Ctx.new_expr (fun t -> EVar (info, t, v)) in
              get_cell_expr info e ctx
          | None -> ctx |> resolve_call_path xs
          end
      | _ -> ctx |> resolve_call_path xs
      end
  | _ -> lower_indirect_call info e args ctx

and lower_expr_item_func info xs ts gs ctx =
  let (ts, ctx) = lower_type_args xs ts gs ctx in
  let (ef, ctx) = ctx |> Ctx.new_expr (fun t -> EItem (info, t, xs, ts)) in
  let (er, ctx) = ctx |> empty_expr_env info in
  ctx |> Ctx.new_expr (fun t -> ERecord (info, t, ([("f", ef); ("r", er)], None)))

and lower_expr_item_path info xs ts ctx =
  let (xs, decl) = ctx |> Ctx.resolve_path info xs in
  match decl with
  | Graph.NItem Ast.IDef (_, _, _, gs, _, _, _, _)
  | Graph.NItem Ast.IAbstractDef (_, _, _, _, gs, _, _, _, _) ->
      ctx |> lower_expr_item_func info xs ts gs
  | Graph.NItem Ast.IVal _ ->
      ctx |> Ctx.new_expr (fun t -> EItem (info, t, xs, []))
  | Graph.NMethodDecl _
  | Graph.NItem _ ->
      raise (Error.NamingError (info, "Found non-expr where expr was expected"))

(* Resolves a path expression *)
and lower_expr_path info xs ts ctx =
  match xs with
  | Ast.PRel [x] when ts = [] ->
      begin match ctx |> Ctx.find_vname_opt x with
      | Some (v, Ctx.MVal) ->
          ctx |> Ctx.new_expr (fun t -> EVar (info, t, v))
      | Some (v, Ctx.MVar) ->
          let (e, ctx) = ctx |> Ctx.new_expr (fun t -> EVar (info, t, v)) in
          get_cell_expr info e ctx
      | None -> ctx |> lower_expr_item_path info xs ts
      end
  | _ -> ctx |> lower_expr_item_path info xs ts

and lower_type_item_path info xs ts ctx =
  let (xs, decl) = ctx |> Ctx.resolve_path info xs in
  match decl with
  | Graph.NItem Ast.IAbstractDef _
  | Graph.NItem Ast.IDef _
  | Graph.NItem Ast.IVal _
  | Graph.NItem Ast.IMod _
  | Graph.NMethodDecl _ ->
      raise (Error.NamingError (info, "Found non-type where type was expected"))
  | Graph.NItem Ast.IType (_, _, _, gs, _, _)
  | Graph.NItem Ast.IClass (_, _, _, gs, _, _)
  | Graph.NItem Ast.IAbstractType (_, _, _, _, gs, _) ->
      let (ts, ctx) = lower_type_args xs ts gs ctx in
      (TNominal (xs, ts), ctx)
  | _ ->
      unreachable ()

and lower_type_path info xs ts ctx =
  match xs with
  | Ast.PRel [x] when ts = [] ->
      begin match ctx |> Ctx.find_gname_opt x with
      | Some x -> (TGeneric x, ctx)
      | None -> ctx |> lower_type_item_path info xs ts
      end
  | _ -> ctx |> lower_type_item_path info xs ts

and lower_expr_opt info e ctx =
  match e with
  | Some e -> lower_expr e ctx
  | None -> ctx |> Ctx.new_expr (fun t -> ELit (info, t, LUnit gen))

and lower_mut info e0 e1 ctx =
  let (e1, ctx) = ctx |> lower_expr e1 in
  match e0 with
  | Ast.EPath (info, Ast.PRel [x], []) ->
      begin match ctx |> Ctx.find_vname info x with
      | (x, Ctx.MVar) ->
          let (e0, ctx) = ctx |> Ctx.new_expr (fun t -> EVar (info, t, x)) in
          ctx |> set_cell_expr info e0 e1
      | (_, Ctx.MVal) -> raise (Error.NamingError (info, "L-value is not a L-value"))
      end
  | Ast.EAccessArray (_, e0, es) ->
      let (e0, ctx) = lower_expr e0 ctx in
      let (es, ctx) = es |> mapm lower_expr ctx in
      ctx |> mut_array_expr info e0 es e1
  | Ast.EAccessTuple (_, e00, i) ->
      let (e00, ctx) = lower_expr e00 ctx in
      ctx |> Ctx.new_expr (fun t -> EUpdateRecord (info, t, e00, index_to_field i, e1))
  | Ast.EAccessRecord (_, e00, x) ->
      let (e00, ctx) = lower_expr e00 ctx in
      ctx |> Ctx.new_expr (fun t -> EUpdateRecord (info, t, e00, x, e1))
  | _ -> panic "Expected variable, found path"

and lower_expr expr ctx =
  match expr with
  | Ast.ESet _ -> todo ()
  | Ast.EDict _ -> todo ()
  | Ast.EAccessRecordMulti (_, _, _) -> todo ()
  | Ast.ESliceRecord (_, _, _) -> todo ()
  | Ast.EDynRecord (_, _) -> todo ()
  | Ast.ESource (_, _, _) -> todo ()
  | Ast.EAnon _ ->
      let (x, ctx) = ctx |> Ctx.add_anon in
      ctx |> Ctx.new_expr (fun t -> EVar (Ast.expr_info expr, t, x))
  (* TODO: REMOVE *)
  | Ast.EBinOpRef (info, op) ->
      let x = Ast.binop_name op in
      let xs = ["std"; indirect_name x] in
      ctx |> lower_expr_item_func info xs [] []
  | Ast.EAccessRecord (info, e, x) ->
      let (e, ctx) = lower_expr e ctx in
      ctx |> Ctx.new_expr (fun t -> EAccessRecord (info, t, e, x))
  | Ast.EArray (info, es, e) ->
      let (es, ctx) = es |> mapm lower_expr ctx in
      let (e0, ctx) = ctx |> make_array_expr info es in
      begin match e with
      | None ->
          (e0, ctx)
      | Some e ->
          let (v1, ctx) = lower_expr e ctx in
          ctx |> append_array_expr info e0 v1
      end
  | Ast.EBinOp (info, Ast.BMut, _, e0, e1) ->
      lower_mut info e0 e1 ctx
  | Ast.EBinOp (info, Ast.BMutAdd, ts, e0, e1) ->
      lower_mut info e0 (Ast.EBinOp (info, Ast.BAdd, ts, e0, e1)) ctx
  | Ast.EBinOp (info, Ast.BMutSub, ts, e0, e1) ->
      lower_mut info e0 (Ast.EBinOp (info, Ast.BSub, ts, e0, e1)) ctx
  | Ast.EBinOp (info, Ast.BMutMul, ts, e0, e1) ->
      lower_mut info e0 (Ast.EBinOp (info, Ast.BMul, ts, e0, e1)) ctx
  | Ast.EBinOp (info, Ast.BMutDiv, ts, e0, e1) ->
      lower_mut info e0 (Ast.EBinOp (info, Ast.BDiv, ts, e0, e1)) ctx
  | Ast.EBinOp (info, Ast.BMutMod, ts, e0, e1) ->
      lower_mut info e0 (Ast.EBinOp (info, Ast.BMod, ts, e0, e1)) ctx
  | Ast.EBinOp (info, Ast.BNotIn, ts, e0, e1) ->
      lower_expr (Ast.EUnOp (info, Ast.UNot, [], (Ast.EBinOp (info, Ast.BIn, ts, e0, e1)))) ctx
  | Ast.EBinOp (info, Ast.BNeq, ts, e0, e1) ->
      lower_expr (Ast.EUnOp (info, Ast.UNot, [], (Ast.EBinOp (info, Ast.BEq, ts, e0, e1)))) ctx
  | Ast.EBinOp (info, op, ts, e0, e1) ->
      let (x, ctx) = lower_binop op ctx in
      ctx |> lower_call true info (Ast.EPath (info, Ast.PRel [x], ts)) [e0; e1]
  | Ast.EUnOp (info, op, ts, e) ->
      let (x, ctx) = lower_unop op ctx in
      ctx |> lower_call true info (Ast.EPath (info, Ast.PRel [x], ts)) [e]
  | Ast.ECall (info, e, args) ->
      ctx |> lower_call false info e args
  | Ast.ECallItem (info, e, x, args) ->
      ctx |> lower_call false info (Ast.EPath (info, Ast.PRel [x], [])) ([e] @ args)
  | Ast.EAnnot (info, e, t) ->
      let (e, ctx) = lower_expr e ctx in
      let (t, ctx) = lower_type t ctx in
      (EAnnot (info, t, e), ctx)
  | Ast.EIf (info, e, b0, b1) ->
      let (e, ctx) = lower_expr e ctx in
      let (b0, ctx) = lower_block b0 ctx in
      let (b1, ctx) = lower_block_opt b1 ctx in
      let (arm0, ctx) = ctx |> Ctx.new_pat (fun t -> (PConst (info, t, LBool (info, true)), b0)) in
      let (arm1, ctx) = ctx |> Ctx.new_pat (fun t -> (PIgnore (info, t), b1)) in
      ctx |> Ctx.new_expr (fun t -> EMatch (info, t, e, [arm0; arm1]))
  | Ast.ELit (_, l) ->
      lower_lit l ctx
  | Ast.ELoop (info, b) ->
      let (b, ctx) = ctx |> lower_block b in
      ctx |> Ctx.new_expr (fun t -> ELoop (info, t, b))
  | Ast.EAccessArray (info, e, es) ->
      let (e0, ctx) = ctx |> lower_expr e in
      let (e1, ctx) = es |> mapm lower_expr ctx in
      ctx |> get_array_expr info e0 e1
  | Ast.ERecord (info, (xes, e)) ->
      let (xes, ctx) = xes |> mapm (lower_field_expr info) ctx in
      let (e, ctx) = ctx |> lower_expr_tail e in
      ctx |> Ctx.new_expr (fun t -> ERecord (info, t, (xes, e)))
  | Ast.EVariant (info, (x, e)) ->
      let (e, ctx) = ctx |> lower_expr e in
      ctx |> Ctx.new_expr (fun t -> EEnwrap (info, t, x, e))
  | Ast.EReturn (info, e) ->
      let (e, ctx) = lower_expr_opt info e ctx in
      ctx |> Ctx.new_expr (fun t -> EReturn (info, t, e))
  | Ast.EBreak (info, e) ->
      let (e, ctx) = lower_expr_opt info e ctx in
      ctx |> Ctx.new_expr (fun t -> EBreak (info, t, e))
  | Ast.EContinue info ->
      ctx |> Ctx.new_expr (fun t -> EContinue (info, t))
  (* Desugared expressions *)
  | Ast.ETuple (info, es) ->
      let (es, ctx) = es |> mapm lower_expr ctx in
      let xes = es |> indexes_to_fields in
      ctx |> Ctx.new_expr (fun t -> ERecord (info, t, (xes, None)))
  | Ast.EAccessTuple (info, e, i) ->
      let (e, ctx) = lower_expr e ctx in
      ctx |> Ctx.new_expr (fun t -> EAccessRecord (info, t, e, index_to_field i))
  | Ast.EBlock (_, b) ->
      let ((ss, e), ctx) = lower_block b ctx in
      let ctx = ctx |> Ctx.add_stmts ss in
      (e, ctx)
  | Ast.EFunc (info, ps, e) ->
      lower_closure info ps e ctx
  | Ast.EFor (_loc, _p, _e, _b) ->
      todo ()
  | Ast.EWhile (info, e, b) ->
      let (e0, ctx) = ctx |> lower_expr e in
      (* Then-branch *)
      let (b0, ctx) = ctx |> lower_block b in
      (* Else-branch *)
      let (e1, ctx) = ctx |> Ctx.new_expr (fun t -> ELit (info, t, LUnit info)) in
      let (e1, ctx) = ctx |> Ctx.new_expr (fun t -> EBreak (info, t, e1)) in
      let b1 = ([], e1) in
      (* If-stmt *)
      let (arm0, ctx) = ctx |> Ctx.new_pat (fun t -> (PConst (info, t, LBool (info, true)), b0)) in
      let (arm1, ctx) = ctx |> Ctx.new_pat (fun t -> (PIgnore (info, t), b1)) in
      let (e2, ctx) = ctx |> Ctx.new_expr (fun t -> EMatch (info, t, e0, [arm0; arm1])) in
      ctx |> Ctx.new_expr (fun t -> ELoop (info, t, ([], e2)))
  | Ast.EWhileVal _ ->
      todo ()
  | Ast.EIfVal (info, p, e, b0, b1) ->
      let ctx = ctx |> Ctx.push_vscope in
      let (e, ctx) = ctx |> lower_expr e in
      let (p0, ctx) = ctx |> lower_pat p in
      let (b0, ctx) = ctx |> lower_block b0 in
      let (b1, ctx) = ctx |> lower_block_opt b1 in
      let (es, ctx) = ctx |> Ctx.pop_vscope in
      let ctx = ctx |> Ctx.add_stmts es in
      let (p1, ctx) = ctx |> Ctx.new_pat (fun t -> (PIgnore (info, t))) in
      let arms = [(p0, b0); (p1, b1)] in
      ctx |> Ctx.new_expr (fun t -> EMatch (info, t, e, arms))
  | Ast.EMatch (info, e, arms) ->
      let (e, ctx) = lower_expr e ctx in
      let (arms, ctx) = arms |> mapm lower_arm ctx in
      ctx |> Ctx.new_expr (fun t -> EMatch (info, t, e, arms))
  | Ast.EPath (info, xs, ts) ->
      ctx |> lower_expr_path info xs ts
  | Ast.EFrom _ ->
      todo ()
  | Ast.EThrow _ -> todo ()
  | Ast.ETry _ -> todo ()

and lower_expr_tail t ctx =
  match t with
  | Some t ->
      let (t, ctx) = lower_expr t ctx in
      (Some t, ctx)
  | None ->
      (None, ctx)

and lower_pat_tail p ctx =
  match p with
  | Some p ->
      let (p, ctx) = lower_pat p ctx in
      (Some p, ctx)
  | None ->
      (None, ctx)

and lower_arm (p, e) ctx =
  let ctx = ctx |> Ctx.push_vscope in
  let (p, ctx) = lower_pat p ctx in
  let (e, ctx) = lower_expr e ctx in
  let (es, ctx) = ctx |> Ctx.pop_vscope in
  ((p, (es, e)), ctx)

and lower_unop op ctx =
  let x = Ast.unop_name op in
  (x, ctx)

and lower_field_expr info field ctx =
  match field with
  | Ast.FName (x, e) ->
    begin match e with
    | Some e ->
      let (e, ctx) = lower_expr e ctx in
      ((x, e), ctx)
    | None ->
        match ctx |> Ctx.find_vname_opt x with
        | Some (v, MVal) ->
            let (e, ctx) = ctx |> Ctx.new_expr (fun t -> EVar (info, t, v)) in
            ((x, e), ctx)
        | Some (e, MVar) ->
            let (e, ctx) = ctx |> Ctx.new_expr (fun t -> EVar (info, t, e)) in
            let (v, ctx) = get_cell_expr info e ctx in
            ((x, v), ctx)
        | None -> panic "Name not found"
    end
  | Ast.FExpr (e, x) ->
      let (e, ctx) = lower_expr e ctx in
      let (e, ctx) = ctx |> Ctx.new_expr (fun t -> EAccessRecord (info, t, e, x)) in
      ((x, e), ctx)

and lower_field_type (x, t) ctx =
  match t with
  | Some t ->
    let (t, ctx) = lower_type t ctx in
    ((x, t), ctx)
  | None ->
    let (t, ctx) = ctx |> Ctx.fresh_t in
    ((x, t), ctx)

and lower_variant_type (x, t) ctx =
  let (t, ctx) = lower_type t ctx in
  ((x, t), ctx)

and lower_type_or_fresh t ctx =
  match t with
  | Some t -> lower_type t ctx
  | None -> ctx |> Ctx.fresh_t

and lower_type_or_unit t ctx =
  match t with
  | Some t -> lower_type t ctx
  | None -> (atom "unit", ctx)

and lower_type t ctx =
  match t with
  | Ast.TFunc (info, ts, t) ->
      let (ts, ctx) = ts |> mapm lower_type ctx in
      let (t, ctx) = lower_type t ctx in
      let (tr, ctx) = empty_type_env info ctx in
      let tf = TFunc (ts @ [tr], t) in
      let tc = TRecord ([("r", tr); ("f", tf)] |> fields_to_rows TRowEmpty) in
      (tc, ctx)
  | Ast.TTuple (_, ts) ->
      let (ts, ctx) = ts |> mapm lower_type ctx in
      let t = types_to_record ts in
      (t, ctx)
  | Ast.TRecord (_, (xts, t)) ->
      let (xts, ctx) = xts |> mapm lower_field_type ctx in
      let (t, ctx) = match t with
      | Some t -> lower_type t ctx
      | None -> (TRowEmpty, ctx)
      in
      let t = TRecord (xts |> fields_to_rows t) in
      (t, ctx)
  | Ast.TEnum (_, (xts, t)) ->
      let (xts, ctx) = xts |> mapm lower_variant_type ctx in
      let (t, ctx) = match t with
      | Some t -> lower_type t ctx
      | None -> (TRowEmpty, ctx)
      in
      let t = TEnum (xts |> fields_to_rows t) in
      (t, ctx)
  | Ast.TPath (info, xs, ts) ->
      ctx |> lower_type_path info xs ts
  | Ast.TArray (_, t) ->
      let (t, ctx) = lower_type t ctx in
      (nominal "Array" [t], ctx)
  | Ast.TUnit _ ->
      (atom "unit", ctx)
  | Ast.TNever _ ->
      (atom "never", ctx)

(* Lowers an irrefutable pattern matching on variable v, e.g., val p = v; *)
and lower_pat p ctx : (Ir1.pattern * Ctx.t)=
  match p with
  | Ast.PIgnore info ->
      let (t, ctx) = ctx |> Ctx.fresh_t in
      (PIgnore (info, t), ctx)
  | Ast.PRecord (info, (xps, p)) ->
      let (xps, ctx) = xps |> mapm (fun (x, p) ctx ->
        match p with
        | Some p ->
            let (p, ctx) = lower_pat p ctx in
            ((x, p), ctx)
        | None ->
            let (x, ctx) = ctx |> Ctx.bind_vname x MVal in
            let (p, ctx) = ctx |> Ctx.new_pat (fun t -> PVar (info, t, x)) in
            ((x, p), ctx)
      ) ctx in
      let (t, ctx) = ctx |> Ctx.fresh_t in
      let (p, ctx) = ctx |> lower_pat_tail p in
      (PRecord (info, t, (xps, p)), ctx)
  | Ast.PAnnot (info, p, t) ->
      let (p, ctx) = lower_pat p ctx in
      let (t, ctx) = lower_type t ctx in
      (PAnnot (info, t, p), ctx)
  | Ast.PTuple (info, ps) ->
      let xps = ps |> map (fun p -> Some p) |> indexes_to_fields in
      let p = (Ast.PRecord (info, (xps, None))) in
      lower_pat p ctx
  | Ast.PArray (_loc, _ps, _p) ->
      todo()
  | Ast.PVar (info, x) ->
      let (x, ctx) = ctx |> Ctx.bind_vname x MVal in
      let (t, ctx) = ctx |> Ctx.fresh_t in
      (PVar (info, t, x), ctx)
  | Ast.PVariant (info, x, p) ->
      let (p, ctx) = lower_pat p ctx in
      let (t, ctx) = ctx |> Ctx.fresh_t in
      (PUnwrap (info, t, x, p), ctx)
  | Ast.POr (info, p0, p1) ->
      let (p0, ctx) = lower_pat p0 ctx in
      let (p1, ctx) = lower_pat p1 ctx in
      let (t, ctx) = ctx |> Ctx.fresh_t in
      (POr (info, t, p0, p1), ctx)
  | Ast.PConst (info, c) ->
      let l = lower_const c in
      let (t, ctx) = ctx |> Ctx.fresh_t in
      (PConst (info, t, l), ctx)

and lower_const c =
  match c with
  | Ast.CInt    (info, c) -> LInt (info, c)
  | Ast.CFloat  (info, c) -> LFloat (info, c)
  | Ast.CBool   (info, c) -> LBool (info, c)
  | Ast.CString (info, c) -> LString (info, c)
  | Ast.CUnit   (info) -> LUnit info
  | Ast.CChar   (info, c) -> LChar (info, c)

and lower_block (ss, expr) ctx =
  let rec lower_stmts ss ctx =
    match ss with
    | s::ss ->
        begin match s with
        | Ast.SNoop _ ->
            ctx |> lower_stmts ss
        | Ast.SVal (info, p, e0) ->
            let (e0, ctx) = ctx |> lower_expr e0 in
            let (p, ctx) = ctx |> lower_pat p in
            let ctx = ctx |> Ctx.push_vscope in
            let (e1, ctx) = ctx |> lower_stmts ss in
            let (es, ctx) = ctx |> Ctx.pop_vscope in
            ctx |> Ctx.new_expr (fun t1 -> EMatch (info, t1, e0, [(p, (es, e1))]))
        | Ast.SVar (info, (x, t), e0) ->
            let (e0, ctx) = ctx |> lower_expr e0 in
            let (e0, ctx) = ctx |> new_cell_expr info e0 in
            let (t, ctx) = ctx |> lower_type_or_fresh t in
            let t = cell_type t in
            let (x, ctx) = ctx |> Ctx.bind_vname x MVar in
            let p = PVar (info, t, x) in
            let ctx = ctx |> Ctx.push_vscope in
            let (e1, ctx) = ctx |> lower_stmts ss in
            let (es, ctx) = ctx |> Ctx.pop_vscope in
            ctx |> Ctx.new_expr (fun t1 -> EMatch (info, t1, e0, [(p, (es, e1))]))
        | Ast.SExpr (info, e) ->
            begin match e with
            | Ast.EReturn _ | Ast.EBreak _ | Ast.EContinue _ ->
                begin match (ss, expr) with
                | ([], None) -> ctx |> lower_expr e
                | _ -> raise (Error.TypingError (info, "Found unreachable code beyond this point"))
                end
            | _ ->
              let (e, ctx) = ctx |> lower_expr e in
              let ctx = ctx |> Ctx.add_stmt_expr e in
              ctx |> lower_stmts ss
            end
        end
    | [] ->
        begin match expr with
        | Some e -> ctx |> lower_expr e
        | None -> ctx |> Ctx.new_expr (fun t -> ELit (gen, t, LUnit gen))
        end
  in
  let ctx = ctx |> Ctx.push_vscope in
  let (e, ctx) = ctx |> lower_stmts ss in
  let (ss, ctx) = ctx |> Ctx.pop_vscope in
  ((ss, e), ctx)

and lower_block_opt b ctx =
  match b with
  | Some b ->
      ctx |> lower_block b
  | None ->
      ctx |> empty_block gen

and lower_binop op ctx =
  let x = Ast.binop_name op in
  (x, ctx)

and splice_regex = (Str.regexp "\\${[^}]+}\\|\\$[a-zA-Z_][a-zA-Z0-9_]*")

and new_string_expr info s ctx =
  let (e0, ctx) = ctx |> Ctx.new_expr (fun t -> ELit (info, t, LString (info, s))) in
  let (e1, ctx) = ctx |> Ctx.new_expr (fun t -> EItem (info, t, ["std"; "from_str"], [])) in
  let (v, ctx) = ctx |> Ctx.new_expr (fun t -> ECallExpr (info, t, e1, [e0])) in
  (v, ctx)

and lower_lit l ctx =
  match l with
  (* Lower interpolated string literals *)
  | Ast.LString (info, s) ->
      let (es, ctx) = s |> Str.full_split splice_regex
        |> mapm (fun s ctx ->
          match s with
          | Str.Text s ->
              new_string_expr info s ctx
          | Str.Delim s ->
              let s = String.sub s 1 ((String.length s) - 1) in
              let e = Parser.expr Lexer.token (Lexing.from_string s) in
              let ctx = ctx |> Ctx.push_vscope in
              let (e, ctx) = ctx |> lower_expr e in
              let (es, ctx) = ctx |> Ctx.pop_vscope in
              let ctx = ctx |> Ctx.add_stmts es in
              (e, ctx)
        ) ctx in
      begin match es with
      | v::vs ->
        vs |> foldl (fun (v1, ctx) v2 ->
          let (v0, ctx) = ctx |> Ctx.new_expr (fun t -> EItem (info, t, ["std"; "concat"], [])) in
          ctx |> Ctx.new_expr (fun t -> ECallExpr (info, t, v0, [v1; v2]))
        ) (v, ctx)
      | [] -> new_string_expr info s ctx
      end
  | Ast.LBool (info, l) ->
      ctx |> Ctx.new_expr (fun t -> ELit (gen, t, LBool (info, l)))
  | Ast.LChar (info, l) ->
      ctx |> Ctx.new_expr (fun t -> ELit (gen, t, LChar (info, l)))
  | Ast.LFloat (info, l, s) ->
      begin match s with
      | Some x ->
          let (e0, ctx) = ctx |> Ctx.new_expr (fun t -> ELit (gen, t, LFloat (info, l))) in
          ctx |> Ctx.new_expr (fun t -> ECallItem (gen, t, ["i" ^ x], [], [e0]))
      | None ->
          ctx |> Ctx.new_expr (fun t -> ELit (gen, t, LFloat (info, l)))
      end
  | Ast.LInt (info, l, s) ->
      begin match s with
      | Some x ->
          let (e0, ctx) = ctx |> Ctx.new_expr (fun t -> ELit (gen, t, LInt (info, l))) in
          ctx |> Ctx.new_expr (fun t -> ECallItem (gen, t, ["i" ^ x], [], [e0]))
      | None -> ctx |> Ctx.new_expr (fun t -> ELit (gen, t, LInt (info, l)))
      end
  | Ast.LUnit info ->
      ctx |> Ctx.new_expr (fun t -> ELit (gen, t, LUnit (info)))

and lower_closure info ps b ctx =

  (* Print.Ast.pr_block b Print.Ctx.brief; *)

  (* Compile the closure body *)
  let ctx = ctx |> Ctx.push_vscope in
  let (ps, ctx) = ps |> mapm lower_param ctx in
  let ((ss0, e), ctx) = lower_block b ctx in
  let (ss1, ctx) = ctx |> Ctx.pop_vscope in
  let b = (ss1 @ ss0, e) in
  (* Print.Ir1.pr_block b Print.Ctx.brief; *)

  (* Create the variables which need to be stored in the environment *)
  let fvs = free_vars (ps |> bound_vars) b in

  (* Create an extra function parameter for the environment *)
  let (v_env, ctx) = ctx |> Ctx.fresh_x in
  let (t_env, ctx) = ctx |> Ctx.fresh_t in

  (* Create the function signature *)
  let ts = ps |> map Ir1.typeof_pat in
  let (vs, ctx) = ps |> mapm (fun _ ctx -> Ctx.fresh_x ctx) ctx in
  let vts = (zip vs ts) @ [(v_env, t_env)] in

  (* Create code for unpacking the environment inside the closure *)
  let (es, ctx) = ctx |> vars_to_exprs info vs in
  let (e_env, ctx) = ctx |> Ctx.new_expr (fun t -> EVar (info, t, v_env)) in
  let (e, ctx) = ctx |> exprs_to_record info (es @ [e_env]) in

  let (ps_env, ctx) = ctx |> vars_to_patterns info fvs in
  let (p_env, ctx) = ctx |> patterns_to_record info ps_env in
  let (p, ctx) = ctx |> patterns_to_record info (ps @ [p_env]) in

  let (e, ctx) = ctx |> Ctx.new_expr (fun t -> EMatch (info, t, e, [(p, b)])) in

  (* Create the function *)
  let (xs, ctx) = ctx |> Ctx.fresh_f in
  let (t, ctx) = ctx |> Ctx.fresh_t in
  let ctx = ctx |> Ctx.add_item xs (IDef (info, [], [], vts, t, [], ([], e))) in

  (* Create the function pointer *)
  let (e_fun, ctx) = ctx |> Ctx.new_expr (fun t -> EItem (info, t, xs, [])) in

  (* Printf.eprintf "\n"; *)
  (* Printf.eprintf "B-vars: "; *)
  (* bvs |> List.iter (fun v -> Printf.eprintf "%s, " v); *)
  (* Printf.eprintf "\n"; *)
  (* Printf.eprintf "F-vars: "; *)
  (* fvs |> List.iter (fun v -> Printf.eprintf "%s, " v); *)
  (* Printf.eprintf "\n"; *)
  let (es, ctx) = ctx |> vars_to_exprs info fvs in
  let (e_env, ctx) = ctx |> exprs_to_record info es in
  ctx |> Ctx.new_expr (fun t -> ERecord (info, t, ([("f", e_fun); ("r", e_env)], None)))

(* Create a new cell *)
and new_cell_expr info e ctx =
  let (t, ctx) = ctx |> Ctx.fresh_t in
  ctx |> Ctx.new_expr (fun t1 -> ECallItem (info, t1, ["std"; "new_cell"], [t], [e]))

and cell_type t =
  nominal "Cell" [t]

(* Retrieve the value from a cell *)
and get_cell_expr info e ctx =
  let (t, ctx) = ctx |> Ctx.fresh_t in
  ctx |> Ctx.new_expr (fun t1 -> ECallItem (info, t1, ["std"; "get_cell"], [t], [e]))

(* Update the value inside a cell, e.g., `x = 1` *)
and set_cell_expr info e0 e1 ctx =
  let (t, ctx) = ctx |> Ctx.fresh_t in
  ctx |> Ctx.new_expr (fun t1 -> ECallItem (info, t1, ["std"; "set_cell"], [t], [e0; e1]))

(* Create an empty block, i.e., `{}` *)
and empty_block info ctx =
  let (v, ctx) = ctx |> Ctx.new_expr (fun t -> ELit (info, t, LUnit info)) in
  (([], v), ctx)

(* Create an empty environment expr, i.e., `{}` *)
and empty_expr_env info ctx =
  lower_expr (Ast.ERecord (info, ([], None))) ctx

(* Create an empty environment type, i.e., `{}` *)
and empty_type_env info ctx =
  lower_type (Ast.TRecord (info, ([], None))) ctx

(* Create an array constructor expr, i.e., `[e0, e1, e2]` *)
and make_array_expr info es ctx =
  let (t, ctx) = ctx |> Ctx.fresh_t in
  let (v0, ctx) = ctx |> Ctx.new_expr (fun t1 -> ECallItem (info, t1, ["std"; "array"], [t], [])) in
  let ctx = es |> foldl (fun ctx v1 -> ctx |> Ctx.new_expr (fun t1 -> ECallItem (info, t1, ["std"; "push_back"], [t], [v0; v1])) |> snd) ctx in
  (v0, ctx)

(* Create an array constructor expr, i.e., `[e0, e1, e2]` *)
and append_array_expr info e0 e1 ctx =
  let (t, ctx) = ctx |> Ctx.fresh_t in
  ctx |> Ctx.new_expr (fun t1 -> ECallItem (info, t1, ["std"; "append"], [t], [e0; e1]))

and get_array_expr info e0 es ctx =
  let (t, ctx) = ctx |> Ctx.fresh_t in
  let (e1, ctx) = make_array_expr info es ctx in
  ctx |> Ctx.new_expr (fun t1 -> ECallItem (info, t1, ["std"; "get"], [t], [e0; e1]))

and mut_array_expr info e0 es e1 ctx =
  let (t, ctx) = ctx |> Ctx.fresh_t in
  let (es, ctx) = make_array_expr info es ctx in
  ctx |> Ctx.new_expr (fun t1 -> ECallItem (info, t1, ["std"; "replace"], [t], [e0; es; e1]))
