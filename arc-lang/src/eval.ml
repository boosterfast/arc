open Utils
open Error
open Ir3

exception Continue
exception Break of expr
exception Return of expr

module Ctx = struct
  type t = {
    istack: iscope list;
    ir3: ir3;
  }
  and iscope = {
    vstack: vscope list;
  }
  and vscope = {
    vsubsts: (name * expr) list;
  }

  let empty ir3 = { istack = []; ir3 }

  let push_iscope (ctx:t) (pes:(var * expr) list) =
    { ctx with istack = { vstack = [{ vsubsts = pes }] }::ctx.istack }

  let pop_iscope (ctx:t) =
    { ctx with istack = ctx.istack |> tl }

  let push_vscope (ctx:t) = match ctx.istack with
    | [] -> failwith "push_vscope: empty istack"
    | { vstack }::is ->
        { ctx with istack = { vstack = { vsubsts = []; }::vstack }::is }

  let pop_vscope (ctx:t) = match ctx.istack with
    | [] -> failwith "pop_vscope: empty istack"
    | { vstack }::is ->
        match vstack with
        | [] -> failwith "pop_vscope: empty vstack"
        | _::vs -> { ctx with istack = { vstack = vs }::is }

  let add_expr (x, v) (ctx:t) = match ctx.istack with
    | [] -> failwith "add_expr: empty istack"
    | { vstack }::is ->
        match vstack with
        | [] -> failwith "add_expr: empty vstack"
        | { vsubsts; }::vs ->
            { ctx with istack = { vstack = { vsubsts = (x, v)::vsubsts; }::vs }::is }

  let add_expr (x, v) (ctx:t) = match ctx.istack with
    | [] -> failwith "add_expr: empty istack"
    | { vstack }::is ->
        match vstack with
        | [] -> failwith "add_expr: empty vstack"
        | { vsubsts; }::vs ->
            { ctx with istack = { vstack = { vsubsts = (x, v)::vsubsts; }::vs }::is }


  let find_expr (x:name) (ctx:t) =
    let iscope = ctx.istack |> hd in
    iscope.vstack |> find_map (fun vscope -> assoc_opt x vscope.vsubsts) |> Option.get

  let find_item (xs:name list) (ts:ty list) (ctx:t) =
    ctx.ir3 |> List.assoc (xs, ts)

  let post_query (ctx:t) =
    let iscope = ctx.istack |> hd in
    let vscope = iscope.vstack |> hd in
    vscope.vsubsts
  
end

let rec eval (ir3:ir3) =
  let ctx = Ctx.empty ir3 in
  eval_expr (ECallItem (gen, ["main"], [], [])) ctx

and eval_expr (expr:expr) (ctx:Ctx.t): (expr * Ctx.t) =
  match expr with
  | EAccessRecord (_, v, x) ->
      begin match ctx |> Ctx.find_expr v with
      | ERecord (_, xvs) ->
          let v = List.assoc x xvs in
          let e = ctx |> Ctx.find_expr v in
          (e, ctx)
      | _ ->
          unreachable ()
      end
  | EBreak (_, v) ->
      let e = Ctx.find_expr v ctx in
      raise (Break e)
  | ECallExpr _ ->
      todo ()
  | ECallItem (_, xs, ts, vs) ->
      let i = ctx |> Ctx.find_item xs ts in
      let es = List.map (fun v -> ctx |> Ctx.find_expr v) vs in
      begin match i with
      | IDef (_, _, ps, _, b) ->
          let pes = zip (ps |> map fst) es in
          let ctx = Ctx.push_iscope ctx pes in
          let (e, ctx) =
            try 
              eval_block b ctx
            with
            | Return e ->
              (e, ctx)
          in
          let ctx = Ctx.pop_iscope ctx in
          (e, ctx)
      | IAbstractDef _ ->
          begin match xs, es with
          | ["add"], [ELit (info, LInt(_, i0)); ELit (_, LInt(_, i1))] ->
              let e = ELit (info, LInt(info, i0+i1)) in
              (e, ctx)
          | _ -> unreachable ()
          end
      | _ -> unreachable ()
      end
  | EAnnot _ ->
      unreachable ()
  | EContinue _ ->
      raise Continue
  | EEnwrap _ ->
      (expr, ctx)
  | EUnwrap (_, _, v0) ->
      begin match ctx |> Ctx.find_expr v0 with
      | EEnwrap (_, _, v1) ->
          let e = ctx |> Ctx.find_expr v1 in
          (e, ctx)
      | _ ->
          unreachable ()
      end
  | ECheck (_, x0, v0) ->
      begin match ctx |> Ctx.find_expr v0 with
      | EEnwrap (_, x1, _) ->
          (ELit (gen, LBool (gen, x0 = x1)), ctx)
      | _ ->
          unreachable ()
      end
  | EItem _ ->
      unreachable ()
  | EIf (_, v, b0, b1) ->
      begin match ctx |> Ctx.find_expr v with
      | ELit (_, LBool (_, true)) ->
          eval_block b0 ctx
      | ELit (_, LBool (_, false)) ->
          eval_block b1 ctx
      | _ -> unreachable ()
      end
  | ELit _ ->
      (expr, ctx)
  | ELoop (_, b) ->
      let rec loop ctx =
        let (_, ctx) = eval_block b ctx in
        loop ctx
      in
      begin try
        loop ctx
      with
      | Break e ->
        (e, ctx)
      | Continue ->
        loop ctx
      end
  | ERecord _ ->
      (expr, ctx)
  | EReturn (_, v) ->
      let e = Ctx.find_expr v ctx in
      raise (Return e)
  | EUpdateRecord (_, _v0, _x, _v1) ->
      todo ()

and eval_block ((stmts, var):block) ctx =
  let ctx = ctx |> Ctx.push_vscope in
  let (e, ctx) = match stmts with
  | s::ss ->
      let ctx = eval_stmt s ctx in
      eval_block (ss, var) ctx
  | [] ->
      let e = ctx |> Ctx.find_expr var in
      (e, ctx)
  in
  let ctx = ctx |> Ctx.pop_vscope in
  (e, ctx)

and eval_stmt stmt ctx =
  match stmt with
  | SVal (x, _, e) ->
      let (e, ctx) = eval_expr e ctx in
      ctx |> Ctx.add_expr (x, e)
