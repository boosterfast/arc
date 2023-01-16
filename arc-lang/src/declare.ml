open Utils

module Ctx = struct

  type t = {
    graph: Graph.t
  }

  let make = {
    graph = Graph.make
  }

  and add_node info (xs, d0) ctx =
    {graph=ctx.graph |> Graph.add info xs (Graph.DNode d0)}

  and add_edge info (xs_alias, xs_aliased) ctx =
    {graph=ctx.graph |> Graph.add info xs_alias (Graph.DEdge xs_aliased)}

end

(* [declare] takes an ast and returns a list of top-level declarations *)
let rec declare ast =
  let ctx = Ctx.make in
  let xs = [] in
  let (ctx:Ctx.t) = ast |> List.fold_left (fun ctx i -> ctx |> declare_item i xs) ctx in
  ctx.graph

(* Declare an item *)
and declare_item i xs ctx =
  match i with
  (* | Ast.INoop _ -> ctx *)
  | Ast.IVal (info, _, x, _, _) ->
      Ctx.add_node info (x::xs, Graph.NItem i) ctx
  | Ast.IAbstractDef (info, _, _, d, _, _, _, _, _) ->
      let x = Ast.def_name d in
      Ctx.add_node info (rev (x::xs), Graph.NItem i) ctx
  | Ast.IAbstractType (info, _, _, x, _, _) ->
      Ctx.add_node info (rev (x::xs), Graph.NItem i) ctx
  | Ast.IDef (info, _, d, _, _, _, _, _) ->
      let x = Ast.def_name d in
      Ctx.add_node info (rev (x::xs), Graph.NItem i) ctx
  | Ast.IClass (info, _, x, _, _, decls) ->
      let ctx = ctx |> Ctx.add_node info (rev (x::xs), Graph.NItem i) in
      decls |> foldl (fun ctx (x, _, _, _, _) -> Ctx.add_node info (rev (x::xs), Graph.NItem i) ctx) ctx
  | Ast.IInstance _ -> ctx
  | Ast.IType (info, _, x, _, _, _) ->
      Ctx.add_node info (rev (x::xs), Graph.NItem i) ctx
  | Ast.IMod (_, _, x, is) ->
      is |> List.fold_left (fun ctx i -> declare_item i (x::xs) ctx) ctx
  | Ast.IUse (info, _, xs_aliased, alias) ->
      let xs_aliased = match xs_aliased with
      | Ast.PRel xs_aliased -> (rev xs) @ xs_aliased
      | Ast.PAbs xs_aliased -> xs_aliased
      in
      let xs_alias = match alias with
      | Some Ast.UAlias x -> rev (x::xs)
      | Some Ast.UGlob -> todo ()
      | None -> rev ((last xs_aliased)::xs)
      in
      Ctx.add_edge info (xs_alias, xs_aliased) ctx
  | Ast.IFrom _ -> ctx
