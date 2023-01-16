open Lexing
open Printf
open Utils
open Error

(* Converts a file to a module *)
let parse (modname, filepath, inx) =
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filepath };
  try
    let items = Parser.program Lexer.token lexbuf in
    Ast.IMod (gen, [], modname, items)
  with
  | Error.ParsingError (info, msg) -> Error.report info msg
  | Error.LexingError (info, msg) -> Error.report info msg
  | Parser.Error -> Error.report (Lexer.info lexbuf) "Parser Error"

(* Converts an AST to MLIR *)
let compile ast =
  try
    if !Args.show_parsed then Print.Ast.pr_ast ast;

    let graph = Declare.declare ast in
    if !Args.show_declared then Graph.debug graph;

    let ir1 = Ast_to_ir1.ast_to_ir1 graph ast in
    if !Args.show_desugared then Print.Ir1.pr_ir1 ir1;

    let ir1 = Infer_ir1.infer_ir1 ir1 in
    if !Args.show_inferred then Print.Ir1.pr_ir1 ir1;

    let ir2 = Ir1_to_ir2.ir1_to_ir2 ir1 in
    if !Args.show_patcomped then Print.Ir2.pr_ir2 ir2;

    let ir3 = Ir2_to_ir3.ir2_to_ir3 ir2 in
    if !Args.show_monomorphised then Print.Ir3.pr_ir3 ir3;

    let mlir = Ir3_to_mlir.ir3_to_mlir ir3 in
    if !Args.show_mlir then Print.Mlir.pr_mlir mlir;
    ()
  with
  | Error.TypingError (info, msg) -> Error.report info msg
  | Error.NamingError (info, msg) -> Error.report info msg

let filepath_to_modname path =
  path
    |> Filename.basename
    |> Filename.remove_extension
    |> Str.global_replace (Str.regexp "-") "_"

let read_stdin () = ("stdin", "<stdin>", Core.In_channel.stdin)
let read_file i = (filepath_to_modname i, i, Core.In_channel.create i)

let main () =
  Args.parse ();
  try
    let app_mods = match !Args.input with
    | [] -> [read_stdin () |> parse]
    | input -> input |> map read_file |> map parse
    in
    let std_mod = parse (read_file Args.arc_lang_stdlibpath) in
    let std_mod = if not !Args.show_std then Ast.hide std_mod else std_mod in
    let prelude = Ast.extract_prelude std_mod in
    let app_mods = app_mods |> map (Ast.add_prelude prelude) in
    compile (std_mod::app_mods)
  with
  | Utils.Panic msg ->
      eprintf "Panic: %s. %s" msg (Printexc.get_backtrace ());
      exit 1
  | Error.InputError msg ->
      eprintf "InputError: %s" msg;
      exit 1
;;

main ()
