type info = vis * pos option
and vis = Show | Hide
and pos = Lexing.position * Lexing.position

let loc pos = (Show, Some pos)
let gen = (Show, None)

exception InputError of string
exception LexingError of (info * string)
exception ParsingError of (info * string)
exception NamingError of (info * string)
exception TypingError of (info * string)

let report (info:info) (msg:string) =
  begin
    match snd info with
    | Some (p0, p1) ->
      Printf.eprintf "Error [%s:%d:%d-%d:%d]: %s\n"
        p0.pos_fname
        p0.pos_lnum
        (p0.pos_cnum - p0.pos_bol + 1)
        p1.pos_lnum
        (p1.pos_cnum - p1.pos_bol + 1)
        msg
    | None ->
      Printf.eprintf "Error %s\n" msg
  end;
  if !Args.verbose then begin
    Printf.eprintf "%s" (Printexc.get_backtrace ())
  end;
  exit 1
