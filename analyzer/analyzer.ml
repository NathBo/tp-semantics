(*
  Cours "Semantics and applications to verification"

  Antoine Miné 2014
  Marc Chevalier 2018
  Ecole normale supérieure, Paris, France / CNRS / INRIA
*)

(*
  Simple driver: parses the file given as argument and prints it back.

  You should modify this file to call your functions instead!
*)

type tp =
  | TP1
  | TP2
  | TP3
  | TP4
  | TP5

let select_domain (tp: tp) : (module Interpreter.ITERATOR) =
  match tp with
  | TP1 ->
    let module Err = Errors.Errors in
    let module Bool = Bool_domain.MakeConcrete(Err) in
    let module Int = Int_domain.MakeConcrete(Err) in
    let module V = Values.Make(Int)(Bool) in
    let module Env = Environment.MakeConcrete(V) in
    let module Interp = Interpreter.MakeConcrete(Env) in
    (module Interp)
  | TP2 ->
    let module Err = Errors.Errors in
    let module Env = Environment.TypingEnv(Err) in
    let module Interp = Interpreter.MakeConcrete(Env) in
    (module Interp)
  | TP3 ->
    let module Err = Errors.Errors in
    let module Bool = Bool_domain.Make(Err) in
    let module IntInterv = Int_domain.MakeInterval(Err) in
    let module V = Values.Make(IntInterv)(Bool) in
    let module Env = Environment.MakeFromValues(V) in
    let module Interp = Interpreter.MakeAbstract(Env) in
    (module Interp)
  | TP4 ->
    let module Err = Errors.Errors in
    let module Env = Environment.MakeEquality(Err) in
    let module Interp = Interpreter.MakeAbstract(Env) in
    (module Interp)
  | TP5 ->
    let module Err = Errors.Errors in
    let module Bool = Bool_domain.Make(Err) in
    let module IntInterv = Int_domain.MakeInterval(Err) in
    let module VInterv = Values.Make(IntInterv)(Bool) in
    let module EnvInterv = Environment.MakeFromValues(VInterv) in
    let module IntParity = Int_domain.MakeParity(Err) in
    let module VParity = Values.Make(IntParity)(Bool) in
    let module EnvParity = Environment.MakeFromValues(VParity) in
    let module Env = Environment.MakeProduct(EnvInterv)(EnvParity) in
    let module Interp = Interpreter.MakeAbstract(Env) in
    (module Interp)

(* parse and print filename *)
let doit filename =
  let prog = File_parser.parse_file filename in
  Abstract_syntax_printer.print_prog Format.std_formatter prog;
  let interp = select_domain TP1 in
  let module Interp = (val interp) in
  let env_init = Interp.init in
  let env_final, err = Interp.eval_prog env_init prog in
  Interp.Env.pp_env Format.std_formatter env_final;
  Interp.E.pp Format.std_formatter err


(* parses arguments to get filename *)
let main () =
  try
    match Array.length Sys.argv with
    | 2 -> doit Sys.argv.(1)
    | 0 -> Printf.printf "Usage: executable file\n"; exit 1
    | _ -> Printf.printf "Usage: %s file\n" Sys.argv.(0); exit 1
  with e -> Printexc.print_backtrace stdout; flush stdout; raise e

let _ = main ()
