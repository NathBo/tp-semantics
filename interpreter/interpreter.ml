open Abstract_syntax_tree
open Environment
open Errors

module type ITERATOR =
sig
  module E: ERRORS
  module Env: ENVIRONMENT with module E = E
  type env = Env.env
  type err = Env.err
  val init: env
  val eval_prog: env -> prog -> env * err
end

(* TODO: TP1 *)
module MakeConcrete(Env: ENVIRONMENT) : ITERATOR =
struct
  module Env = Env
  module E = Env.E
  type env = Env.env
  type err = Env.err

  let lfp (type a) (cmp: a -> a -> int) (f: a -> a) (x: a) : a =
    assert false

  let init : env = Env.init

  let rec eval_stat (s, ext: stat ext) (env: env) : env * err =
    let env, err =
      match s with
      | AST_block b -> assert false
      | AST_assign (l, r) -> assert false
      | AST_if (guard, t_stat, f_stat) -> assert false
      | AST_while (guard, body) -> assert false
      | AST_HALT -> assert false
      | AST_assert guard -> assert false
      | AST_print l -> assert false
      | AST_local l -> assert false
    in
    env, err

  let eval_prog (env: env) (l, _: prog) : env * err =
    List.fold_left
      (fun (env, err) (AST_stat s) ->
         let env, e = eval_stat s env in
         env, E.union err e
      )
      (env, E.empty)
      l
end

(* TODO: TP3 *)
module MakeAbstract(Env: ENVIRONMENT) : ITERATOR =
struct
  module Env = Env
  module E = Env.E
  type env = Env.env
  type err = Env.err
  let init : env = Env.init

  let lfp (type a)
      (is_le: a -> a -> bool)
      (widen: a -> a -> a)
      (narrow: a -> a -> a)
      (f: a -> a)
      (x: a) : a =
    assert false

  let eval_stat (s, ext: stat ext) (env: env) : env * err =
    match s with
    | AST_block _
    | AST_assign (_, _)
    | AST_if (_, _, _)
    | AST_while (_, _)
    | AST_HALT
    | AST_assert _
    | AST_print _
    | AST_local _
      -> assert false

  let eval_prog (env: env) (prog, _: prog) : env * err =
    List.fold_left
      (fun (env, err) (AST_stat s) ->
         let env, e = eval_stat s env in
         env, E.union err e
      )
      (env, E.empty)
      prog
end
