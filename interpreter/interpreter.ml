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

  let lfp (type a)
      (cmp: a -> a -> int)
      (widen: a -> a -> a)
      (_: a -> a -> a)
      (f: a -> a)
      (x: a) : a =
    let rec iter_widen x =
      let x' = widen x (f x) in
      if cmp x x' <= 0 then
        x'
      else
        iter_widen x' in
    iter_widen x


  let init : env = Env.init

  let rec eval_stat (s, _: stat ext) (_: env) : env * err =
    let env, err =
      match s with
      | AST_block _ -> assert false(*List.fold_left (fun a -> fun b -> eval_stat b a) (env,E.empty) sl *)
      | AST_assign (_, _) -> assert false
      | AST_if (_, _, _) -> assert false
      | AST_while (_, _) -> assert false
      | AST_HALT -> assert false
      | AST_assert _ -> assert false
      | AST_print _ -> assert false
      | AST_local _ -> assert false
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
      (_: a -> a -> bool)
      (_: a -> a -> a)
      (_: a -> a -> a)
      (_: a -> a)
      (_: a) : a =
    assert false

  let eval_stat (s, _: stat ext) (_: env) : env * err =
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
