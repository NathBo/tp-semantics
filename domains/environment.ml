open Values
open Constrain
open Abstract_syntax_tree

let rec range a b =
  if b-a<=0 then []
  else a::range (a+1) b


module type ENVIRONMENT =
sig
  module E: Errors.ERRORS
  module C: CONSTRAINT
  type env
  type err = E.t
  val compare: env -> env -> int
  val init: env
  val eval_assign: lvalue ext -> expr ext -> env -> env * err
  val eval_guard: expr ext -> env -> env * err
  val eval_assert: expr ext -> env -> env * err
  val eval_print: lvalue ext list -> env -> env * err
  val eval_halt: extent -> env -> env * err
  val eval_local: var_init list -> env -> env * err
  val union: env -> env -> env
  val inter: env -> env -> env
  val widen: env -> env -> env
  val narrow: env -> env -> env
  val is_le: env -> env -> bool
  val pp_env: Format.formatter -> env -> unit
  val get_constrains: env -> C.t
  val use_constrains: C.t -> env -> env
end


(* TODO: TP1 *)
module MakeConcrete(V: VALUE) : ENVIRONMENT
  with module C = SimpleConstraints
   and module E = V.E
   and type err = V.E.t =
  (struct
    module E = V.E
    module C = SimpleConstraints
    module IvalSet = Set.Make(struct type t = V.ival let compare = compare end)
    module SimpleEnv = Mapext.Make(struct type t = variable let compare = compare end)
    type simple_env = V.ival SimpleEnv.t
    module Env =
      Set.Make(struct
        type t = simple_env
        let compare = SimpleEnv.compare V.compare
      end)
    type env = Env.t
    type err = E.t
    let init: env = Env.singleton SimpleEnv.empty
    let compare (a: env) (b: env) : int = Env.compare a b
    let pp_env (fmt: Format.formatter) (env: env) : unit =
      let () =
        Env.iter
          (fun simple_env ->
             let () =
               SimpleEnv.iter
                 (fun variable value ->
                    let () = Format.fprintf fmt "%a = %a; "
                        Abstract_syntax_printer.print_var variable
                        V.pp_ival value in
                    ()
                 )
                 simple_env
             in
             let () = Format.fprintf fmt "\n" in
             ()
          )
          env
      in
      ()

    let rec eval_expr (e, ext: expr ext) (env: simple_env) : IvalSet.t * err = match e with
      | AST_int_const i -> IvalSet.singleton (V.of_int i),E.empty
      | AST_bool_const b -> IvalSet.singleton (V.of_bool b),E.empty
      | AST_variable (v,ext') -> if SimpleEnv.mem v env then IvalSet.singleton (SimpleEnv.find v env),E.empty else (IvalSet.empty,E.error (E.make_err UndefinedVariable "" ext))
      | AST_int_rand ((i1,_),(i2,_)) -> IvalSet.of_list (List.map (fun x -> V.of_int (Z.of_int x)) (range (Z.to_int i1) (Z.to_int i2+1))),E.empty
      | AST_unary (op,arg) -> let valposs, err = eval_expr arg env in
      begin match op with
        | AST_UNARY_MINUS -> IvalSet.map (fun x ->  fst(V.uminus x ext)) valposs,err    (*bon je void les erreur mais est-ce grave ? (oui mdr)*)
        | AST_UNARY_PLUS -> IvalSet.map (fun x ->  fst(V.uplus x ext)) valposs,err
        | AST_NOT -> IvalSet.map (fun x ->  fst(V.logical_not x ext)) valposs,err
      end
      | AST_binary (op,arg1,arg2) -> let arg1, e1 = eval_expr arg1 env in
      let arg2, e2 = eval_expr arg2 env in
      let err = E.union e1 e2 in
      let f =
        (match op with
         | AST_PLUS -> V.add
         | AST_MINUS -> V.sub
         | AST_MULTIPLY -> V.times
         | AST_DIVIDE -> V.div
         | AST_MODULO -> V.modulo
         | AST_EQUAL -> V.eq
         | AST_NOT_EQUAL -> V.not_eq
         | AST_LESS -> V.less
         | AST_LESS_EQUAL -> V.less_eq
         | AST_GREATER -> V.greater
         | AST_GREATER_EQUAL -> V.greater_eq
         | AST_AND -> V.logical_and
         | AST_OR -> V.logical_or) in

      let rep = ref IvalSet.empty in
      let reperr = ref err in

      let aux1 x y =
        let z = f x y ext in
        rep := IvalSet.add (fst(z)) !rep;
        reperr := E.union (snd z) !reperr
      in

      let aux2 x =
          IvalSet.iter (aux1 x) arg2 in

      IvalSet.iter aux2 arg1;
      
      !rep,E.union err err

      let eval_assign (l, _: lvalue ext) (e: expr ext) (env: env) : env * err =
        Env.fold
          (fun simple_env (acc, err) ->
             let v, new_err = eval_expr e simple_env in
             let env = IvalSet.fold
               (fun ival acc -> Env.add (SimpleEnv.add l ival simple_env) acc)
               v Env.empty in
              Env.union env acc, E.union err new_err)
          env
          (Env.empty, E.empty)
  

    let can_be_true (v: IvalSet.t) ext : bool * err =
      IvalSet.fold
        (fun v (b1, e1) ->
           let b2, e2 = V.can_be_true v ext in
           b1 || b2, E.union e1 e2
        )
        v
        (false, E.empty)

    let eval_guard ((_, ext) as e: expr ext) (env: env) : env * err =
      Env.fold
        (fun simple_env (env, err) ->
           let v, e1 = eval_expr e simple_env in
           match can_be_true v ext with
           | true, e -> Env.add simple_env env, E.union err e
           | false, e -> env, E.union err e
        )
        env
        (Env.empty, E.empty)

    let eval_assert ((_, ext) as e: expr ext) (env: env) : env * err =
      Env.fold
        (fun simple_env (env, err) ->
           let v, e1 = eval_expr e simple_env in
           match can_be_true v ext with
           | true, e -> Env.add simple_env env, E.union err e
           | false, e ->
             let err_assert = E.make_err Errors.AssertFalse "" ext in
             env, E.union (E.union err e) (E.error err_assert)
        )
        env
        (Env.empty, E.empty)

    let eval_print(l: lvalue ext list) (env: env) : env * err =
      let () =
        Env.iter
          (fun simple_env ->
             let () =
               List.iter
                 (fun (lvalue, ext) ->
                    let i = SimpleEnv.find lvalue simple_env in
                    let () = Format.printf "%a = %a; "
                        Abstract_syntax_printer.print_var lvalue
                        V.pp_ival i in
                    ()
                 )
                 l
             in
             let () = Format.printf "\n" in
             ()
          )
          env
      in
      let () = Format.printf "\n" in
      env, E.empty

    let eval_halt: extent -> env -> env * err = fun ext _ ->
      Env.empty, E.error (E.make_err Errors.Halt "" ext)

    let eval_local (l: var_init list) (env: env) : env * err =
      List.fold_left
        (fun (env, err) (v, eo) ->
           match eo with
           | None -> env, err
           | Some eo -> eval_assign v eo env
        )
        (env, E.empty)
        l

    let union: env -> env -> env = Env.union

    let inter: env -> env -> env = Env.inter

    let widen: env -> env -> env = union
    let narrow: env -> env -> env = inter
    let is_le: env -> env -> bool = fun _ _ -> assert false (* Leave empty for TP1, possiblement ajouter 2 arg *)

    let get_constrains: env -> C.t = fun _ -> C.empty
    let use_constrains: C.t -> env -> env = fun _ a -> a
  end)

(* TODO: TP2 *)
module TypingEnv(E: Errors.ERRORS) : ENVIRONMENT
  with module C = SimpleConstraints
   and module E = E
   and type err = E.t =
  (struct
    module E = E
    module C = SimpleConstraints
    type ty_value = Int | Bool | Bot | Top
    module Env = Mapext.Make(struct type t = variable let compare = compare end)
    type env = ty_value Env.t
    type err = E.t
    let compare = Env.compare (compare)
    let init: env = Env.empty
    let bot = Bot
    let int = Int
    let bool = Bool
    let stronger ty1 ty2 =
      match ty1,ty2 with
        | Bot,_ | _,Bot -> Bot
        | Top,_ | _,Top | Bool,Int | Int,Bool -> Top
        | Bool,Bool -> Bool
        | Int,Int -> Int

    let rec eval_expr ext env expr = match expr with
      | AST_int_const _ -> Int,E.empty
      | AST_bool_const _ -> Bool,E.empty
      | AST_int_rand _ -> Int,E.empty
      | AST_variable (x,_) -> Env.find x env,E.empty
      | AST_unary (op,(e,ext)) -> (match op with
        | AST_UNARY_PLUS | AST_UNARY_MINUS -> let t,err =  eval_expr ext env e in
          if t = Bool
            then Bot,E.union err (E.error(E.make_err Typing "" ext))
            else t,err
        | AST_NOT -> let t,err =  eval_expr ext env e in
        if t = Int
          then Bot,E.union err (E.error(E.make_err Typing "" ext))
          else t,err
        )
      | AST_binary (op,(e1,ext1),(e2,ext2)) -> (match op with
          | AST_PLUS | AST_MINUS | AST_MULTIPLY | AST_DIVIDE | AST_MODULO -> let t1,err1 =  eval_expr ext env e1 in
            let t2,err2 =  eval_expr ext env e2 in
            let err = E.union err1 err2 in
            if t1=Bool || t2=Bool
            then Bot,E.union err (E.error(E.make_err Typing "" ext))
            else stronger t1 t2,err
          | AST_AND | AST_OR -> let t1,err1 =  eval_expr ext env e1 in
            let t2,err2 =  eval_expr ext env e2 in
            let err = E.union err1 err2 in
            if t1=Int || t2=Int
            then Bot,E.union err (E.error(E.make_err Typing "" ext))
            else stronger t1 t2,err
          | AST_GREATER | AST_GREATER_EQUAL | AST_LESS | AST_LESS_EQUAL -> let t1,err1 =  eval_expr ext env e1 in
          let t2,err2 =  eval_expr ext env e2 in
          let err = E.union err1 err2 in
          if t1=Bool || t2=Bool
          then Bot,E.union err (E.error(E.make_err Typing "" ext))
          else if t1=Int && t2=Int
          then Bool,err
          else stronger t1 t2,err
          | AST_EQUAL | AST_NOT_EQUAL -> let t1,err1 =  eval_expr ext env e1 in
          let t2,err2 =  eval_expr ext env e2 in
          let err = E.union err1 err2 in
          if (t1=Bool && t2=Bool) || (t1=Int && t2=Int)
          then Bot,E.union err (E.error(E.make_err Typing "" ext))
          else stronger t1 t2,err
          )

    let eval_assign (lvalue, ext: lvalue ext) ((expr,ext): expr ext) (env: env) : env * err =
      
      let t,err = eval_expr ext env expr in
      let env = Env.add lvalue t env in
      env,err

    let eval_guard ((expr,ext): expr ext) (env: env) : env * err =
      let _,err = eval_expr ext env expr in
      env,err

    let eval_assert (expr: expr ext) (env: env) : env * err =
      assert false

    let eval_print (l: lvalue ext list) (env: env) : env * err =
      assert false

    let eval_halt (ext: extent) (env: env) : env * err = assert false
    let eval_local (l: var_init list) (env: env) : env * err =
      assert false

    let union (a: env) (b: env) : env =
      assert false
    let inter (a: env) (b: env) : env =
      assert false
    let widen: env -> env -> env = union
    let narrow: env -> env -> env = inter
    let is_le (a: env) (b: env) : bool =
      assert false
    let pp_env (fmt: Format.formatter) (env: env) : unit = assert false
    let get_constrains: env -> C.t = fun _ -> C.empty
    let use_constrains: C.t -> env -> env = fun _ env -> env
  end)

(* TODO: TP3 *)
module MakeFromValues(V: VALUE with module C = NonRelationalConstraint)
  : ENVIRONMENT
    with module C = SimpleConstraints
     and module E = V.E and type err = V.E.t
  =
  (struct
    module E = V.E
    module C = SimpleConstraints
    module Env = Mapext.Make(struct type t = variable let compare = compare end)
    type env = V.ival Env.t
    type err = E.t
    let compare a b = Env.compare V.compare a b
    let init: env = Env.empty

    let rec eval_expr (expr, ext: expr ext) (env: env) : V.ival * err =
      let v, err =
        match expr with
        | AST_int_const i -> V.of_int i, E.empty
        | AST_bool_const b -> V.of_bool b, E.empty
        | AST_int_rand ((l, _), (h, _)) -> V.rand l h
        | AST_variable (v, _) -> Env.find v env, E.empty
        | AST_unary (op, (_, ext as arg)) ->
          let arg, err = eval_expr arg env in
          let i, err =
            match op with
            | AST_UNARY_PLUS -> V.uplus arg ext
            | AST_UNARY_MINUS -> V.uminus arg ext
            | AST_NOT -> V.logical_not arg ext
          in
          i, err
        | AST_binary (op, arg1, arg2) ->
          let arg1, e1 = eval_expr arg1 env in
          let arg2, e2 = eval_expr arg2 env in
          let err = E.union e1 e2 in
          let i, err_ =
            (match op with
             | AST_PLUS -> V.add
             | AST_MINUS -> V.sub
             | AST_MULTIPLY -> V.times
             | AST_DIVIDE -> V.div
             | AST_MODULO -> V.modulo
             | AST_EQUAL -> V.eq
             | AST_NOT_EQUAL -> V.not_eq
             | AST_LESS -> V.less
             | AST_LESS_EQUAL -> V.less_eq
             | AST_GREATER -> V.greater
             | AST_GREATER_EQUAL -> V.greater_eq
             | AST_AND -> V.logical_and
             | AST_OR -> V.logical_or) arg1 arg2 ext
          in
          i, E.union err err_
      in
      v, err

    let eval_assign (l, _: lvalue ext) (expr: expr ext) (env: env) : env * err =
      let i, err = eval_expr expr env in
      let env = Env.add l i env in
      env, err

    let eval_guard (e: expr ext) (env: env) : env * err =
      let i, e1 = eval_expr e env in
      let can_be_true, e2 = V.can_be_true i (snd e) in
      if can_be_true then
        env, E.union e1 e2
      else
        Env.empty, E.union e1 e2

    let eval_assert (e: expr ext) (env: env) : env * err =
      let i, e1 = eval_expr e env in
      let can_be_false, e2 = V.can_be_false i (snd e) in
      if can_be_false then
        let err_assert = E.make_err Errors.AssertFalse "" (snd e) in
        Env.empty, E.add err_assert (E.union e1 e2)
      else
        env, E.union e1 e2

    let eval_print (l: lvalue ext list) (env: env) : env * err =
      let () =
        List.iter
          (fun (lvalue, _) ->
             Format.printf "%a=%a; "
               Abstract_syntax_printer.print_lvalue lvalue
               V.pp_ival (Env.find lvalue env)
          )
          l
      in
      let () = Format.printf "\n" in
      env, E.empty

    let eval_local (l: var_init list) (env: env) : env * err =
      List.fold_left
        (fun (env, errs) (l, init) ->
           match init with
           | None -> env, errs
           | Some init ->
             let env, e = eval_assign l init env in
             env, E.union e errs
        )
        (env, E.empty)
        l

    let eval_halt (ext: extent) (env: env) : env * err =
      Env.empty, E.error (E.make_err Errors.Halt "" ext)

    let union (a: env) (b: env) : env =
      Env.map2o
        (fun _ a -> a)
        (fun _ a -> a)
        (fun _ a b -> V.union a b)
        a b

    let inter (a: env) (b: env) : env =
      Env.map2o
        (fun _ a -> a)
        (fun _ a -> a)
        (fun _ a b -> V.inter a b)
        a b

    let widen (a: env) (b: env) : env =
      Env.map2o
        (fun _ a -> a)
        (fun _ a -> a)
        (fun _ a b -> V.widen a b)
        a b

    let narrow (a: env) (b: env) : env =
      Env.map2o
        (fun _ a -> a)
        (fun _ a -> a)
        (fun _ a b -> V.narrow a b)
        a b

    let is_le (a: env) (b: env) : bool =
      Env.for_all2o
        (fun _ a -> a = V.bot)
        (fun _ _ -> true)
        (fun _ a b -> V.is_le a b)
        a b

    let pp_env (fmt: Format.formatter) (env: env) : unit =
      Env.iter
        (fun variable value ->
           Format.printf "%a=%a; "
             Abstract_syntax_printer.print_lvalue variable
             V.pp_ival value
        )
        env;
      Format.printf "\n"

    let get_constrains (env: env) : C.t =
      Env.fold
        (fun k v acc ->
           C.union
             acc
             (V.C.fold
                (fun c acc -> C.add (NonRelational(k, c)) acc)
                (V.get_constrains v)
                C.empty)
        )
        env
        C.empty

    let use_constrains (c: C.t) (env: env) : env =
      C.fold
        (fun c env ->
           match c with
           | Constrain.NonRelational (v, c) ->
             let env =
               try
                 let i = V.use_constrains (V.C.add c V.C.empty) (Env.find v env) in
                 Env.add v i env
               with Not_found -> env
             in
             env
           | Constrain.Equal _ -> env
        )
        c
        env
  end)

(* TODO: TP4 *)
module MakeEquality(E: Errors.ERRORS) : ENVIRONMENT
  with module C = SimpleConstraints
   and module E = E
   and type err = E.t =
  (struct
    module E = E
    module C = SimpleConstraints
    type env
    type err = E.t
    let compare (a: env) (b: env) : int = assert false

    let init: env = assert false

    let eval_assign (lvalue, _: lvalue ext) (e, _: expr ext) (env: env) : env * err = assert false

    let eval_guard (_: expr ext) (env: env) : env * err = assert false

    let eval_assert (_, ext: expr ext) (env: env) : env * err = assert false

    let eval_print: lvalue ext list -> env -> env * err = assert false
    let eval_halt: extent -> env -> env * err = assert false
    let eval_local: var_init list -> env -> env * err = assert false
    let union: env -> env -> env = assert false
    let inter: env -> env -> env = assert false
    let widen: env -> env -> env = assert false
    let narrow: env -> env -> env = assert false
    let is_le: env -> env -> bool = assert false
    let pp_env: Format.formatter -> env -> unit = assert false
    let get_constrains: env -> C.t = assert false
    let use_constrains: C.t -> env -> env = assert false
  end)

(* TODO: TP5 *)
module MakeProduct
    (A: ENVIRONMENT)
    (B: ENVIRONMENT with module E = A.E and module C = A.C)
  : ENVIRONMENT
    with module E = A.E
     and module C = A.C =
  (struct
    module E = A.E
    module C = A.C

    type env
    type err = E.t
    let compare enva envb =
      assert false

    let init: env = assert false

    let reduce = fun enva envb -> assert false

    let eval_assign: lvalue ext -> expr ext -> env -> env * err =
      fun l e env -> assert false

    let eval_guard: expr ext -> env -> env * err =
      fun e env -> assert false

    let eval_assert: expr ext -> env -> env * err =
      fun e env -> assert false

    let eval_print: lvalue ext list -> env -> env * err =
      fun l env -> assert false

    let eval_halt: extent -> env -> env * err =
      fun ext env -> assert false

    let eval_local: var_init list -> env -> env * err =
      fun l env -> assert false

    let pp_env: Format.formatter -> env -> unit =
      fun fmt env -> assert false

    let union: env -> env -> env =
      fun enva envb -> assert false

    let inter: env -> env -> env =
      fun enva envb -> assert false

    let widen: env -> env -> env =
      fun enva envb -> assert false

    let narrow: env -> env -> env =
      fun enva envb -> assert false

    let is_le: env -> env -> bool =
      fun enva envb -> assert false

    let get_constrains: env -> C.t =
      fun env -> assert false

    let use_constrains: C.t -> env -> env =
      fun c env -> assert false
  end)
