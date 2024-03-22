open Errors
open Abstract_syntax_tree

type 'a add_bottom = Nb of 'a | Bot

module type INT_DOMAIN =
sig
  module E: ERRORS
  module C: Constrain.CONSTRAINT
  type err = E.t
  type t
  val of_int: Z.t -> t
  val rand: Z.t -> Z.t -> t

  val pp: Format.formatter -> t -> unit

  val union: t -> t -> t
  val inter: t -> t -> t add_bottom
  val widen: t -> t -> t
  val narrow: t -> t -> t add_bottom
  val is_le: t -> t -> bool

  val add: t -> t -> extent -> t add_bottom * err
  val sub: t -> t -> extent -> t add_bottom * err
  val times: t -> t -> extent -> t add_bottom * err
  val div: t -> t -> extent -> t add_bottom * err
  val modulo: t -> t -> extent -> t add_bottom * err

  val uplus: t -> extent -> t * err
  val uminus: t -> extent -> t * err

  val less: t -> t -> extent -> bool * bool (* may_be_true, may_be_false *)
  val less_eq: t -> t -> extent -> bool * bool
  val greater: t -> t -> extent -> bool * bool
  val greater_eq: t -> t -> extent -> bool * bool
  val eq: t -> t -> extent -> bool * bool
  val not_eq: t -> t -> extent -> bool * bool

  val get_constrains: t -> C.t
  val use_constrains: C.t -> t -> t
end

(* TODO: TP1 *)
module MakeConcrete (E: ERRORS) : INT_DOMAIN
  with module E = E
   and module C = Constrain.NoConstraint =
  (struct
    module E = E
    module C = Constrain.NoConstraint
    type err = E.t
    type t = Z.t
    let rec rand: Z.t -> Z.t -> t = fun a b -> assert false (* Leave empty *)
    let of_int: Z.t -> t = fun x -> assert false
    let bin
        (f: Z.t -> Z.t -> Z.t)
        (a: t) (b: t)
        (ext: extent) : t add_bottom * err =
      (Nb(f a b),E.empty)
    let add: t -> t -> extent -> t add_bottom * err = bin Z.add
    let sub: t -> t -> extent -> t add_bottom * err = bin Z.sub
    let times: t -> t -> extent -> t add_bottom * err = bin Z.mul

    let div: t -> t -> extent -> t add_bottom * err = fun a b ext ->
      if b=Z.zero
        then (Bot,E.error (E.make_err DivisionByZero "" ext))
        else bin Z.div a b ext
    let modulo: t -> t -> extent -> t add_bottom * err = fun a b ext ->
      if b=Z.zero
        then (Bot,E.error (E.make_err DivisionByZero "" ext))
        else bin Z.(mod) a b ext

    let uplus: t -> extent -> t * err = fun a _ -> (a,E.empty)
    let uminus: t -> extent -> t * err = fun a _ -> (Z.neg a, E.empty)

    let bin_bool
        (f: Z.t -> Z.t -> bool)
        (a: t) (b: t)
        (ext: extent) : bool * bool =
        let x = f a b in
        (x, not x)
    let less: t -> t -> extent -> bool * bool = bin_bool Z.lt
    let less_eq: t -> t -> extent -> bool * bool = bin_bool Z.leq
    let greater: t -> t -> extent -> bool * bool = bin_bool Z.gt
    let greater_eq: t -> t -> extent -> bool * bool = bin_bool Z.geq
    let eq: t -> t -> extent -> bool * bool = bin_bool Z.equal
    let not_eq: t -> t -> extent -> bool * bool =
      bin_bool (fun a b -> Z.equal a b |> not)

    let pp: Format.formatter -> t -> unit = fun fmt t -> Z.pp_print fmt t

    let union (a: t) (b: t) : t = assert false (* Leave empty *)
    let inter (a: t) (b: t) : t add_bottom = assert false (* Leave empty *)
    let widen: t -> t -> t = union
    let narrow: t -> t -> t add_bottom = inter
    let is_le (a: t) (b: t) : bool = assert false (* Leave empty *)

    let get_constrains: t -> C.t = fun _ -> C.empty
    let use_constrains: C.t -> t -> t = fun _ a -> a
  end)

(* TODO: TP3 *)
module MakeInterval (E: ERRORS) : INT_DOMAIN
  with module E = E
   and module C = Constrain.NonRelationalConstraint =
  (struct
    module E = E
    module C = Constrain.NonRelationalConstraint
    type err = E.t
    type t (* TODO : type definition to complete *)
    let of_int: Z.t -> t = fun x -> assert false
    let rand: Z.t -> Z.t -> t = fun a b -> assert false

    let add: t -> t -> extent -> t add_bottom * err = assert false
    let sub: t -> t -> extent -> t add_bottom * err = assert false
    let times: t -> t -> extent -> t add_bottom * err = assert false
    let div: t -> t -> extent -> t add_bottom * err = assert false
    let modulo: t -> t -> extent -> t add_bottom * err = assert false

    let uplus: t -> extent -> t * err = assert false
    let uminus: t -> extent -> t * err = assert false

    let less: t -> t -> extent -> bool * bool = assert false
    let less_eq: t -> t -> extent -> bool * bool = assert false
    let greater: t -> t -> extent -> bool * bool = assert false
    let greater_eq: t -> t -> extent -> bool * bool = assert false
    let eq: t -> t -> extent -> bool * bool = assert false
    let not_eq: t -> t -> extent -> bool * bool = assert false

    let pp: Format.formatter -> t -> unit = assert false

    let union: t -> t -> t = assert false
    let inter: t -> t -> t add_bottom = assert false
    let widen: t -> t -> t = assert false
    let narrow: t -> t -> t add_bottom = assert false
    let is_le: t -> t -> bool = assert false

    let get_constrains: t -> C.t = assert false
    let use_constrains: C.t -> t -> t = assert false
  end)

(* TODO: TP5 *)
module MakeParity (E: ERRORS) : INT_DOMAIN
  with module E = E
   and module C = Constrain.NonRelationalConstraint =
  (struct
    module E = E
    module C = Constrain.NonRelationalConstraint
    type err = E.t
    type t = Odd | Even | Top
    let is_empty: t -> bool = assert false
    let empty: t = assert false
    let of_int: Z.t -> t = assert false
    let rand: Z.t -> Z.t -> t = assert false

    let add: t -> t -> extent -> t add_bottom * err = assert false
    let sub: t -> t -> extent -> t add_bottom * err = assert false
    let times: t -> t -> extent -> t add_bottom * err = assert false
    let div: t -> t -> extent -> t add_bottom * err = assert false
    let modulo: t -> t -> extent -> t add_bottom * err = assert false

    let uplus: t -> extent -> t * err = assert false
    let uminus: t -> extent -> t * err = assert false

    let less: t -> t -> extent -> bool * bool = assert false
    let less_eq: t -> t -> extent -> bool * bool = assert false
    let greater: t -> t -> extent -> bool * bool = assert false
    let greater_eq: t -> t -> extent -> bool * bool = assert false
    let eq: t -> t -> extent -> bool * bool = assert false
    let not_eq: t -> t -> extent -> bool * bool = assert false

    let pp: Format.formatter -> t -> unit = assert false

    let union (a: t) (b: t) : t = assert false
    let inter (a: t) (b: t) : t add_bottom = assert false
    let widen (a: t) (b: t) : t = assert false
    let narrow (a: t) (b: t) : t add_bottom = assert false
    let is_le (a: t) (b: t) : bool = assert false

    let get_constrains: t -> C.t = assert false
    let use_constrains: C.t -> t -> t = assert false
  end)
