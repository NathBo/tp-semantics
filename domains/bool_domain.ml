open Errors
open Abstract_syntax_tree
open Int_domain

module type BOOL_DOMAIN =
sig
  module E: ERRORS
  module C: Constrain.CONSTRAINT
  type t

  val of_bool: bool -> t
  val is_empty: t -> bool
  val can_be_true: t -> bool
  val can_be_false: t -> bool

  val pp: Format.formatter -> t -> unit

  val union: t -> t -> t
  val inter: t -> t -> t add_bottom
  val widen: t -> t -> t
  val narrow: t -> t -> t add_bottom
  val is_le: t -> t -> bool

  val cst_true: t
  val cst_false: t
  val logical_or: t -> t -> t
  val logical_and: t -> t -> t

  val logical_not: t -> t

  val eq: t -> t -> t
  val not_eq: t -> t -> t

  val get_constrains: t -> C.t
  val use_constrains: C.t -> t -> t
end

(* TODO: TP1 *)
module MakeConcrete(E: ERRORS) : BOOL_DOMAIN
  with module E = E
   and module C = Constrain.NoConstraint =
  (struct
    module E = E
    module C = Constrain.NoConstraint
    type t = bool
    type err = E.t
    let can_be_true b = b
    let can_be_false b = not b
    let cst_true: t = true
    let cst_false: t = false
    let of_bool x = x
    let is_empty x = false
    let logical_or b1 b2 = b1 || b2
    let logical_and b1 b2 = b1 && b2
    let logical_not b = not b
    let eq b1 b2 = (b1 = b2)
    let not_eq b1 b2 = (b1 <> b2)
    let pp: Format.formatter -> t -> unit = fun fmt t -> Format.pp_print_bool fmt t
    let union: t -> t -> t = fun a b -> assert false (* Leave empty *)
    let inter: t -> t -> t add_bottom = fun a b -> assert false (* Leave empty *)
    let narrow: t -> t -> t add_bottom = inter
    let widen: t -> t -> t = union
    let is_le: t -> t -> bool = fun a b -> assert false (* Leave empty *)
    let get_constrains: t -> C.t = fun _ -> C.empty
    let use_constrains: C.t -> t -> t = fun _ a -> a
  end)

(* TODO: TP3 *)
module Make (E: ERRORS) : BOOL_DOMAIN
  with module E = E
   and module C = Constrain.NonRelationalConstraint =
  (struct
    module E = E
    module C = Constrain.NonRelationalConstraint
    type t (* TODO : type definition to complete *)
    type err = E.t

    let can_be_true: t -> bool = assert false
    let can_be_false: t -> bool = fun c -> assert false
    let bot = assert false
    let top = assert false
    let cst_true: t = assert false
    let cst_false: t = assert false
    let of_bool: bool -> t = fun b -> assert false
    let is_empty: t -> bool = assert false
    let logical_or: t -> t -> t = fun a b -> assert false
    let logical_and: t -> t -> t = fun a b -> assert false
    let logical_not: t -> t = fun a -> assert false
    let eq: t -> t -> t = fun a b -> assert false
    let not_eq: t -> t -> t = fun a b -> assert false
    let pp: Format.formatter -> t -> unit = fun fmt x -> assert false
    let union: t -> t -> t = fun a b -> assert false
    let inter: t -> t -> t add_bottom = fun a b -> assert false
    let narrow: t -> t -> t add_bottom = inter
    let widen: t -> t -> t = union
    let is_le: t -> t -> bool = fun a b -> assert false
    let get_constrains: t -> C.t = fun _ -> C.empty
    let use_constrains: C.t -> t -> t = fun _ a -> a
  end)

