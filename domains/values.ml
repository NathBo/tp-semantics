open Abstract_syntax_tree
open Int_domain

module type VALUE_BASICS =
  (sig
    module E : Errors.ERRORS
    module C : Constrain.CONSTRAINT
    module I : Int_domain.INT_DOMAIN with module E = E and module C = C
    module B : Bool_domain.BOOL_DOMAIN with module E = E and module C = C
    type err = E.t
    type ival
    val compare: ival -> ival -> int
    val bot: ival
    val int_bool_binary_bool: (I.t -> I.t -> extent -> bool * bool) -> (B.t -> B.t -> B.t) -> ival -> ival -> extent -> ival * err
    val int_unary: (I.t -> extent -> I.t * err) -> ival -> extent -> ival * err
    val int_binary: (I.t -> I.t -> extent -> I.t add_bottom * err) -> ival -> ival -> extent -> ival * err
    val int_binary_bool: (I.t -> I.t -> extent -> bool * bool) -> ival -> ival -> extent -> ival * err
    val bool_binary: (B.t -> B.t -> B.t) ->ival -> ival -> extent -> ival * err
    val bool_unary: (B.t -> B.t) -> ival -> extent -> ival * err
    val of_int: Z.t -> ival
    val of_bool: bool -> ival
    val rand: Z.t -> Z.t -> ival * err
    val pp_ival: Format.formatter -> ival -> unit
    val can_be_true: ival -> extent -> bool * err
    val can_be_false: ival -> extent -> bool * err

    val union: ival -> ival -> ival
    val inter: ival -> ival -> ival
    val widen: ival -> ival -> ival
    val narrow: ival -> ival -> ival
    val is_le: ival -> ival -> bool
  end)

module type VALUE =
  (sig
    module E : Errors.ERRORS
    module C : Constrain.CONSTRAINT
    module I : Int_domain.INT_DOMAIN with module E = E and module C = C
    module B : Bool_domain.BOOL_DOMAIN with module E = E and module C = C
    type ival
    type err = E.t
    val compare: ival -> ival -> int
    val bot: ival
    val add: ival -> ival -> extent -> ival * err
    val sub: ival -> ival -> extent -> ival * err
    val times: ival -> ival -> extent -> ival * err
    val div: ival -> ival -> extent -> ival * err
    val modulo: ival -> ival -> extent -> ival * err

    val uplus: ival -> extent -> ival * err
    val uminus: ival -> extent -> ival * err

    val less: ival -> ival -> extent -> ival * err
    val less_eq: ival -> ival -> extent -> ival * err
    val greater: ival -> ival -> extent -> ival * err
    val greater_eq: ival -> ival -> extent -> ival * err

    val eq: ival -> ival -> extent -> ival * err
    val not_eq: ival -> ival -> extent -> ival * err

    val logical_or: ival -> ival -> extent -> ival * err
    val logical_and: ival -> ival -> extent -> ival * err
    val logical_not: ival -> extent -> ival * err

    val of_int: Z.t -> ival
    val of_bool: bool -> ival

    val pp_ival: Format.formatter -> ival -> unit

    val rand: Z.t -> Z.t -> ival * err

    val union: ival -> ival -> ival
    val inter: ival -> ival -> ival
    val widen: ival -> ival -> ival
    val narrow: ival -> ival -> ival
    val is_le: ival -> ival -> bool
    val can_be_true: ival -> extent -> bool * err
    val can_be_false: ival -> extent -> bool * err

    val get_constrains: ival -> C.t
    val use_constrains: C.t -> ival -> ival
  end)

(* This should help you to write the next functor. *)
(* TODO: TP1 *)
module MakeValueBasics
    (I: Int_domain.INT_DOMAIN)
    (B: Bool_domain.BOOL_DOMAIN with module C = I.C and module E = I.E) :
  VALUE_BASICS
    with module C = I.C
     and module E = I.E
     and module I = I
     and module B = B =
  (struct
    module E = I.E
    module B = B
    module I = I
    module C = I.C

    type err = E.t

    type ival = {int: I.t add_bottom; bool: B.t add_bottom}

    let compare a b =
      compare (compare a.int b.int) (compare a.bool b.bool)

    let bot = {int = Bot; bool = Bot}

    let int_bool_binary_bool
        (fi: I.t -> I.t -> extent -> bool * bool)   (*le bool*bool c pour (x,not x) ?*)
        (fb: B.t -> B.t -> B.t)
        ({int=ia; bool=ba}: ival)
        ({int=ib; bool=bb}: ival)
        (ext: extent) : ival * err =
      let y,e = match ia, ba, ib, bb with
      | Nb i1, _, Nb i2, _ -> Nb(B.of_bool(fst(fi i1 i2 ext))),E.empty
      | _, Nb(b1), _, Nb(b2) -> Nb(fb b1 b2),E.empty
      | _ -> Bot,E.error (E.make_err Typing "" ext) in
      ({int=Bot;bool=y},e)

    let int_unary
        (f: I.t -> extent -> I.t * err)
        ({int=ia;bool=ba}: ival)
        (ext: extent) : ival * err =
      match ia with
        | Nb i -> let a,b = f i ext in {int=Nb(i);bool=Bot},b
        | Bot -> {int=Bot;bool=Bot},E.error (E.make_err Typing "" ext)

    let int_binary
        (fi: I.t -> I.t -> extent -> I.t add_bottom * err)
        ({int=ia; bool=ba}: ival)
        ({int=ib; bool=bb}: ival)
        (ext: extent) : ival * err =
      match ia,ib with
        | Nb i1, Nb i2 -> let a,e = fi i1 i2 ext in {int=a;bool=Bot},e
        | _ -> {int=Bot;bool=Bot},E.error (E.make_err Typing "" ext)

    let int_binary_bool
        (f: I.t -> I.t -> extent -> bool * bool)
        ({int=ia; bool=ba}: ival)
        ({int=ib; bool=bb}: ival)
        (ext: extent) : ival * err =
      match ia,ib with
        | Nb i1, Nb i2 -> let a = f i1 i2 ext in {int=Bot;bool=Nb(B.of_bool(fst a))},E.empty
        | _ -> {int=Bot;bool=Bot},E.error (E.make_err Typing "" ext)

    let bool_binary
        (fb: B.t -> B.t -> B.t)
        ({int=ia; bool=ba}: ival)
        ({int=ib; bool=bb}: ival)
        (ext: extent) : ival * err =
        match ba,bb with
        | Nb b1, Nb b2 -> let a = fb b1 b2 in {int=Bot;bool=Nb(a)},E.empty
        | _ -> {int=Bot;bool=Bot},E.error (E.make_err Typing "" ext)

    let bool_unary
        (fb: B.t -> B.t)
        ({int=ia; bool=ba}: ival)
        (ext: extent) : ival * err =
        match ba with
        | Nb b -> let a = fb b in {int=Bot;bool=Nb(a)},E.empty
        | Bot -> {int=Bot;bool=Bot},E.error (E.make_err Typing "" ext)

    let of_int (n: Z.t) : ival = assert false
    let of_bool (b: bool) : ival = assert false
    let rand (x: Z.t) (y: Z.t) : ival * err = assert false
    let can_be_true (a: ival) (ext: extent) : bool * err =
      assert false

    let can_be_false (a: ival) (ext: extent) : bool * err =
      assert false

    let pp_ival (fmt: Format.formatter) (a: ival) : unit =
      assert false

    let union (a: ival) (b: ival) : ival =
      assert false (* Leave empty for TP1 *)

    let inter (a: ival) (b: ival) : ival =
      assert false (* Leave empty for TP1 *)

    let widen (a: ival) (b: ival) : ival =
      assert false (* Leave empty for TP1 *)

    let narrow (a: ival) (b: ival) : ival =
      assert false (* Leave empty for TP1 *)

    let is_le (a: ival) (b: ival) : bool =
      assert false

  end)

(* Don't forget to use VB *)
module MakeValue(VB: VALUE_BASICS) : VALUE
  with module C = VB.C
   and module E = VB.E
   and module I = VB.I
   and module B = VB.B
   and type ival = VB.ival =
  (struct
    module E = VB.E
    module I = VB.I
    module B = VB.B
    module C = VB.C
    module VB = VB
    type ival = VB.ival
    type err = VB.err
    let compare = VB.compare
    let bot = VB.bot
    let add: ival -> ival -> extent -> ival * err = VB.int_binary I.add
    let sub: ival -> ival -> extent -> ival * err = VB.int_binary I.sub
    let times: ival -> ival -> extent -> ival * err = VB.int_binary I.times
    let div: ival -> ival -> extent -> ival * err = VB.int_binary I.div
    let modulo: ival -> ival -> extent -> ival * err = VB.int_binary I.modulo
    let uplus: ival -> extent -> ival * err = VB.int_unary I.uplus
    let uminus: ival -> extent -> ival * err = VB.int_unary I.uminus
    let less: ival -> ival -> extent -> ival * err = VB.int_binary_bool I.less
    let less_eq: ival -> ival -> extent -> ival * err = VB.int_binary_bool I.less_eq
    let greater: ival -> ival -> extent -> ival * err = VB.int_binary_bool I.greater
    let greater_eq: ival -> ival -> extent -> ival * err = VB.int_binary_bool I.greater_eq
    let eq: ival -> ival -> extent -> ival * err = VB.int_bool_binary_bool I.eq B.eq
    let not_eq: ival -> ival -> extent -> ival * err = VB.int_bool_binary_bool I.not_eq B.not_eq
    let logical_or: ival -> ival -> extent -> ival * err = VB.bool_binary B.logical_or
    let logical_and: ival -> ival -> extent -> ival * err = VB.bool_binary B.logical_and
    let logical_not: ival -> extent -> ival * err = VB.bool_unary B.logical_not
    let of_int: Z.t -> ival = VB.of_int
    let of_bool: bool -> ival = VB.of_bool
    let can_be_true: ival -> extent -> bool * err = VB.can_be_true
    let can_be_false: ival -> extent -> bool * err = VB.can_be_false
    let pp_ival: Format.formatter -> ival -> unit = VB.pp_ival
    let rand: Z.t -> Z.t -> ival * err = VB.rand
    let union: ival -> ival -> ival = VB.union
    let inter: ival -> ival -> ival = VB.inter
    let widen: ival -> ival -> ival = VB.widen
    let narrow: ival -> ival -> ival = VB.narrow
    let is_le: ival -> ival -> bool = VB.is_le

    let get_constrains: ival -> C.t = fun _ -> C.empty
    let use_constrains: C.t -> ival -> ival = fun _ a -> a
  end)

module Make
    (I: Int_domain.INT_DOMAIN)
    (B: Bool_domain.BOOL_DOMAIN with module C = I.C and module E = I.E)
  : VALUE
    with module C = I.C
    and module E = I.E
    and module I = I
    and module B = B
  = MakeValue(MakeValueBasics(I)(B))
