open Abstract_syntax_tree

type error_kind =
  | Typing
  | UndefinedVariable
  | DivisionByZero
  | ModuloByZero
  | AssertFalse
  | Halt
  (* Add what you need *)

let pp_error_kind fmt ek =
  Format.fprintf fmt
  (match ek with
  | Typing            -> "Typing"
  | UndefinedVariable -> "UndefinedVariable"
  | DivisionByZero    -> "DivisionByZero"
  | ModuloByZero      -> "ModuloByZero"
  | AssertFalse       -> "AssertFalse"
  | Halt              -> "Halt")

type err = error_kind * string * extent
module ErrSet = Set.Make(struct type t = err let compare = compare end)

module type ERRORS =
  (sig
    type nonrec err
    type t
    val pp_err: Format.formatter -> err -> unit
    val pp: Format.formatter -> t -> unit
    val empty: t
    val error: err -> t
    val union: t -> t -> t
    val inter: t -> t -> t
    val add: err -> t -> t
    val make_err: error_kind -> string -> extent -> err
  end)

module Errors: ERRORS =
  (struct
    type nonrec err = err
    type t = ErrSet.t
    let pp_err (fmt: Format.formatter) (ek, s, ext: err) : unit =
      Format.fprintf fmt
        "Error %a at %a: %s"
        pp_error_kind ek
        Abstract_syntax_printer.pp_extent ext
        s
    let pp (fmt: Format.formatter) (t: t) : unit =
      ErrSet.iter
        (pp_err fmt)
        t
    let empty : t = ErrSet.empty
    let error (err: err) : t = ErrSet.singleton err
    let union (a: t) (b: t) : t = ErrSet.union a b
    let inter (a: t) (b: t) : t = ErrSet.inter a b
    let add (e: err) (t: t) : t = ErrSet.add e t
    let make_err (ek: error_kind) (s: string) (ext: extent) : err = ek, s, ext
  end)
