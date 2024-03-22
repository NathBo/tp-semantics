open Abstract_syntax_tree

module type CONSTRAINT =
sig
  type c (* A constraint *)
  type t (* A set of contrains *)
  val empty: t
  val add: c -> t -> t
  val union: t -> t ->t
  val fold: (c -> 'acc -> 'acc) -> t -> 'acc -> 'acc
end

type no_constraint = |

type non_relational_constraint =
  | Parity of bool
  | Interval of Z.t * Z.t

type simple_constraint =
  | NonRelational of variable * non_relational_constraint
  | Equal of variable * variable

module NoConstraint : CONSTRAINT with type c = no_constraint =
  (struct
    type c = no_constraint
    module S = Set.Make(struct type t = c let compare = compare end)
    include S
  end)

module NonRelationalConstraint : CONSTRAINT with type c = non_relational_constraint =
  (struct
    type c = non_relational_constraint
    module S = Set.Make(struct type t = c let compare = compare end)
    include S
  end)


module SimpleConstraints : CONSTRAINT with type c = simple_constraint =
  (struct
    type c = simple_constraint
    module S = Set.Make(struct type t = c let compare = compare end)
    include S
  end)
