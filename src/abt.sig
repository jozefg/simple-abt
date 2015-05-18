signature ABT =
sig
    structure Variable : VARIABLE
    structure Operator : OPERATOR

    type t

    datatype 'a view
      = Var  of Variable.t
      | Oper of Operator.t * 'a list
      | Bind of Variable.t * 'a

    exception Malformed

    val into : t view -> t
    val out  : t -> t view

    val var  : Variable.t -> t
    val oper : Operator.t -> t list -> t
    val bind : Variable.t -> t -> t

    val aequiv : t * t -> bool
    val map    : ('a -> 'b) -> 'a view -> 'b view

    val free     : t -> Variable.t list
    val subst    : t -> Variable.t -> t -> t
    val toString : t -> string
end
