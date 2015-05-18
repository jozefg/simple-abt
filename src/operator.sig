signature OPERATOR =
sig
    type t

    val eq       : t * t -> bool
    val arity    : t -> int list
    val toString : t -> string
end
