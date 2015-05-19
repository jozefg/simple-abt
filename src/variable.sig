signature VARIABLE =
sig
    eqtype t

    val gen      : string -> t
    val toString : t -> string
    val compare  : t * t -> order
end
