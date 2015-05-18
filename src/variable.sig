signature VARIABLE =
sig
    eqtype t

    val gen : string -> t
    val toString : t -> string
end
