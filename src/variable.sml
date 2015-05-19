structure Variable :> VARIABLE =
struct

  open IntInf

  val counter = ref (fromInt 0)

  datatype t = Var of {name : string, guid : int}

  fun gen name =
      let
          val guid = !counter before counter := !counter + fromInt 1
      in Var {name = name, guid = guid} end

  fun toString (Var {name, guid}) = name ^ "@" ^ IntInf.toString guid

  fun compare (Var l, Var r) = IntInf.compare (#guid l, #guid r)
end
