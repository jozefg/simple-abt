structure Variable :> VARIABLE =
struct

  open IntInf

  val counter = ref 0

  type t = {name : string, guid : int}

  fun gen name =
      let
          val guid = !counter before counter := !counter + 1
      in {name = name, guid = guid} end

  fun toString {name, guid} = name
end
