structure LamOp : OPERATOR =
struct
  datatype t = LAM | AP

  val eq = op=

  fun arity LAM = [1]
    | arity AP = [0, 0]

  fun toString LAM = "Lam"
    | toString AP = "Ap"
end

structure Lam = Abt(structure O = LamOp
                    structure V = Variable)
