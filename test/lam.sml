structure LamOp =
struct
  datatype t = LAM | AP

  val eq = op=

  fun arity LAM = [1]
    | arity AP = [0, 0]

  fun toString LAM = "Lam"
    | toString AP = "Ap"
end

structure SomeTests =
struct
  structure Lam = Abt(structure O = LamOp
                      structure V = Variable)
  open LamOp
  open Lam

  fun ap l r = oper AP [l, r]
  fun lam x e = oper LAM [bind x e]

  fun eval e =
      case out e of
          Oper (LAM, arg) => oper LAM arg
        | Oper (AP, [l, r]) => (
            case out (eval l) of
                Oper (LAM, [e]) => (
                 case out e of
                     Bind (x, e') => eval (subst r x e')
                   | _ => raise Fail "Impossible"
             )
             | _ => raise Fail "Impossible"
        )
        | _ => raise Fail "Impossible"

  val [x, y, z] = List.map Variable.gen ["x", "y", "z"]
  val id = lam x (var x)
  val const = lam x (lam y (var x))

  val true = aequiv (eval (oper AP [id, id]), id)
  val true = aequiv (eval (ap (ap const id) id), id)

  val () = print (toString (oper AP [id, id]) ^ "\n")
end
