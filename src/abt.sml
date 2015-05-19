functor Abt(structure V : VARIABLE; structure O : OPERATOR) :>
        ABT where type Variable.t = V.t
              and type Operator.t = O.t =
struct

  structure Variable = V
  structure Operator = O

  datatype t
    = BVar of int
    | FVar of Variable.t
    | IOper of Operator.t * t list
    | IBind of t

  datatype 'a view
    = Var  of Variable.t
    | Oper of Operator.t * 'a list
    | Bind of Variable.t * 'a

  exception Malformed

  fun bind v =
      let
          fun go i e =
              case e of
                  FVar v' => if v = v' then BVar i else FVar v'
                | BVar j => if i <= j then BVar (j + 1) else BVar j
                | IOper (oper, args) => IOper (oper, map (go i) args)
                | IBind t => IBind (go (i + 1) t)
      in go 0 end

  fun unbind fv =
      let
          fun go i e =
              case e of
                  FVar v => FVar v
                | BVar j => (
                    case Int.compare (i, j) of
                        GREATER => BVar j
                      | EQUAL => FVar fv
                      | LESS => BVar (j - 1)
                )
                | IOper (oper, args) => IOper (oper, map (go i) args)
                | IBind t => IBind (go (i + 1) t)
      in go 0 end

  fun arity e =
      case e of
          IBind t => 1 + arity t
        | _ => 0

  fun into v =
      case v of
          Var v => FVar v
        | Oper (oper, args) =>
          if Operator.arity oper = map arity args
          then IOper (oper, args)
          else raise Malformed
        |  Bind (x, e) => IBind (bind x e)

  fun out e =
      case e of
          BVar _ => raise Malformed
        | FVar v => Var v
        | IOper (oper, args) => Oper (oper, args)
        | IBind e =>
          let
              val v = Variable.gen "v"
          in Bind (v, unbind v e) end

  val var = into o Var
  fun oper oper args = into (Oper (oper, args))
  fun bind v e = into (Bind (v, e))

  fun aequiv (l, r) =
      case (l, r) of
          (FVar v, FVar v') => v = v'
        | (BVar i, BVar j) => i = j
        | (IBind e, IBind e') => aequiv (e, e')
        | (IOper (oper, args), IOper (oper', args')) =>
          Operator.eq (oper, oper')
          andalso List.all aequiv (ListPair.zip (args, args'))
        | _ => false

  fun map f v =
      case v of
          Var v => Var v
        | Bind (x, e) => Bind (x, f e)
        | Oper (oper, args) => Oper (oper, List.map f args)

  fun dedup [] = []
    | dedup (x :: xs) =
      if List.exists (fn y => x = y) xs
      then dedup xs
      else x :: dedup xs

  fun free e =
      case e of
          FVar v => [v]
        | BVar _ => []
        | IBind e => free e
        | IOper (oper, args) => dedup (List.concat (List.map free args))

  fun subst e v e' =
      case out e' of
          Var v' => if v = v' then e else var v'
        | Bind (v', e') =>
          if v = v' then bind v' e else bind v' (subst e v e')
        | Oper (operator, args) => oper operator (List.map (subst e v) args)

  fun formatArgs [] = ""
    | formatArgs xs =
      List.foldl (fn (x, rest) => rest ^ x ^ ", ")
                 ""
                 (List.take (xs, List.length xs - 1))
      ^ List.last xs

  fun toString t =
      case map toString (out t) of
          Var v => Variable.toString v
        | Bind (v, e) => Variable.toString v ^ ". " ^ e
        | Oper (oper, args) =>
          Operator.toString oper ^ "(" ^ formatArgs args ^ ")"
end
