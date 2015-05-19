## Just Simple Abts

So I can scratch stuff out quickly and without thinking. In SML to aid in the
process of not thinking too hard.

## How To Use This

If you want to use this library here's the basic procedure.

 1. Copy this directory into your project's.
 2. Add `simple-abt/sources.cm` to your `sources.cm`
 3. Define a module for your language's operators

    Essentially you define a normal SML type which represents each branch of
    your ABT. However, this type shouldn't be recursive! You then should define
    the function `eq` and `toString` which do the right things for your
    operators. Finally, define `arity`. Given an operator this should return a
    list of numbers. There should be one number for each recursive component of
    this node in the ABT. Each number should represent how many variables each
    of those nodes should bind.

 4. Get your language's ABT for *free* with

        Abt(structure O = Operator; structure V = Variable)


Consult the signatures for more details. You can find an example of the untyped
lambda calculus in `test/lam.sml`.
