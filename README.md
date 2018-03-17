This library uses Backpack, prisms and Plated to abstract over
lambda-calculus-based languages and their common operations.

Given some `Term` (lambda-term-sig/Lambda/Term.hsig) that has at least these
three constructors:

* `Var` (lambda-var-sig/Lambda/Var.hsig)
* `Abs` (lambda-abs-sig/Lambda/Abs.hsig)
* `App` (lambda-app-sig/Lambda/App.hsig)

We can implement capture-avoiding substitution and beta-reduction
(src/Lambda.hs)
