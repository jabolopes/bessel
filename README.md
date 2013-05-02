bsl
===

#### The Bessel programming language

A FL and Haskell inspired, gradually typed programming language with
automatic type inference.


Examples
--------

These are few examples from the 'Prelude' to exemplify polymorphic
types, dynamic types, pattern matching, pattern guards, and
user-defined operators, among other features.

    sig id : a -> a
    def id x@ = x

    sig const : a -> b -> a
    def const x@ @ = x

    sig add : Dyn -> Dyn -> Dyn
    def add x@isInt  y@isInt  = addInt (toInt x) (toInt y)
      | x@isReal y@isReal = addReal (toReal x) (toReal y)
      | x@isInt  y@isReal = addReal (mkReal x) (toReal y)
      | x@isReal y@isInt  = addReal (toReal x) (mkReal y)

    sig length : [a] -> Int
    def length @[] = 0
         | (@ +> xs@) = length xs + 1

    sig (+) : Dyn -> Dyn -> Dyn
    def (+) = add