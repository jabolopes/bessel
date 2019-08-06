# Variants

## Definitions

type Fruit
  = Apple
  | Banana @isInt
  | Fig (@isInt, @isReal)
  | Orange Fruit

let f1
  a@Apple = 0
  b@(Banana x@isInt) = 1
  c@(Fig (x@isInt, y@isReal)) = 2

## Expansions

See Expander.Variant for expansions.
