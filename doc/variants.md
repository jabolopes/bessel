# Variants

## Definitions

type Fruit
  = Apple
  | Banana @isInt
  | Fig (@isInt, @isReal)
  | Orange :Fruit

let f1
  a@Apple = 0
  b@(Banana x@isInt) = 1
  c@(Fig (x@isInt, y@isReal)) = 2

## Expansions

let isFruit = isType# "Fruit"

let isApple : () -> Fruit -> Bool = isVariant# "Fruit" 0
let isBanana : (a -> Bool) -> Fruit -> Bool = isVariant# "Fruit" 1
let isFig : ((a, b) -> Bool) -> Fruit -> Bool = isVariant# "Fruit" 2

let mkApple : Fruit
  = mkVariant# "Fruit" 0 ()
let mkBanana : Int -> Fruit
  x@isInt = mkVariant# "Fruit" 1 x
let Fig : (Int, Real) -> Fruit =
  x@(@isInt, @isReal) = mkVariant# "Fruit" 2 x

let unApple : Fruit -> () = x@isFruit = (r@() -> r) (unVariant# x)
let unBanana : Fruit -> () = x@isFruit = (r@isInt -> r) (unVariant# x)
let unFig : Fruit -> () = x@isFruit = (r@(@isInt, @isReal) -> r) (unVariant# x)

let f1
  \x#0 ->
    cond
      isApple isTuple0 x#0 =
        let a = a#0 in
        0
      isBanana isInt x#0 = let x = x#0 in 0
        let b = x#0 in
        let x = unBanana a#0 in
        1
      isFig (isTuple2 (isInt, isReal)) x#0 =
        let c = x#0 inb
        let x = tuple2Ref0 (unFig a#0) in
        let y = tuple2Ref1 (unFig a#0) in
        2
