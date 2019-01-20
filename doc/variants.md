# Variants

## Definitions

type Fruit
  = Apple
  | Banana @isInt
  | Fig [@isInt, @isReal]
  | Orange @Fruit

let show
  Apple  = "apple"
| Banana = "banana"
| Fig    = "fig"
| Orange = "orange"

let show
  Apple       = "apple"
| (Banana x@) = concat ["banana", showInt x]
| (Fig xs@)   = concat ["fig", showListInt xs]
| (Orange x@) = concat ["orange", show x]

## Expansions

let isApple = isCons# (link# "Apple")
let isBanana = isCons# (link# "Banana")
let isFig = isCons# (link# "Fig")
let isOrange = isCons# (link# "Orange")

let isFruit x@ = isApple x || isBanana x || isFig x || isOrange x

let Apple = mkCons# (link# "Apple") 0
let Banana x#0@isInt = mkCons# (link# "Banana") x#0
let Fig x#0@[isInt, isReal] = mkCons# (link# "Fig") x#0
let Orange x#0@Fruit = mkCons# (link# "Orange") x#0

let show
  isApple  = "apple"
  isBanana = "banana"
  isFig    = "fig"
  isOrange = "orange"

let show
  isApple = "apple"
  x#0@isBanana = concat ["banana", showInt x]
    where let x = unCons# x#0
  x#1@isFig = concat ["fig", showListInt xs]
    where let xs = unCons# x#1
  x#2@isOrange = concat ["orange", show x]
    where let x = unCons# x#2


## Definitions and expansions (tuples)

type Fruit
  = Apple
  | Banana isInt
  | Fig (isInt, isReal)
  | Orange Fruit

let show
  Apple                     = "apple"
| (Banana x@isInt)          = concat ["banana", showInt x]
| (Fig (x@isInt, y@isReal)) = concat ["fig", showInt x, showReal y]
| (Orange x@)               = concat ["orange", show x]

let isApple : Fruit -> Bool = isVariant "Fruit" 0 isTuple0
let isBanana : (Int -> Bool) -> Fruit -> Bool = isVariant "Fruit" 1
let isFig : (Int -> Bool, Int -> Bool) -> Fruit -> Bool = isVariant "Fruit" 2 . isTuple2
let isOrange : (Fruit -> Bool) -> Fruit -> Bool = isVariant "Fruit" 3

let Apple : Fruit = mkVariant "Fruit" 0 ()
let Banana : Int -> Fruit = mkVariant "Fruit" 1
let Fig : (Int, Real) -> Fruit = mkVariant "Fruit" 2
let Orange : Fruit -> Fruit = mkVariant "Fruit" 3

let unApple : Fruit -> () = unVariant
let unBanana : Fruit -> Int = unVariant
let unFig : Fruit -> (Int, Real) = unVariant
let unOrange : Fruirt -> Fruit = unVariant

let show
  \x#0 ->
    cond
      isApple x#0 = "apple"
      isBanana isInt x#0 = concat ["banana", showInt x]
        where
          let x = unBanana x#0
      isFig (isInt, isReal) x#0 = concat ["fig", showInt x, showReal y]
        where
          let (x, y) = unFig x#0
      isOrange = concat ["orange", show x]
        where
          let x = unOrange x#0
