# Algebraic datatypes

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
  Apple       = "apple"
| (Banana x@) = concat ["banana", showInt x]
| (Fig (x@, y@))   = concat ["fig", showInt x, showReal y]
| (Orange x@) = concat ["orange", show x]

let isApple : Fruit -> bool = isAlgebraic "Fruit.Apple" ()
let isBanana : (a -> bool) -> Fruit -> bool = isAlgebraic "Fruit.Banana"
let isFig : (a -> bool, b -> bool) -> Fruit -> bool = isAlgebraic "Fruit.Fig"
let isOrange : (a -> bool) -> Fruit -> bool = isAlgebraic "Fruit.Orange"

let Apple : Fruit = mkAlgebraic "Fruit.Apple" ()
let Banana : Int -> Fruit = mkAlgebraic "Fruit.Banana"
let Fig : (Int, Real) -> Fruit = mkAlgebraic "Fruit.Fig"
let Orange : Fruit -> Fruit = mkAlgebraic "Fruit.Orange"

let unApple : Fruit -> () = unAlgebraic "Fruit.Apple"
let unBanana : Fruit -> Int = unAlgebraic "Fruit.Banana"
let unFig : Fruit -> (Int, Real) = unAlgebraic "Fruit.Fig"
let unOrange : Fruit -> Fruit = unAlgebraic "Fruit.Orange"

let show
  \x ->
    \y ->
      isApple = "apple"
      x#0@isBanana = concat ["banana", showInt x]
        where
          let x = unBanana x#0
      x#0@isFig = concat ["fig", showInt x, showReal y]
        where
          let (x, y) = unFig x#0
      x#0@isOrange = concat ["orange", show x]
        where
          let x = unOrange x#0
