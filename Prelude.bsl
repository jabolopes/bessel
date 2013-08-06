me Prelude

use Core


-- type List a = null | cons a (List a)

-- sig isNull# : List a -> Bool
-- def isNull# = ...

-- sig isCons# = List a -> Bool
-- def isCons# = ...

-- sig isList# = Dyn -> Bool
-- def isList# = ...

-- sig null = List a
-- def null = null#

-- sig cons = a -> List a -> List a
-- def cons = cons#


-- sig isNull : List a -> Bool
-- def isNull @[] = true
--          | @ = false

-- sig isNull : List a -> Bool
-- def isNull @null = true
--          | @ = false

-- sig isNull : List a -> Bool
-- def isNull x@isNull# = true
--           | @ = false


def id : a -> a
  x@ = x

def const : a -> b -> a
  x@ @ = x

def not : Bool -> Bool
  @id = false
| @ = true

def isDyn : Dyn -> Bool
  = const true

def toDyn : Dyn -> Dyn
  = id

def toBool : Dyn -> Bool
  x@isBool = mkBool x

def toInt : Dyn -> Int
  x@isInt = mkInt x

def toReal : Dyn -> Real
  x@isReal = mkReal x

def toChar : Dyn -> Char
  x@isChar = mkChar x

def toSeq : (Dyn -> a) -> Dyn -> [a]
  fn@ x@(isSeq (const true)) = mkSeq fn x

def eq : Dyn -> Dyn -> Bool
  x@Bool y@Bool = eqBool x y
| x@Int  y@Int  = eqInt x y
| x@Real y@Real = eqReal x y
| x@Char y@Char = eqChar x y
| x@[Dyn] y@[Dyn] = eqSeq x y
    where {
      def eqSeq : [Dyn] -> [Dyn] -> Bool
        @[] @[] = true
      | (z@ +> zs@) (w@ +> ws@) = eq z w && eqSeq zs ws
      | @ @ = false
    }
| @ @ = false

def lt : Dyn -> Dyn -> Bool
  x@Bool y@Bool = ltBool x y
| x@Int  y@Int  = ltInt x y
| x@Int  y@Real = ltReal (toReal x) y
| x@Real y@Int  = ltReal x (toReal y)
| x@Real y@Real = ltReal x y
| x@Char y@Char = ltChar x y
| x@[Dyn] y@[Dyn] = ltSeq x y
    where {
      def ltSeq : [Dyn] -> [Dyn] -> Bool
        @[] @[] = true
      | (z@ +> zs@) (w@ +> ws@) = lt z w && ltSeq zs ws
      | @ @ = false
    }
| @ @ = false

def (==) : Dyn -> Dyn -> Bool
  = eq

def (/=) : Dyn -> Dyn -> Bool
  x@ y@ = not (x == y)

def (<) : Dyn -> Dyn -> Bool
  = lt

def (<=) : Dyn -> Dyn -> Bool
  x@ y@ = x == y || x < y

def (>) : Dyn -> Dyn -> Bool
  x@ y@ = x /= y && not (x < y)

def (>=) : Dyn -> Dyn -> Bool
  x@ y@ = not (x < y)

def isNum : Dyn -> Bool
  x@ = isInt x || isReal x

def isPos : Dyn -> Bool
  x@ = isNum x && x > 0

def isNeg : Dyn -> Bool
  x@ = isNum x && x < 0

def isZero : Int -> Bool
  x@ = x == 0

def isNull : [a] -> Bool
  @[] = true
| @ = false

def isPair : [a] -> Bool
  [@,@] = true
| @ = false

def isString : [Dyn] -> Bool
  x@[Char] = true
|  @ = false

def add : Dyn -> Dyn -> Dyn
  x@isInt  y@isInt  = addInt  (toInt  x) (toInt  y)
| x@isReal y@isReal = addReal (toReal x) (toReal y)
| x@isInt  y@isReal = addReal (toReal x) (toReal y)
| x@isReal y@isInt  = addReal (toReal x) (toReal y)

def sub : Dyn -> Dyn -> Dyn
  x@isInt  y@isInt  = subInt  (toInt  x) (toInt  y)
| x@isReal y@isReal = subReal (toReal x) (toReal y)
| x@isInt  y@isReal = subReal (toReal x) (toReal y)
| x@isReal y@isInt  = subReal (toReal x) (toReal y)

def mul : Dyn -> Dyn -> Dyn
  x@isInt  y@isInt  = mulInt  (toInt  x) (toInt  y)
| x@isReal y@isReal = mulReal (toReal x) (toReal y)
| x@isInt  y@isReal = mulReal (toReal x) (toReal y)
| x@isReal y@isInt  = mulReal (toReal x) (toReal y)

def div : Dyn -> Dyn -> Dyn
  x@isInt  y@isInt  = divInt  (toInt  x) (toInt  y)
| x@isReal y@isReal = divReal (toReal x) (toReal y)
| x@isInt  y@isReal = divReal (toReal x) (toReal y)
| x@isReal y@isInt  = divReal (toReal x) (toReal y)

def (+) : Dyn -> Dyn -> Dyn
  = add

def (-) : Dyn -> Dyn -> Dyn
  = sub

def (*) : Dyn -> Dyn -> Dyn
  = mul

def (/) : Dyn -> Dyn -> Dyn
  = div

-- def abs : Dyn -> Dyn
--   x@isInt  = absInt  (toInt  x)
-- | x@isReal = absReal (toReal x)

def abs : Dyn -> Dyn
  x@Int  = absInt x
| x@Real = absReal x

def floor : Dyn -> Int
  x@Int  = x
| x@Real = floorReal x

def ceiling : Dyn -> Int
  x@Int  = x
| x@Real = ceilingReal x

def neg : Dyn -> Dyn
  x@Int  = negInt x
| x@Real = negReal x

def rem : Int -> Int -> Int
  x@ y@ = remInt x y

def (+>) : a -> [a] -> [a]
  = al

def (<+) : [a] -> a -> [a]
  = ar

def length : [a] -> Int
  @[] = 0
| (@ +> xs@) = toInt (length xs + 1)

def reverse : [a] -> [a]
  @[] = []
| (x@ +> xs@) = reverse xs <+ x

def map : (a -> b) -> [a] -> [b]
  fn@ @[] = []
| fn@ (x@ +> xs@) = fn x +> map fn xs

-- sig index : [a] -> Int -> a
-- def index (x@ +> xs@) n@((==) 0 o ofInt) = x
--         | ( @ +> xs@) n@ = index xs (n - 1)

-- nrdef raise := \f@isfunc isfunc || (seqof isfunc && (¬ isnull)) -> lift f | f

type {x : Int | y : Real}

def f : {x : Int | y : Real}
  = {x = 0 | y = 0.0}

def g : {x : Int | y : Real} -> Int
  v@ = x v


type {Ola.i : Int | Ola.j : Real}

def a : {Ola.i : Int | Ola.j : Real}
  = {Ola.i = 0 | Ola.j = 0.0}

def b : {Ola.i : Int | Ola.j : Real} -> Int
  v@ = Ola.i v
