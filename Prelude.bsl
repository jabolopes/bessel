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
  x@isBool = cast# x

def toInt : Dyn -> Int
  x@isInt = cast# x

def toReal : Dyn -> Real
  x@isReal = cast# x

def toChar : Dyn -> Char
  x@isChar = cast# x

def toSeq : (Dyn -> a) -> Dyn -> [a]
  fn@ x@(isSeq (const true)) = mkSeq fn x

def toAnd : ((Dyn -> a) & (Dyn -> b)) -> Dyn -> a & b
  fns@ x@(isAnd [const true, const true]) = cast# x

-- def is Int : Dyn -> Bool
--   = isInt

-- def is [A@is] : Dyn -> Bool
--   = isSeq (is A)

-- def is @(a & b) : Dyn -> Bool
--   = isAnd (mkAnd (is a) (is b))

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
| x@Int  y@Real = ltReal (mkReal x) y
| x@Real y@Int  = ltReal x (mkReal y)
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
  x@Int  y@Int  = addInt x y
| x@Real y@Real = addReal x y
| x@Int  y@Real = addReal (mkReal x) y
| x@Real y@Int  = addReal x (mkReal y)

def sub : Dyn -> Dyn -> Dyn
  x@Int  y@Int  = subInt x y
| x@Real y@Real = subReal x y
| x@Int  y@Real = subReal (mkReal x) y
| x@Real y@Int  = subReal x (mkReal y)

def mul : Dyn -> Dyn -> Dyn
  x@Int  y@Int  = mulInt x y
| x@Real y@Real = mulReal x y
| x@Int  y@Real = mulReal (mkReal x) y
| x@Real y@Int  = mulReal x (mkReal y)

def div : Dyn -> Dyn -> Dyn
  x@Int  y@Int  = divInt x y
| x@Real y@Real = divReal x y
| x@Int  y@Real = divReal (mkReal x) y
| x@Real y@Int  = divReal x (mkReal y)

def (+) : Dyn -> Dyn -> Dyn
  = add

def (-) : Dyn -> Dyn -> Dyn
  = sub

def (*) : Dyn -> Dyn -> Dyn
  = mul

def (/) : Dyn -> Dyn -> Dyn
  = div

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


-- type {x : Int | y : Real}

-- def f : {x : Int | y : Real}
--   = {x = 0 | y = 0.0}

-- def g : {x : Int | y : Real} -> Int
--   v@ = x v


-- def h : [a] -> Int
--   [x@,y@,z@] = 0

-- def i : [a] -> Int
--   (x@ & (y@ & z@)) = 0

-- def j : {x : Int} & a -> {x : Int} & a
--   ({x@} & y@) = {x = x + 1} & y


-- def x : Int & Int -> Int
--   (a@ & b@) = a

-- def ola : Int & Int & Int
--   = andLeft 1 (andLeft 2 3)

-- def fst : a & b -> a
--   (x@ & y@) = x
-- | (x@ & (y@ & z@)) = x

--
-- def f : Dyn -> ...
--   x@(@isInt || @isReal) = g x

--
-- def f : Dyn -> ...
--   (x@isInt || y@isReal) = g x y

--
-- def f : Dyn -> ...
--   x@(@Int || @Real) = g x

--
-- def f : Dyn -> ...
--   (x@Int || y@Real) = g x y

--
-- def f : Dyn -> ...
--   x@Int = g x
--   x@Real = g x

--
-- def f : {Int | Real} -> ...
--   x@Int = g x
--   x@Real = g x

--
-- def f : {int Int | real Real} -> ...
--   (int x) = g x
--   (real x) = g x

--
-- type Num = {int Int | real Real}

-- int : Int -> Num
-- real : Int -> Num

-- def isNum : Dyn -> Bool
--   = isOr [isInt, isReal]

-- def toNum : Dyn -> {int Int | real Real}
--   x@Int  = int x
--   x@Real = real x

-- def f : Num -> ...
--   (int x) = g x
--   (real x) = g x




--
-- def f : Dyn -> Bool
--   x@(@isInt & @isReal) = true

--
-- def f : Dyn -> ...
--   x@isInt & y@isReal = g x y

--
-- def f : Dyn -> ...
--   x@(@Int & y@Real) = g x

--
-- def f : Dyn -> ...
--   x@Int & y@Real = g x y

--
-- def f : {Int & Real} -> ...
--   x@ & y@ = g x y

--
-- def f : {fst Int & snd Real} -> ...
--   (fst x) & (snd y) = g x y

--
-- type Coord = {fst Int & snd Real}

-- fst : Coord -> Int
-- snd : Coord -> Real

-- def f : Coord -> ...
--   (fst x) & (snd y) = g x y



-- type {Ola.i : Int | Ola.j : Real}

-- def a : {Ola.i : Int | Ola.j : Real}
--   = {Ola.i = 0 | Ola.j = 0.0}

-- def b : {Ola.i : Int | Ola.j : Real} -> Int
--   v@ = Ola.i v


def adeus : Dyn -> Int
 l@[x@Int, y@, isInt] = 0
