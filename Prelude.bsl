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

def toBool : Dyn -> Bool
  x@isBool = x

def toInt : Dyn -> Int
  x@isInt = x

def toReal : Dyn -> Real
  x@isReal = x

def toChar : Dyn -> Char
  x@isChar = x

def toSeq : Dyn -> [Dyn]
  x@isSeq = x

def ofInt : Int -> Dyn
  x@isInt = x

def eq : Dyn -> Dyn -> Bool
  x@isBool y@isBool = eqBool (toBool x) (toBool y)
| x@isInt  y@isInt  = eqInt  (toInt  x) (toInt  y)
| x@isReal y@isReal = eqReal (toReal x) (toReal y)
| x@isChar y@isChar = eqChar (toChar x) (toChar y)
| seq1@isSeq seq2@isSeq = eqSeq (toSeq seq1) (toSeq seq2)
    where {
      def eqSeq : [Dyn] -> [Dyn] -> Bool
        @[] @[] = true
      | (x@ +> xs@) (y@ +> ys@) = eq x y && eqSeq xs ys
      | @ @ = false
    }
-- | x@isobj  y@isobj  = eqObj x y
| @ @ = false

def lt : Dyn -> Dyn -> Bool
  x@isBool y@isBool = ltBool (toBool x) (toBool y)
| x@isInt  y@isInt  = ltInt  (toInt  x) (toInt  y)
| x@isInt  y@isReal = ltReal (mkReal x) (toReal y)
| x@isReal y@isInt  = ltReal (toReal x) (mkReal y)
| x@isReal y@isReal = ltReal (toReal x) (toReal y)
| x@isChar y@isChar = ltChar (toChar x) (toChar y)
| x@isSeq  y@isSeq  = ltSeq  (toSeq  x) (toSeq  y)
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
  x@ = isSeqOf isChar x

def add : Dyn -> Dyn -> Dyn
  x@isInt  y@isInt  = addInt  (toInt  x) (toInt  y)
| x@isReal y@isReal = addReal (toReal x) (toReal y)
| x@isInt  y@isReal = addReal (mkReal x) (toReal y)
| x@isReal y@isInt  = addReal (toReal x) (mkReal y)

def sub : Dyn -> Dyn -> Dyn
  x@isInt  y@isInt  = subInt  (toInt  x) (toInt  y)
| x@isReal y@isReal = subReal (toReal x) (toReal y)
| x@isInt  y@isReal = subReal (mkReal x) (toReal y)
| x@isReal y@isInt  = subReal (toReal x) (mkReal y)

def mul : Dyn -> Dyn -> Dyn
  x@isInt  y@isInt  = mulInt  (toInt  x) (toInt  y)
| x@isReal y@isReal = mulReal (toReal x) (toReal y)
| x@isInt  y@isReal = mulReal (mkReal x) (toReal y)
| x@isReal y@isInt  = mulReal (toReal x) (mkReal y)

def div : Dyn -> Dyn -> Dyn
  x@isInt  y@isInt  = divInt  (toInt  x) (toInt  y)
| x@isReal y@isReal = divReal (toReal x) (toReal y)
| x@isInt  y@isReal = divReal (mkReal x) (toReal y)
| x@isReal y@isInt  = divReal (toReal x) (mkReal y)

def (+) : Dyn -> Dyn -> Dyn
  = add

def (-) : Dyn -> Dyn -> Dyn
  = sub

def (*) : Dyn -> Dyn -> Dyn
  = mul

def (/) : Dyn -> Dyn -> Dyn
  = div

def abs : Dyn -> Dyn
  x@isInt  = absInt  (toInt  x)
| x@isReal = absReal (toReal x)

def floor : Dyn -> Int
  x@isInt  = toInt x
| x@isReal = floorReal (toReal x)

def ceiling : Dyn -> Int
  x@isInt  = toInt x
| x@isReal = ceilingReal (toReal x)

def neg : Dyn -> Dyn
  x@isInt  = negInt  (toInt  x)
| x@isReal = negReal (toReal x)

def rem : Int -> Int -> Int
  x@ y@ = remInt x y

def (+>) : a -> [a] -> [a]
  = al

def (<+) : [a] -> a -> [a]
  = ar

def length : [a] -> Int
  @[] = 0
| (@ +> xs@) = length xs + 1

def reverse : [a] -> [a]
  @[] = []
| (x@ +> xs@) = reverse xs <+ x

-- sig index : [a] -> Int -> a
-- def index (x@ +> xs@) n@((==) 0 o ofInt) = x
--         | ( @ +> xs@) n@ = index xs (n - 1)

-- nrdef raise := \f@isfunc isfunc || (seqof isfunc && (¬ isnull)) -> lift f | f

type {x : Int | y : Real}

def f : {x : Int | y : Real}
  = {x = 0 & y = 0.0}

def g : {x : Int | y : Real} -> Int
  v@ = x v


type {Ola.i : Int | Ola.j : Real}

def a : {Ola.i : Int | Ola.j : Real}
  = {Ola.i = 0 & Ola.j = 0.0}

def b : {Ola.i : Int | Ola.j : Real} -> Int
  v@ = Ola.i v


def adeus : Int -> Int
  x@isZero = 0
| x@       = addInt (adeus (subInt x 1)) 2
