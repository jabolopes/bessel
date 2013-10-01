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


def id x@ = x

def const x@ @ = x

def not @id = false
      | @ = true

def isDyn = const true

-- def is Int : Dyn -> Bool
--   = isInt

-- def is [A@is] : Dyn -> Bool
--   = isSeq (is A)

-- def is @(a & b) : Dyn -> Bool
--   = isAnd (mkAnd (is a) (is b))

def eq
  x@isBool y@isBool = eqBool x y
| x@isInt  y@isInt  = eqInt x y
| x@isReal y@isReal = eqReal x y
| x@isChar y@isChar = eqChar x y
-- | x@[isDyn] y@[isDyn] = eqSeq x y
    where {
      def eqSeq
        @[] @[] = true
      | (z@ +> zs@) (w@ +> ws@) = eq z w && eqSeq zs ws
      | @ @ = false
    }
| @ @ = false

def lt
  x@isBool y@isBool = ltBool x y
| x@isInt  y@isInt  = ltInt x y
| x@isInt  y@isReal = ltReal (mkReal x) y
| x@isReal y@isInt  = ltReal x (mkReal y)
| x@isReal y@isReal = ltReal x y
| x@isChar y@isChar = ltChar x y
-- | x@[Dyn] y@[Dyn] = ltSeq x y
    where {
      def ltSeq
        @[] @[] = true
      | (z@ +> zs@) (w@ +> ws@) = lt z w && ltSeq zs ws
      | @ @ = false
    }
| @ @ = false

def (==) = eq

def (/=) x@ y@ = not (x == y)

def (<) = lt

def (<=) x@ y@ = x == y || x < y

def (>) x@ y@ = x /= y && not (x < y)

def (>=) x@ y@ = not (x < y)

def isNum x@ = isInt x || isReal x

def isPos x@ = isNum x && x > 0

def isNeg x@ = isNum x && x < 0

def isZero x@ = x == 0

def isNull
  @[] = true
| @ = false

def isPair
  [@,@] = true
| @ = false

-- def isString
--   x@[isChar] = true
-- |  @ = false

def add
  x@isInt  y@isInt  = addInt x y
| x@isReal y@isReal = addReal x y
| x@isInt  y@isReal = addReal (mkReal x) y
| x@isReal y@isInt  = addReal x (mkReal y)

def sub
  x@isInt  y@isInt  = subInt x y
| x@isReal y@isReal = subReal x y
| x@isInt  y@isReal = subReal (mkReal x) y
| x@isReal y@isInt  = subReal x (mkReal y)

def mul
  x@isInt  y@isInt  = mulInt x y
| x@isReal y@isReal = mulReal x y
| x@isInt  y@isReal = mulReal (mkReal x) y
| x@isReal y@isInt  = mulReal x (mkReal y)

def div
  x@isInt  y@isInt  = divInt x y
| x@isReal y@isReal = divReal x y
| x@isInt  y@isReal = divReal (mkReal x) y
| x@isReal y@isInt  = divReal x (mkReal y)

def (+) = add

def (-) = sub

def (*) = mul

def (/) = div

def abs
  x@isInt  = absInt x
| x@isReal = absReal x

def floor
  x@isInt  = x
| x@isReal = floorReal x

def ceiling
  x@isInt  = x
| x@isReal = ceilingReal x

def neg
  x@isInt  = negInt x
| x@isReal = negReal x

def rem
  x@ y@ = remInt x y

def (+>) = al

def (<+) = ar

def length
  @[] = 0
| (@ +> xs@) = length xs + 1

def reverse
  @[] = []
| (x@ +> xs@) = reverse xs <+ x

def map
  fn@ @[] = []
| fn@ (x@ +> xs@) = fn x +> map fn xs
