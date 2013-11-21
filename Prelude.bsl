me Prelude

use Core

-- Fn

def id x@ = x
def const x@ @ = x

-- Bool

def isBool isBool#
def true true#
def false false#
def eqBool eqBool#

def not @id = false
      | @ = true

-- Int

def isInt isInt#
def eqInt eqInt#
def ltInt ltInt#
def addInt addInt#
def subInt subInt#
def mulInt mulInt#
def divInt divInt#
def absInt absInt#
def negInt negInt#
def invInt negInt#
def remInt remInt#

-- Real

def isReal isReal#
def eqReal eqReal#
def ltReal ltReal#
def addReal addReal#
def subReal subReal#
def mulReal mulReal#
def divReal divReal#
def absReal absReal#
def ceilingReal ceilingReal#
def floorReal floorReal#
def negReal negReal#
def invReal invReal#

-- Int and Real

def ltIntReal ltIntReal#
def ltRealInt ltRealInt#
def addIntReal addIntReal#
def addRealInt x@ y@ = addIntReal y x
def subIntReal x@ y@ = addIntReal x (negReal y)
def subRealInt x@ y@ = addRealInt x (negInt y)
def mulIntReal mulIntReal#
def mulRealInt x@ y@ = mulIntReal y x
def divIntReal x@ y@ = mulIntReal x (invReal y)
def divRealInt x@ y@ = mulReal x (invInt y)

-- Char

def isChar isChar#
def eqChar eqChar#
def ltChar ltChar#

-- Seq

def null null#
def cons cons#
def isTuple isTuple#
def isList isList#
def hd hd#
def tl tl#
def (+>) cons

def map
  fn@ @[] = []
| fn@ (x@ +> xs@) = fn x +> map fn xs

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
  x@isInt  y@isInt  = ltInt x y
| x@isInt  y@isReal = ltIntReal x y
| x@isReal y@isInt  = ltRealInt x y
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

def (==) eq

def (/=) x@ y@ = not (x == y)

def (<) lt

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
  @[@,@] = true
| @ = false

-- def isString
--   x@[isChar] = true
-- |  @ = false

-- Number

def add
  x@isInt  y@isInt  = addInt x y
| x@isReal y@isReal = addReal x y
| x@isInt  y@isReal = addIntReal x y
| x@isReal y@isInt  = addRealInt x y

def sub
  x@isInt  y@isInt  = subInt x y
| x@isReal y@isReal = subReal x y
| x@isInt  y@isReal = subIntReal x y
| x@isReal y@isInt  = subRealInt x y

def mul
  x@isInt  y@isInt  = mulInt x y
| x@isReal y@isReal = mulReal x y
| x@isInt  y@isReal = mulIntReal x y
| x@isReal y@isInt  = mulRealInt x y

def div
  x@isInt  y@isInt  = divInt x y
| x@isReal y@isReal = divReal x y
| x@isInt  y@isReal = divIntReal x y
| x@isReal y@isInt  = divRealInt x y

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

def (+) add
def (-) sub
def (*) mul
def (/) div

def length
  @[] = 0
| (@ +> xs@) = length xs + 1

-- def reverse
--   @[] = []
-- | (x@ +> xs@) = reverse xs <+ x

def ola
  @1 = true
| @  = false

def adeus
  @"ola" = true
| @      = false

def case x@ fn@ = fn x

def if x@ then@ else@ = 0

def foo x@ =
  (@isInt = true
 | @      = false) x

def bar x@ =
  case x
    (@isInt = true
   | @      = false)
