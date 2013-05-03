me Prelude

use Core


sig id : a -> a
def id x@ = x

sig const : a -> b -> a
def const x@ @ = x

sig not : Bool -> Bool
def not @id = false
      | @ = true

sig toBool : Dyn -> Bool
def toBool x@isBool = x

sig toInt : Dyn -> Int
def toInt x@isInt = x

sig toReal : Dyn -> Real
def toReal x@isReal = x

sig toChar : Dyn -> Char
def toChar x@isChar = x

sig toSeq : Dyn -> [Dyn]
def toSeq x@isSeq = x

sig eq : Dyn -> Dyn -> Bool
def eq x@isBool y@isBool = eqBool (toBool x) (toBool y)
     | x@isInt  y@isInt  = eqInt (toInt x) (toInt y)
     | x@isReal y@isReal = eqReal (toReal x) (toReal y)
     | x@isChar y@isChar = eqChar (toChar x) (toChar y)
     | seq1@isSeq seq2@isSeq = eqSeq (toSeq seq1) (toSeq seq2)
                           where {
                             sig eqSeq : [Dyn] -> [Dyn] -> Bool
                             def eqSeq @[] @[] = true
                                     | (x@ +> xs@) (y@ +> ys@) = eq x y && eqSeq xs ys
                                     | @ @ = false
                           }
     -- | x@isobj  y@isobj  = eqObj x y
     | @ @ = false

sig lt : Dyn -> Dyn -> Bool
def lt x@isBool y@isBool = eqBool (toBool x) (toBool y)
     | x@isInt  y@isInt  = ltInt (toInt x) (toInt y)
     | x@isInt  y@isReal = ltReal (mkReal x) (toReal y)
     | x@isReal y@isInt  = ltReal (toReal x) (mkReal y)
     | x@isReal y@isReal = ltReal (toReal x) (toReal y)
     | x@isChar y@isChar = ltChar (toChar x) (toChar y)
     | x@isSeq  y@isSeq  = ltSeq (toSeq x) (toSeq y)
     | @ @ = false

sig (==) : Dyn -> Dyn -> Bool
def (==) = eq

sig (/=) : Dyn -> Dyn -> Bool
def (/=) x@ y@ = not (x == y)

sig (<) : Dyn -> Dyn -> Bool
def (<) = lt

sig (<=) : Dyn -> Dyn -> Bool
def (<=) x@ y@ = x == y || x < y

sig (>) : Dyn -> Dyn -> Bool
def (>) x@ y@ = x /= y && not (x < y)

sig (>=) : Dyn -> Dyn -> Bool
def (>=) x@ y@ = not (x < y)

sig isNum : Dyn -> Bool
def isNum x@ = isInt x || isReal x

sig isPos : Dyn -> Bool
def isPos x@ = isNum x && x > 0

sig isNeg : Dyn -> Bool
def isNeg x@ = isNum x && x < 0

sig isZero : Int -> Bool
def isZero x@ = x == 0

sig isNull : [a] -> Bool
def isNull @[] = true
         | @ = false

sig isPair : [a] -> Bool
def isPair [@,@] = true
         | @ = false

sig isString : [Dyn] -> Bool
def isString x@ = isSeqOf isChar x

sig add : Dyn -> Dyn -> Dyn
def add x@isInt  y@isInt  = addInt (toInt x) (toInt y)
      | x@isReal y@isReal = addReal (toReal x) (toReal y)
      | x@isInt  y@isReal = addReal (mkReal x) (toReal y)
      | x@isReal y@isInt  = addReal (toReal x) (mkReal y)

sig sub : Dyn -> Dyn -> Dyn
def sub x@isInt  y@isInt  = subInt (toInt x) (toInt y)
      | x@isReal y@isReal = subReal (toReal x) (toReal y)
      | x@isInt  y@isReal = subReal (mkReal x) (toReal y)
      | x@isReal y@isInt  = subReal (toReal x) (mkReal y)

sig mul : Dyn -> Dyn -> Dyn
def mul x@isInt  y@isInt  = mulInt (toInt x) (toInt y)
      | x@isReal y@isReal = mulReal (toReal x) (toReal y)
      | x@isInt  y@isReal = mulReal (mkReal x) (toReal y)
      | x@isReal y@isInt  = mulReal (toReal x) (mkReal y)

sig div : Dyn -> Dyn -> Dyn
def div x@isInt  y@isInt  = divInt (toInt x) (toInt y)
      | x@isReal y@isReal = divReal (toReal x) (toReal y)
      | x@isInt  y@isReal = divReal (mkReal x) (toReal y)
      | x@isReal y@isInt  = divReal (toReal x) (mkReal y)

sig (+) : Dyn -> Dyn -> Dyn
def (+) = add

sig (-) : Dyn -> Dyn -> Dyn
def (-) = sub

sig (*) : Dyn -> Dyn -> Dyn
def (*) = mul

sig (/) : Dyn -> Dyn -> Dyn
def (/) = div

sig abs : Dyn -> Dyn
def abs x@isInt  = absInt (toInt x)
      | x@isReal = absReal (toReal x)

sig floor : Dyn -> Int
def floor x@isInt  = toInt x
        | x@isReal = floorReal (toReal x)

sig ceiling : Dyn -> Int
def ceiling x@isInt  = toInt x
          | x@isReal = ceilingReal (toReal x)

sig neg : Dyn -> Dyn
def neg x@isInt  = negInt (toInt x)
      | x@isReal = negReal (toReal x)

sig rem : Int -> Int -> Int
def rem x@ y@ = remInt x y

sig (+>) : a -> [a] -> [a]
def (+>) = al

sig (<+) : [a] -> a -> [a]
def (<+) = ar

sig length : [a] -> Int
def length @[] = 0
         | (@ +> xs@) = length xs + 1

sig reverse : [a] -> [a]
def reverse @[] = []
          | (x@ +> xs@) = ar (reverse xs) x

-- nrdef raise := \f@isfunc isfunc || (seqof isfunc && (¬ isnull)) -> lift f | f