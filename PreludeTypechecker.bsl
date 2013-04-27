me PreludeTypechecker

use Core


sig toBool : Dyn -> Bool
def toBool x@isbool = x

sig toInt : Dyn -> Int
def toInt x@isint = x

sig toReal : Dyn -> Real
def toReal x@isreal = x

sig toChar : Dyn -> Char
def toChar x@ischar = x

sig toSeq : Dyn -> [Dyn]
def toSeq x@isseq = x

sig eq : Dyn -> Dyn -> Bool
def eq x@isbool y@isbool = eqBool (toBool x) (toBool y)
     | x@isint  y@isint  = eqInt (toInt x) (toInt y)
     | x@isreal y@isreal = eqReal (toReal x) (toReal y)
     | x@ischar y@ischar = eqChar (toChar x) (toChar y)
     | x@isseq  y@isseq  = eqSeq (toSeq x) (toSeq y)
     -- | x@isobj  y@isobj  = eqObj x y
     | @ @ = false

sig (==) : Dyn -> Dyn -> Bool
def (==) = eq

sig not : Bool -> Bool
def not @id = false
      | @ = true

sig (/=) : Dyn -> Dyn -> Bool
def (/=) x@ y@ = not (x == y)

sig (<) : Dyn -> Dyn -> Bool
def (<) = less

sig (<=) : Dyn -> Dyn -> Bool
def (<=) x@ y@ = x == y || x < y

sig (>) : Dyn -> Dyn -> Bool
def (>) x@ y@ = x /= y && not (x < y)

sig (>=) : Dyn -> Dyn -> Bool
def (>=) x@ y@ = not (x < y)

sig isnum : Dyn -> Bool
def isnum x@ = isint x || isreal x

sig ispos : Dyn -> Bool
def ispos x@ = isnum x && x > 0

sig isneg : Dyn -> Bool
def isneg x@ = isnum x && x < 0

sig iszero : Int -> Bool
def iszero x@ = x == 0

-- def isnull []@ = true
--          | @ = false

-- def ispair [@,@] = true
--          | @ = false

-- def isstring x@ = isseqof ischar x

sig add : Dyn -> Dyn -> Dyn
def add x@isint  y@isint  = addInt (toInt x) (toInt y)
      | x@isreal y@isreal = addReal (toReal x) (toReal y)
      | x@isint  y@isreal = addReal (mkReal x) (toReal y)
      | x@isreal y@isint  = addReal (toReal x) (mkReal y)

sig sub : Dyn -> Dyn -> Dyn
def sub x@isint  y@isint  = subInt (toInt x) (toInt y)
      | x@isreal y@isreal = subReal (toReal x) (toReal y)
      | x@isint  y@isreal = subReal (mkReal x) (toReal y)
      | x@isreal y@isint  = subReal (toReal x) (mkReal y)

sig mul : Dyn -> Dyn -> Dyn
def mul x@isint  y@isint  = mulInt (toInt x) (toInt y)
      | x@isreal y@isreal = mulReal (toReal x) (toReal y)
      | x@isint  y@isreal = mulReal (mkReal x) (toReal y)
      | x@isreal y@isint  = mulReal (toReal x) (mkReal y)

sig div : Dyn -> Dyn -> Dyn
def div x@isint  y@isint  = divInt (toInt x) (toInt y)
      | x@isreal y@isreal = divReal (toReal x) (toReal y)
      | x@isint  y@isreal = divReal (mkReal x) (toReal y)
      | x@isreal y@isint  = divReal (toReal x) (mkReal y)

sig (+) : Dyn -> Dyn -> Dyn
def (+) = add

sig (-) : Dyn -> Dyn -> Dyn
def (-) = sub

sig (*) : Dyn -> Dyn -> Dyn
def (*) = mul

sig (/) : Dyn -> Dyn -> Dyn
def (/) = div

sig abs : Dyn -> Dyn
def abs x@isint  = absInt (toInt x)
      | x@isreal = absReal (toReal x)

sig floor : Dyn -> Int
def floor x@isint  = toInt x
        | x@isreal = floorReal (toReal x)

sig ceiling : Dyn -> Int
def ceiling x@isint  = toInt x
          | x@isreal = ceilingReal (toReal x)

sig neg : Dyn -> Dyn
def neg x@isint  = negInt (toInt x)
      | x@isreal = negReal (toReal x)

sig rem : Int -> Int -> Int
def rem x@ y@ = remInt x y

-- def length []@ = 0
--          | (@ +> xs@) = length xs + 1

-- def reverse []@ = []
--           | (x@ +> xs@) = ar (reverse xs) x

-- nrdef const x@ @ = x