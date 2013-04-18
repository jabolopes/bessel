me Prelude

use Core


sig eq : Dyn -> Dyn -> Bool
def eq x@isbool y@isbool = eqBool x y
     | x@isint  y@isint  = eqInt x y
     | x@isreal y@isreal = eqReal x y
     | x@ischar y@ischar = eqChar x y
     | x@isseq  y@isseq  = eqSeq x y
     | x@isobj  y@isobj  = eqObj x y
     | @ @ = false

def (==) = eq

sig not : Bool -> Bool
def not @id = false
      | @ = true

def (/=) x@ y@ = not (x == y)

sig (<) : Dyn -> Dyn -> Bool
def (<) = less

def (<=) x@ y@ = x == y || x < y
def (>) x@ y@ = x /= y && not (x < y)

sig (>=) : Dyn -> Dyn -> Bool
def (>=) x@ y@ = not (x < y)

sig isnum : Dyn -> Bool
def isnum x@ = isint x || isreal x

def ispos x@ = isnum x && x > 0

sig isneg : Dyn -> Bool
def isneg x@ = isnum x && x < 0

def iszero x@ = x == 0

def isnull []@ = true
         | @ = false

def ispair [@,@] = true
         | @ = false

def isstring x@ = isseqof ischar x

def add x@isint  y@isint  = addInt x y
      | x@isreal y@isreal = addReal x y
      | x@isint  y@isreal = addReal (mkReal x) y
      | x@isreal y@isint  = addReal x (mkReal y)

def sub x@isint  y@isint  = subInt x y
      | x@isreal y@isreal = subReal x y
      | x@isint  y@isreal = subReal (mkReal x) y
      | x@isreal y@isint  = subReal x (mkReal y)

def mul x@isint  y@isint  = mulInt x y
      | x@isreal y@isreal = mulReal x y
      | x@isint  y@isreal = mulReal (mkReal x) y
      | x@isreal y@isint  = mulReal x (mkReal y)

def div x@isint  y@isint  = divInt x y
      | x@isreal y@isreal = divReal x y
      | x@isint  y@isreal = divReal (mkReal x) y
      | x@isreal y@isint  = divReal x (mkReal y)

def (+) = add
def (-) = sub
def (*) = mul
def (/) = div

def abs x@isint  = absInt x
      | x@isreal = absReal x

def floor x@isint  = x
        | x@isreal = floorReal x

def ceiling x@isint  = x
          | x@isreal = ceilingReal x

def neg x@isint  = negInt x
      | x@isreal = negReal x

sig rem : Int -> Int -> Int
def rem x@ y@ = remInt x y

def length []@ = 0
         | (@ +> xs@) = length xs + 1

def reverse []@ = []
          | (x@ +> xs@) = ar (reverse xs) x

def const x@ @ = x

-- nrdef raise := \f@isfunc isfunc || (seqof isfunc && (¬ isnull)) -> lift f | f