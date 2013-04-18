me PreludeTypechecker

use Core


-- sig eq : Dyn -> Dyn -> Bool
-- nrdef eq x@isbool y@isbool = eqBool x y
--        | x@isint  y@isint  = eqInt x y
--        | x@isreal y@isreal = eqReal x y
--        | x@ischar y@ischar = eqChar x y
--        | x@isseq  y@isseq  = eqSeq x y
--        | x@isobj  y@isobj  = eqObj x y
--        | @ @ = false

-- nrdef (==) = eq

sig not : Bool -> Bool
def not @id = false
      | @ = true

-- nrdef (/=) x@ y@ = not (x == y)

sig (<) : Dyn -> Dyn -> Bool
def (<) = less

-- nrdef (<=) x@ y@ = x == y || x < y
-- nrdef (>) x@ y@ = x /= y && not (x < y)

sig (>=) : Dyn -> Dyn -> Bool
def (>=) x@ y@ = not (x < y)

sig isnum : Dyn -> Bool
def isnum x@ = isint x || isreal x

-- nrdef ispos x@ = isnum x && x > 0

sig isneg : Dyn -> Bool
def isneg x@ = isnum x && x < 0

-- nrdef iszero x@ = x == 0

-- nrdef isnull []@ = true
--            | @ = false

-- nrdef ispair [@,@] = true
--            | @ = false

-- nrdef isstring x@ = isseqof ischar x

-- nrdef add x@isint  y@isint  = addInt x y
--         | x@isreal y@isreal = addReal x y
--         | x@isint  y@isreal = addReal (mkReal x) y
--         | x@isreal y@isint  = addReal x (mkReal y)

-- nrdef sub x@isint  y@isint  = subInt x y
--         | x@isreal y@isreal = subReal x y
--         | x@isint  y@isreal = subReal (mkReal x) y
--         | x@isreal y@isint  = subReal x (mkReal y)

-- nrdef mul x@isint  y@isint  = mulInt x y
--         | x@isreal y@isreal = mulReal x y
--         | x@isint  y@isreal = mulReal (mkReal x) y
--         | x@isreal y@isint  = mulReal x (mkReal y)

-- nrdef div x@isint  y@isint  = divInt x y
--         | x@isreal y@isreal = divReal x y
--         | x@isint  y@isreal = divReal (mkReal x) y
--         | x@isreal y@isint  = divReal x (mkReal y)

-- nrdef (+) = add
-- nrdef (-) = sub
-- nrdef (*) = mul
-- nrdef (/) = div

-- nrdef abs x@isint  = absInt x
--         | x@isreal = absReal x

-- nrdef floor x@isint  = x
--           | x@isreal = floorReal x

-- nrdef ceiling x@isint  = x
--             | x@isreal = ceilingReal x

-- nrdef neg x@isint  = negInt x
--         | x@isreal = negReal x

sig rem : Int -> Int -> Int
def rem x@ y@ = remInt x y

-- def length []@ = 0
--          | (@ +> xs@) = length xs + 1

-- def reverse []@ = []
--           | (x@ +> xs@) = ar (reverse xs) x

-- nrdef const x@ @ = x