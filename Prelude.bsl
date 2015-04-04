me Prelude

use Core

-- Fn

let id x = x
let const x @ = x

-- Bool

let isBool = isBool#
let true = true#
let false = false#
let eqBool = eqBool#

let not
  id = false
  @  = true

-- Int

let isInt = isInt#
let eqInt = eqInt#
let ltInt = ltInt#
let addInt = addInt#
let subInt = subInt#
let mulInt = mulInt#
let divInt = divInt#
let absInt = absInt#
let negInt = negInt#
let invInt = negInt#
let remInt = remInt#

-- Real

let isReal = isReal#
let eqReal = eqReal#
let ltReal = ltReal#
let addReal = addReal#
let subReal = subReal#
let mulReal = mulReal#
let divReal = divReal#
let absReal = absReal#
let ceilingReal = ceilingReal#
let floorReal = floorReal#
let negReal = negReal#
let invReal = invReal#

-- Int and Real

let ltIntReal = ltIntReal#
let ltRealInt = ltRealInt#
let addIntReal = addIntReal#
let addRealInt x y = addIntReal y x
let subIntReal x y = addIntReal x (negReal y)
let subRealInt x y = addRealInt x (negInt y)
let mulIntReal = mulIntReal#
let mulRealInt x y = mulIntReal y x
let divIntReal x y = mulIntReal x (invReal y)
let divRealInt x y = mulReal x (invInt y)

-- Char

let isChar = isChar#
let eqChar = eqChar#
let ltChar = ltChar#

-- Seq

let null = null#
let cons = cons#
let isTuple = isTuple#
let isList = isList#
let hd = hd#
let tl = tl#
let (+>) = cons

let map
  fn [] = []
  fn (x +> xs) = fn x +> map fn xs

let eq
  x@isBool y@isBool = eqBool x y
  x@isInt  y@isInt  = eqInt x y
  x@isReal y@isReal = eqReal x y
  x@isChar y@isChar = eqChar x y
  x y = eqSeq x y
  @ @ = false
where
  let eqSeq
    [] [] = true
    (z +> zs) (w +> ws) = eq z w && eqSeq zs ws
    @ @ = false

let lt
  x@isInt  y@isInt  = ltInt x y
  x@isInt  y@isReal = ltIntReal x y
  x@isReal y@isInt  = ltRealInt x y
  x@isReal y@isReal = ltReal x y
  x@isChar y@isChar = ltChar x y
  x y = ltSeq x y
where
  let ltSeq
    [] [] = true
    (z +> zs) (w +> ws) = lt z w && ltSeq zs ws
    @ @ = false

let (==) = eq

let (/=) x y = not (x == y)

let (<) = lt

let (<=) x y = x == y || x < y

let (>) x y = x /= y && not (x < y)

let (>=) x y = not (x < y)

let isNum x = isInt x || isReal x

let isPos x = isNum x && x > 0

let isNeg x = isNum x && x < 0

let isZero x = x == 0

let isNull
  [] = true
  @ = false

let isPair
  [@,@] = true
  @ = false

let isString
  []                   = true
  (isChar +> isString) = true
  @                    = false

-- Number

let add
  x@isInt  y@isInt  = addInt x y
  x@isReal y@isReal = addReal x y
  x@isInt  y@isReal = addIntReal x y
  x@isReal y@isInt  = addRealInt x y

let sub
  x@isInt  y@isInt  = subInt x y
  x@isReal y@isReal = subReal x y
  x@isInt  y@isReal = subIntReal x y
  x@isReal y@isInt  = subRealInt x y

let mul
  x@isInt  y@isInt  = mulInt x y
  x@isReal y@isReal = mulReal x y
  x@isInt  y@isReal = mulIntReal x y
  x@isReal y@isInt  = mulRealInt x y

let div
  x@isInt  y@isInt  = divInt x y
  x@isReal y@isReal = divReal x y
  x@isInt  y@isReal = divIntReal x y
  x@isReal y@isInt  = divRealInt x y

let abs
  x@isInt  = absInt x
  x@isReal = absReal x

let floor
  x@isInt  = x
  x@isReal = floorReal x

let ceiling
  x@isInt  = x
  x@isReal = ceilingReal x

let neg
  x@isInt  = negInt x
  x@isReal = negReal x

let rem x y = remInt x y

let (+) = add
let (-) = sub
let (*) = mul
let (/) = div

let length
  [] = 0
  (@ +> xs) = length xs + 1

-- let reverse
--   [] = []
--   (x@ +> xs@) = reverse xs <+ x

-- Io

let readLine = readLine#
let putLine = putLine#
let (>>) = bindIO#

let ola
  1 = true
  @ = false

let adeus
  "ola" = true
  @     = false

let case x fn = fn x

let if x then else = 0

let ($) fn x = fn x

let foo x =
  (isInt = true
  @      = false) x

let bar x =
  case x
    (isInt = true
    @      = false)

let foobar x =
  case x $
     isInt = true
     @     = false

type Fruit Apple isInt

let ola1 = Apple 1
let ola2 = Apple 2

let adeus1 (Apple x) = x
let adeus2 (Apple x y) = x
let adeus3 (Apple x@isInt) = x

let pol
  x@0 = 0
  x  = pol (x - 1) + 1

let one
  x@1 = 1
  @   = 0

let oi filename =
  mapFile# filename fn
where
  let fn str = adeus str

let f n =
  let [x, y] = [1, 2] in
  case n
    ((<) 0) = x
    @       = y

let [g, h] = [1, 2]
