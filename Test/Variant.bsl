me Test.Variant

type Fruit =
  Apple
  | Banana x@isInt
  | Fig (@isInt, @isReal)
  | Orange @isFruit
  | Lemon (Orange @isFruit)
  | Melon Fruit

let f1
  a@Apple = 0
  b@(Banana x@isInt) = 1
  c@(Fig (x@isInt, y@isReal)) = 2
  d@(Orange x@isFruit) = 3
  e@(Lemon (Orange x@isFruit)) = 4
  f@(Melon x@isFruit) = 5
