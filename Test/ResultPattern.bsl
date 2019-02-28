me Test.ResultPattern

let 'a' = 'a'

let patChar@'a' = 'a'

let 0 = 0

let patInt@0 = 0

let 0.0 = 0.0

let patReal@0.0 = 0.0

let "hello" = "hello"

let patString@"hello" = "hello"

let (Apple x@isInt) = mkApple0

let patVariantTag@(Apple x@isInt) = mkApple 0

let (x@0 +> y@[1]) = cons x y

let patBinOp@(x@0 +> y@[1]) = cons 0 1

let @ = 0

let patAll = 0

let patBind x = x

let @isInt = 0

let patBindGuard@isInt = 0

let [] = []

let patEmptyList@[] = []

let [x, @isInt, y@isInt, 1] = [0, 0, 0, 1]

let patList@[x, @isInt, y@isInt, 1] = [0, 0, 0, 1]

let () = ()

let patEmptyTuple@() = x

let (x, @isInt, y@isInt, 1) = (0, 0, 0, 1)

let patTuple@(x, @isInt, y@isInt, 1) = (0, 0, 0, 1)
