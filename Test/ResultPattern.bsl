me Test.ResultPattern

let 'a' = 'a'

let patChar@'a' = 'a'

let 0 = 0

let patInt@0 = 0

let 0.0 = 0.0

let patReal@0.0 = 0.0

let "hello" = "hello"

let patString@"hello" = "hello"

let (MyCons x@isInt) = mkMyCons 0

let patVariantTag@(MyCons x1@isInt) = mkMyCons 0

let (x2@0 +> y2@[1]) = cons 0 [1]

let patBinOp@(x3@0 +> y3@[1]) = cons 0 [1]

let @ = 0

let patAll = 0

let patBind x4 = x4

let @isInt = 0

let patBindGuard@isInt = 0

let [] = []

let patEmptyList@[] = []

let [x5, @isInt, y5@isInt, 1] = [0, 0, 0, 1]

let patList@[x6, @isInt, y6@isInt, 1] = [0, 0, 0, 1]

let () = ()

let patEmptyTuple@() = x

let (x7, @isInt, y7@isInt, 1) = (0, 0, 0, 1)

let patTuple@(x8, @isInt, y8@isInt, 1) = (0, 0, 0, 1)
