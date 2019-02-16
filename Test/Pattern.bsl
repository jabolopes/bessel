me Test.Pattern

let patChar 'a' = 'a'

let patInt 0 = 0

let patReal 0.0 = 0.0

let patString "hello" = 0

let patVariantTag (Apple x@isInt) = x

let patBinOp (x@0 +> y@[1]) = cons x y

let patAll @ = 0

let patBind x = x

let patGuard @isInt = 0

let patBindGuard x@isInt = x

let patEmptyList1 [] = []

let patEmptyList2 x@[] = x

let patList1 [x, @isInt, y@isInt, 1] = 0

let patList2 [x, @isInt, y@isInt, 1] = 0

let patEmptyTuple1 () = ()

let patEmptyTuple2 x@() = x

let patTuple1 (x, @isInt, y@isInt, 1) = 0

let patTuple2 (x, @isInt, y@isInt, 1) = 0
