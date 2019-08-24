me Test.Pattern

let patChar 'a' = 'a'

let patInt 0 = 0

let patReal 0.0 = 0.0

let patString "hello" = "hello"

let patVariantTag (MyCons x@isInt) = x

let patBinOp (x@0 +> y@[1]) = cons 0 1

let patAll @ = 0

let patBind x = x

let patGuard @isInt = 0

let patBindGuard x@isInt = x

let patEmptyList1 [] = []

let patEmptyList2 x@[] = x

let patList1 [x, @isInt, y@isInt, 1] = [0, 0, 0, 1]

let patList2 [x, @isInt, y@isInt, 1] = [0, 0, 0, 1]

let patEmptyTuple1 () = ()

let patEmptyTuple2 x@() = x

let patTuple1 (x, @isInt, y@isInt, 1) = (0, 0, 0, 1)

let patTuple2 (x, @isInt, y@isInt, 1) = (0, 0, 0, 1)
