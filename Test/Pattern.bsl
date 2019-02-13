me Test.Pattern

let patChar 'a' = 'a'

let patInt 0 = 0

let patReal 0.0 = 0.0

let patString "hello" = 0

let patVariantTag (Apple x@isInt) = x
