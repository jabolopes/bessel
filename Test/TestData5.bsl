me Test.TestData5

let isString
  []                     = true
| (@isChar +> @isString) = true
| @                      = false
