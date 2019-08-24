me Test.TestData5

let isString2
  []                      = true
  (@isChar +> @isString2) = true
  @                       = false
