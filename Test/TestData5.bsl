me Test.TestData5

def isString
  []                   = true
| (isChar +> isString) = true
| @                    = false
