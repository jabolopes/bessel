me Test.TestData2

def f1 x@isInt = let y 0 in f2 y
  where {
    def f2 z@ = z + 1
  }
