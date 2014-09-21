me Test.TestData1

def f1 x@isInt y@isInt = f2 x
  where {
    def f2 z@ = z + y
  }
