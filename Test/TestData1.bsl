me Test.TestData1

let f1 x@isInt y@isInt = f2 x
  where {
    let f2 z = z + y
  }
