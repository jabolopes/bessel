me Test.TestData2

let f1 x@isInt =
  let y 0 in f2 y
  where {
    let f2 z = z + 1
  }
