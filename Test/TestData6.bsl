me Test.TestData6

def f n@ =
  let [x@, y@] [1, 2] in
  case n (
    ((>) 1) = x
  | @       = y)
