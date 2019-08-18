me Test.Lambda

let f1 y =
  (f x = f x y) (+) 1

let f2 x = g
where
  let g y = x + y
  where
    let h z = x + y + g z

let f3 x = gg
where
  let gg y = hh x + y
  where
    let hh z = x + y + hh z

let f4 = ggg hhh
where
  let hhh x = x + x
  let ggg f = f 10 + hhh 10
