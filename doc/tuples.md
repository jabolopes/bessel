# Tuples

Tuples are predefined up to 10-tuple.

## Definitions

let () : () = mkTuple0
let (,) : forall a b. (a, b) = mkTuple2
let (,,) : forall a b c. (a, b, c) = mkTuple3
...

let (_,) = tuple2Ref0
let (,_) = tuple2Ref1
let (_,,) = tuple3Ref0
let (,_,) = tuple3Ref1
let (,,_) = tuple3Ref2
...

## Expansions

let sum (x@isInt, y@isReal) = x + y

let sum
  \x#0 ->
    cond
      isTuple2 (mkTuple2 isInt isReal) x#0 ->
        let
          x = tuple2Ref0 x#0
          y = tuple2Ref1 x#0
        in
          x + y
      _ -> blame ...
