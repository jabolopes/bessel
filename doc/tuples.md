# Tuples

Tuples are predefined up to 10-tuple.

## Definitions

def () : () = unit#
def (,) : forall a b. (a, b) = (,)#
def (,,) : forall a b c. (a, b, c) = (,,)#
...

def (_,) = fst#
def (,_) = snd#
def (_,,) = tuple_3_get_1
def (,_,) = tuple_3_get_2
...

## Expansions

let sum (x@isInt, y@isReal) = x + y

let sum
  \x#0 ->
    cond
      isTuple2 (isInt, isReal) x#0 ->
        let
          x = tuple2Ref0 x#0
          y = tuple2Ref1 x#0
        in
          x + y
      _ -> blame ...
