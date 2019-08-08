# Patterns

A pattern is defined by an optional binder followed by an `@` sign and
an optional guard. A binder is a name to bind the value matched by the
pattern to. A guard describes the constraints of that value and it
cannot be a type.

## General form

```
  Pat: binder '@' guard
```

## Syntax

```
  Pat: binder '@' guard
  Pat: binder
  Pat: '@' guard
  Pat: guard  // Applies only to a subset of the guards.
  Pat: '@'
```

A binder and a `@` and a guard can be supplied.
A binder can be supplied without `@` and without guard.
An `@` and guard can be supplied.
A `@` can be supplied without binder and without guard.
Some guards can be supplied without `@` but not all.

## Binders

Bind `x` to a value that matches an integer

```
  x@isInt
```

Bind `x` to any value

```
  x
```

## Guards

The guard can be a type predicate expression (e.g., an identifier or a
function application), a literal, a list, a tuple, an operator guard,
or an algebraic datatype guard.

```
  Guard: type predicate expression
       | character
       | integer
       | real
       | string
       | List
       | Tuple
       | operator guard
       | algebraic datatype guard
```

### Type predicate expression

Match an integer and bind it to `x` in the first case and do not bind
it in the second case.

```
  x@isInt
   @isInt
```

Match the string prefixed by "ola" with and without a binder.

```
  x@(isPrefix "ola")
   @(isPrefix "ola")
```

### Literals

Match literals, such as, characters, numbers, and strings, with and
without binders, with and without `@`.

```
  x@'a'
   @'a'
    'a'

  x@1
   @1
    1

  x@2.0
   @2.0
    2.0

  x@"hello"
   @"hello"
    "hello"
```

### Lists

Match empty and non-empty lists with and without binders, with and
without `@`, with and without nested guards, etc.

```
  x@[i@isInt, r@isReal]
   @[i@isInt, r@isReal]
   @[i@isInt, r]
   @[@isInt, @isReal]
    [@, @]

  x@[]
   @[]
    []
```

### Tuples

Match empty tuples (i.e., unit) and 2-tuple or larger, with and
without binders, with and without `@`, with and without nested guards,
etc.

```
  t@()
   @()
    ()

  t@(x, y@isInt)
   @(x, y@isInt)
   @(x, y)
   @(@isInt, @isReal)
    (@, @)

  t@(x, y, z)
   @(@isInt, @isReal, @isString)
    (@isInt, @, @)
```

### Operators

Match using the `+>` (i.e., `cons`) operator, with and without
binders, with and without `@`, with and without nested guards, etc.

```
  l@(x +> xs)
  l@(x@isInt +> xs@isInt)
    (x +> xs)
    (@isInt +> @isInt)
```

### Algebraic datatypes

Match on an algebraic datatype constructor `Apple` with one argument.

```
  f@(Apple x)
    (Apple x)
```
