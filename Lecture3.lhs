---
number: 3
title: Algebraic Data Types
published: 2021-09-20
exercises: https://ca.prairielearn.com/pl/course_instance/2333/assessment/16925
---

> module Lecture3 where

(Note: no strange imports this time. Plain old `ghci` should load this file just fine.)

Representing Data
=================

What is a type? One way to think of it is as a set of values, structured in a way 
that helps you represent your data.

Haskell helps in two ways:

1. guaranteeing what type of values you will work with at any program point. You know what type
   of data you are operating on; so, you know what you can do with that data.

2. providing ways to assemble your own types to represent data of interest to you.


Enumerations
------------

For example, your data may simply indicate one of a handful of cases:

+ Is this statement true or false?
+ Is `x > y`, or is `x = y`, or is `x < y`?
+ Is your Tim's drink extra small, small, medium, large, or extra large?

Let's start with the last one. To represent data like this in our programs, we might choose:

> data TimsSize = ExtraSmall
>               | Small
>               | Medium
>               | Large
>               | ExtraLarge
>   deriving (Eq, Show)

Note: the `deriving` statement at the end automatically makes it so we can compare
`TimsSize`s for equality and convert them to `String`s with `show`.

Now, we have a new type `TimsSize`. There are exactly five values in that type:

+ `ExtraSmall`, 
+ `Small`,
+ `Medium`, 
+ `Large`, and
+ `ExtraLarge`.

These values are called _data constructors_ because they construct  a value (data) of our new type.

The true/false and greater/equal/less scenarios are not so different.
Here are [GHC's definitions for `Bool` and `Ordering`](https://github.com/ghc/ghc/blob/master/libraries/base/GHC/Base.hs#L191):

```haskell
data Bool = True 
          | False 

data Ordering = LT
              | EQ 
              | GT
```

(Remember that `qsort`'s type was `Ord a => [a] -> [a]`? To be an instance of `Ord`, a type `a` needs to define 
`compare :: a -> a -> Ordering`, which in turn enables comparisons like `<`, `>=`, and so on.)

We can use data constructors for constructing values and for pattern-matching. Here's two such functions;
let's finish the second together:

> -- | Evaluates to the size in ounces of the given @TimsSize@
> sizeOz :: TimsSize -> Int 
> sizeOz ExtraSmall = 8
> sizeOz Small = 10
> sizeOz Medium = 14
> sizeOz Large = 20
> sizeOz ExtraLarge = 24
>
> -- | Upsize the given size to be one larger (or the same for the largest size).
> upsize :: TimsSize -> TimsSize
> upsize ExtraSmall = Small
> upsize Small = Medium
> upsize Medium = Large
> upsize _ = ExtraLarge

This is a digression about lists and how they're essentially data types
(that cheat on syntax) and how `head` is dangerous and a LIEEEEEEE.

```haskell
head :: [a] -> a
head (x:_) = x

data [a] = []
         | a : [a]
```

This is also a digression:

```haskell
(||) :: Bool -> Bool -> Bool
True || _ = True
_ || True = True
_ || _ = False
```

Back to regularly scheduled programming.

Your turn to work on creating a new data type and a function to operate on it.

(*Next two exercises (on species)!*)


Assembling Data from Other Data
-------------------------------

Your data may also be made up of other data. For example,
maybe you want to represent a person's vaccination declaration status:

> -- | Either declared whether they are vaccinated (True or False)
> -- or not yet declared.
> data VaccinationDeclaration = Declared Bool 
>                             | Undeclared
>   deriving (Eq, Show)

This creates two data constructors. One (`Undeclared`) is also a value of type
`VaccinationDeclaration` as above. The other `Declared` is also a function. You give
it a `Bool`, and it constructs a `VaccinationDeclaration` value.

We can use both data constructors in pattern matching. Let's write a function to determine if 
a person needs to undergo regular testing given their vaccination status:

> needsTesting :: VaccinationDeclaration -> Bool 
> needsTesting Undeclared = True
> needsTesting (Declared False) = True
> needsTesting _ = False



Or perhaps you want to store your ukulele's four strings' current tuning frequencies:

> -- | Tuning in Hz (rounded) for the G, C, E, and A strings.
> data UkeTune = UkeTune Int Int Int Int
>   deriving (Eq, Show)

The type and the single data constructor share a name here. That's allowed and 
even common, if sometimes confusing!


Here's a handy `UkeTune` value:

> perfectTune :: UkeTune
> perfectTune = UkeTune 392 262 330 440

TODO: start next time!

We can (of course?) assemble larger types out of our smaller types. The following
isn't particularly meaningful, but it shows how we can use everything we've learned
so far as we create new types, including our own types, lists, tuples, and 
even functions. The first case just has a `UkeTune` value. The second has a list of
`VaccinationDeclaration`es. The third has a tuple of two `UkeTune`s and a function from
`VaccinationDeclaration` to `UkeTune`. (Which.. seems like an interesting function.)

> data UkunationStatus = Uke UkeTune
>                      | Vaxes [VaccinationDeclaration]
>                      | ThisIsJustSilly (UkeTune, UkeTune) (VaccinationDeclaration -> UkeTune) 


Let's do some exercises defining and using this sort of algebraic data type in Haskell!

(*Next two exercises (on water supply)!*)


Recursive Data Types
--------------------

Where things get really exciting is when we define recursive data types. Imagine
we want to represent a list of `Double`s, but we expect most entries to be zeroes,
and we don't want to waste a bunch of space representing those. We might define a
_sparse_ list type.

A sparse list will represent a list of `Double`s. It will have three cases:

+ `Empty`: is much like `[]` and represents an empty list
+ `OneAndRest`: is much like `(:)` and represents a non-empty list with one
  `Double` at its head and a tail that is a sparse list
+ `SkipAndRest`: is new and represents a sequence of one or more zero elements at
  the start of a list followed by a tail that is a sparse list

Here is our data type:

> -- | A well-defined SparseList has a positive number of zeroes
> -- in SkipAndRest cases. If a zero or negative value appears,
> -- treat it as as a 1.
> data SparseList = Empty
>                 | OneAndRest Double SparseList
>                 | SkipAndRest Int SparseList
>   deriving (Eq, Show)



Here's are some lists:

> emptyL, twoEltL, manyEltL :: [Double]
> emptyL = []
> twoEltL = [0, 0]
> manyEltL = [2.5, 1, 0, 3.2, 0, 0, 0, 1]

Here they are as sparse lists:

> slEmpty, slTwoElt, slManyElt :: SparseList
> slEmpty = Empty
> slTwoElt = SkipAndRest 2 Empty
> slManyElt = OneAndRest 2.5 (OneAndRest 1 (SkipAndRest 1 (OneAndRest 3.2 (SkipAndRest 3 (OneAndRest 1 Empty)))))



Functions on Recursive Data Types
---------------------------------

Let's practice defining some recursive functions on sparse lists:

> -- | Produces a regular list from a sparse list.
> toList :: SparseList -> [Double]
> toList = undefined


> -- | Produces a sparse list, but doesn't bother compressing
> -- sequences of zeroes down. Just uses a SkipAndRest 1 for them.
> fromList :: [Double] -> SparseList
> fromList = undefined


It would be great to define a `toList` that produced a nice, compact `SparseList`.
It should never have a `OneAndRest 0 ...`, and it should never have two consecutive
`SparseList x (SparseList y ...)`, since those could just be replaced by
`SparseList (x+y) ...`.

*Two exercises:* Define the helper functions below. Think in cases! `slZeroToSkip`
is easier than `slCompact`. You may assume that all `SkipAndRest` counts are positive.

> slNormalize :: SparseList -> SparseList
> slNormalize sl = slCompact (slZeroToSkip sl)

> slZeroToSkip :: SparseList -> SparseList
> slZeroToSkip = undefined

> -- Hint: look at that nested SparseList above with the x and then y.
> -- You CAN use something nested like that as a pattern!
> -- But.. what happens if you give it three SparseLists in a row?
> -- Can some creative recursion help?
> slCompact :: SparseList -> SparseList
> slCompact = undefined


There is one last *exercise* for you to practice defining a recursive data type.




P.S. You will want to know about [`case` expressions](https://en.wikibooks.org/wiki/Haskell/Control_structures#case_expressions) 
and [`where` clauses](https://en.wikibooks.org/wiki/Haskell/Variables_and_functions#where_clauses)
at some point. Now's a good time!
