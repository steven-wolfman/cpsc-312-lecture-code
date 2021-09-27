---
number: 4
title: Polymorphism and Higher-Order Functions
published: 2021-09-24
exercises: https://ca.prairielearn.com/pl/course_instance/2333/assessment/16925
---

> module Lecture4 where

Abstracting Behaviour and Data
==============================

Let's revisit `SparseList`, our data type for representing lists of `Double`s without
explicitly representing potentially long sequences of zeroes:


Review of `SparseList`
----------------------

There's nothing new in this section.

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

Here are functions to convert to/from sparse lists:

> -- | Produces a regular list from a sparse list.
> toList :: SparseList -> [Double]
> toList Empty = []
> toList (OneAndRest d rest) = d : toList rest
> toList (SkipAndRest n rest) | n <= 1 = 0 : toList rest
>                             | otherwise = 0 : toList (SkipAndRest (n-1) rest)


> -- | Produces a sparse list, but doesn't bother compressing
> -- sequences of zeroes down. Just uses a SkipAndRest 1 for them.
> fromList :: [Double] -> SparseList
> fromList [] = Empty
> fromList (d:ds) | d == 0 = SkipAndRest 1 (fromList ds)
>                 | otherwise = OneAndRest d (fromList ds)


A Couple of Concrete Functions
------------------------------

Now, let's define two functions: `slDouble` doubles each element of a sparse list, `slSquare` squares each element of a sparse list.
So, for example, doubling our sparse list that represents `[2.5, 1, 0, 3.2, 0, 0, 0, 1]` should result in
`[5.0, 2, 0, 6.4, 0, 0, 0, 2]`, whereas squaring it should result in `[6.25, 1, 0, 10.24, 0, 0, 0, 1]`. (Actually, because of the
way `Double` arithmetic works, the result is off by a little bit, as in the test below.)


> -- >>> slDouble Empty
> -- Empty
>
> -- >>> toList (slDouble slManyElt)
> -- [5.0,2.0,0.0,6.4,0.0,0.0,0.0,2.0]
> slDouble :: SparseList -> SparseList
> slDouble Empty = Empty 
> slDouble (OneAndRest d sl) = OneAndRest (2*d) (slDouble sl)
> slDouble (SkipAndRest n sl) = SkipAndRest n (slDouble sl)

> -- >>> slSquare Empty
> -- Empty
>
> -- >>> toList (slSquare slManyElt)
> -- [6.25,1.0,0.0,10.240000000000002,0.0,0.0,0.0,1.0]
> slSquare :: SparseList -> SparseList
> slSquare Empty = Empty
> slSquare (OneAndRest d sl) = OneAndRest (d^2) (slSquare sl)
> slSquare (SkipAndRest n sl) = SkipAndRest n (slSquare sl)



Abstracting Behaviour
---------------------

Those two functions are **very** similar. Let's abstract out the common behaviour into a function.
Basically, where the functions have common behavior, we'll keep it. Where they differ, we'll build
that difference into a function.

Think carefully as you go as well to see if we're missing anything!

> slMap :: (Double -> Double) -> SparseList -> SparseList
> slMap _ Empty = Empty 
> slMap f (OneAndRest d sl) = OneAndRest (f d) (slMap f sl)
> slMap f (SkipAndRest n sl) | f0 == 0 = SkipAndRest n (slMap f sl)
>                            | n <= 1 = OneAndRest f0 (slMap f sl)
>                            | otherwise = OneAndRest f0 (slMap f (SkipAndRest (n-1) sl))
>   where f0 = f 0



Now, let's redefine `slDouble` and `slSquare`:


> slDouble' :: SparseList -> SparseList
> slDouble' = slMap (* 2)
> --  where double n = 2*n

> slSquare' :: SparseList -> SparseList
> slSquare' = slMap (^2)



Catching All the Cases
----------------------

What about this function:

> -- | Adds 1 to each element of a SparseList.
> slAdd1 :: SparseList -> SparseList
> slAdd1 Empty = Empty
> slAdd1 (OneAndRest d sl) = OneAndRest (d+1) (slAdd1 sl)
> slAdd1 (SkipAndRest n sl) | n <= 1 = OneAndRest 1 (slAdd1 sl)
>                           | otherwise = OneAndRest 1 (slAdd1 (SkipAndRest (n-1) sl))
> -- slAdd1 (SkipAndRest n sl) = slReplicate (max 1 n) (0+1) ++ slAdd1 sl -- would work with slReplicate AND sl++

> slAdd1' :: SparseList -> SparseList
> slAdd1' = slMap (+1)

Will our `slMap` function handle it correctly? If not, what do we need to change?

Let's go change it together. Then, I'll have you define `slFilter` as an exercise.

(*Two exercises*!)


The Benefits of Abstraction
---------------------------

We can abstract out many common behaviours. Perhaps the most common are applying a function
to each element of the list (*mapping*) and selecting only the elements of the list that pass 
some test (*filtering*, like grabbing only the positive elements or all the integer-valued ones).

Doing so:

+ makes it easier for us to create new functions, 
+ focuses designers' attention on making a small set of powerful abstract functions correct, efficient, and useful,
+ refocuses our attention on the high-level task we want to solve rather than the details of its implementation, and
+ makes it easier to adjust for different priorities in the future (like parallelism) .

Limiting side effects also makes it less likely that implementation details will "leak through" our
abstractions.

Haskell supports a **lot** of abstraction!


Parametric Polymorphism
=======================

Our `SparseList`s really only work on numbers.[^monoids]

But for regular lists, `map` and `filter` are defined in the Haskell [Prelude](https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#v:map)
for any type of list at all.

+ `map :: (a -> b) -> [a] -> [b]`: takes a function and applies it to each element of a list, collecting the results into a new list.
  Its type is also `map :: (a -> b) -> ([a] -> [b])`: taking a function on elements and turning it into a function on lists!
+ `filter :: (a -> Bool) -> [a] -> [a]`: takes a test (a Boolean-valued function, also known as a predicate) and a list and
  returns only those elements of the list that pass the test.

Let's rewrite some of our assignment functions to use abstract functions instead!

(*Two exercises!*)


Polymorphic Types
-----------------

It's not just functions that we might want to abstract, however, but types as well. In fact, a list is already 
a polymorphic type. Its data constructors (`[]` or "empty" and `:` or "cons") can operate on any element type.

Let's define our own. Imagine you wanted to redesign Haskell so that it maintained the "provenance" of all the
values it computed: where they came from. You could create a data type that attached a textual description to each
value, maybe allowing for both "commented values" and "plain values" that don't yet have any provenance attached.

For `Double`s only, that might look like:

> -- | A "provenance value" for doubles, in two cases: 
> -- a commented value with a string comment describing provenance, and
> -- a plain value with no provenance.
> data ProValD = CVD Double String
>              | PVD Double


But we want to be able to represent *any Haskell type*. That means we don't need a *type* like `ProValD`,
we need something like a function that takes a type of value we want to represent and constructs a version
of it that carries provenance around: a *type constructor*:

> -- | A "provenance value", in two cases: 
> -- a commented value with a string comment describing provenance, and
> -- a plain value with no provenance.
> data ProVal a = CV a String
>               | PV a

This time, the first argument of each of our constructors can be any type!

The type `ProVal Double` behaves like `ProValD` above. When we define a polymorphic
type, we list out its arguments (which are just lowercase identifiers) and then use them on
the right to describe data constructors just like any type.

Here are some example values:

> plainInt :: ProVal Int
> plainInt = PV 7
>
> commentedDouble :: ProVal Double 
> commentedDouble = CV 3.1415926525 "From IHOP"
>
> proListOfString :: ProVal [String]
> proListOfString = CV ["Four", "score", "and", "seven"] "Being raised in the US"
>
> proProInt :: ProVal (ProVal Int)
> proProInt = CV (CV 42 "The answer") "What do you get when you multiply.."


Let's define some functions that operate on `ProVal`s. (Or, technically, they will
operate on actual types, like `ProVal a`, `ProVal b`, or `ProVal Char`.)

(*Three exercises!* Let's do at least the first together!)


More Practice with Making/Using Types
-------------------------------------

Finally, we have a few *more questions* in the exercises to give you more practice with types.



[^monoids]: Actually, we might imagine that they work well on anything with a particular "zero element". 
Haskell has several abstractions to help programmers work with "stuff with zero elements".
The [mempty](https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#v:mempty) and [mzero](https://hackage.haskell.org/package/base-4.15.0.0/docs/Control-Monad.html#v:mzero)
values in `Monoid` and `MonadPlus` are perhaps the most likely to match what we might want here!
