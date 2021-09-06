---
number: 1
title: Welcome to CPSC 312!
published: 2021-09-08
exercises: https://ca.prairielearn.com/pl/course_instance/2333/assessment/16845
---

> import System.Random
> import Data.List

A quick note on masks: 
Masks are mandated in class. I'm immune suppressed.
For me and others at-risk or close to those who are at-risk,
please wear your mask, including over your nose.

However, there are medical circumstances that merit exemptions.
To seek such accommodation, please contact the Centre for
Accessibility at info.accessibility@ubc.ca.

Now, let's really start with a 
[song](https://www.cs.ubc.ca/~wolf/songs/like_it_called_on_me_quicksort.html).

Showing Off in Haskell
======================

Haskell is a 
_pure_, 
_lazy_, 
_statically-typed_ 
_functional_ programming language 
with powerful _type inference_.
You'll learn about a lot of that in the reading. Today, we're just
going to _show off_ with a bit of what that means.

For now, we need to know that: 
**a Haskell program is just an expression to be evaluated**, 
not a series of statements to execute.
Programs build up complex expressions to be evaluated by giving
names to and using smaller expressions (functions and variables).

Quicksort in English
--------------------

[Quicksort](https://en.wikipedia.org/wiki/Quicksort)
is a beautiful sorting algorithm. Here's a simple version.
To quick sort a list:

1. If the list is empty, simply return it.
2. Otherwise, let `p` be the first element of the list.
   Construct two sublists: everything else that is `< p` and
   everything else that is `>= p`. Quick sort each sublist
   recursively. Then return the quick-sorted `< p` list,
   followed by a list containing just `p`, followed by
   the quick-sorted `>= p` list.

(Let's draw a picture.)

Quicksort in Haskell
--------------------

First, let's write Quicksort in Haskell. The first line is
the signature. The second is the function definition... which
is undefined so far. **We'll finish it!**

> qsort :: Ord a => [a] -> [a]
> qsort _ = undefined

What did we see? Haskell:

+ Uses (beautiful?) pattern-matching to make decisions.
+ Has polymorphic types. In this case, _ad hoc polymorphism_: `qsort`
  sorts lists of any type that is an instance of `Ord` (like a Java interface).
+ Supports list comprehensions[^comprehensions] that can be quite elegantly mathematical.

Test Data for Quicksort
-----------------------

**Let's test `qsort` a bit: on `[]`, on `[2, 4, 6, 0, 1]`, and on `"mindblowing"`.**
**We'll use `ghci` for its REPL (read-eval-print-loop) to try these out.**

Next, let's try `qsort` on something bigger.
We'll need some input to call it on:

> -- | @genList cap seed@ returns an unending list of (pseudo-)random
> -- @Int@s in the range @[0, cap)@, using @seed@ for the random generator.
> -- Don't worry about this code for now!
> genList :: Int -> Int -> [Int]
> genList cap seed = map (`mod` cap) (randoms (mkStdGen seed))

We could try out `qsort` with:

> infResults :: [Int]
> infResults = qsort (genList 10000 0)

**But wait. How many elements are we sorting?!**
**Try `infResults` at the `ghci` REPL.**
(Wait... why didn't that run forever _before_ we typed `infResults`?)

Finite Test Data
----------------

Here's code for a random list of `n` numbers in the range `[0, n)`,
plus a sample `bigList` to work with.

> randList :: Int -> [Int]
> randList n = take n (genList n 0)
> 
> bigList :: [Int]
> bigList = randList 1000000

The `take` function uses _parametric polymorphism_.
`take n lst` produces the first `n` elements of the list `lst`,
without caring what type of value is in `lst`. Its type is 
`Int -> [a] -> [a]`: given an `Int` and a list of any
type of value `a`, return a list of (that same type of) `a` values.

You can run `bigList` at the REPL, but it prints for a _long_ time.
Instead, get just the last element of `bigList`:

```haskell
:set +s       -- turn on performance information
last bigList
```

**How long did that take? Try `last bigList` again. How long did it take the second time?**

Haskell uses _lazy evaluation_. Loosely: it evaluates expressions
only when forced to, e.g., by you the user. (Once it evaluates a 
variable's expression, it caches it to avoid evaluating it again.)[^caching]

Laziness Goes Deep
------------------

Let sort `bigList` but just look at the last result.
**Run `last (qsort bigList)`.**

**How long did that take?**

**How about `take 100 (qsort bigList)`?** (That's the first 100
elements of the sorted list.) How long did that take? Why?!?

(Was it caching? Let's guess that it _was_. How could you falsify that guess?)

Showing off in Prolog
=====================

As strange as Haskell is, Prolog is stranger. That makes it
a little easier to show off.

Above, we relied on Haskell's `++` operator to append two lists.
It's [defined](https://hackage.haskell.org/package/base-4.15.0.0/docs/src/GHC-Base.html#%2B%2B) as:

```haskell
(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : xs ++ ys
```

Let's redefine this in Prolog. We'll call it `append`.

Functions That Are (Not) Functions
----------------------------------

Haskell's "functions" aren't like Java "functions" because Java
"functions" are **not** functions. You can call a Java function
twice with the same arguments and (because it sets variables,
reads input from the user, accesses a datasource via REST, etc.), 
it can return _two different things_ or _move a robot_ or something
whacky like that.

That is _not_ how mathematical functions work. So, Haskell does many
clever things to use _real mathematical functions_ while still being 
able to read input from the user and use REST APIs and such.

Prolog _really_ doesn't do normal "functions".

`append` doesn't _return_ the result of appending two lists.
Instead, it is a (logical) predicate of three values that 
**describes the circumstances under which**
**the third value is the result of appending the first two**.

The Haskell code above kind of tells us our cases.
`append(Xs, Ys, Zs)` is true when:

1. `Xs` is just the empty list and `Ys = Zs`.

2. `Xs` and `Zs` start with the same element (one element down!),
   and `append` is true for the rest of `Xs`, `Ys`, and the rest of `Zs`.


Append in Prolog
----------------

**Let's write that together in Prolog.**

Notes: `append(X, Y, Z) :- stuff(over, here)` means
"if `stuff(over, here)` is true, then `append(X, Y, Z)` is true".
Also, anything that starts with an uppercase letter is a variable;
anything else is just an atom (like a symbol or a bit like a string).

```prolog
% Fill in the blanks on append/3 so that append(Xs, Ys, Zs)
% is true if appending together Xs and Ys results in Zs.
append(    ,    ,    ) :-           .
append(    ,    ,    ) :-           .
```

What a Predicate Can Do, Forwards and Backwards
-----------------------------------------------

**Let's try that in the `swipl` (SWI-Prolog) REPL.**

It's downright strange what we can do with `append` now that
we've defined it. Remember that `append(Xs, Ys, Zs)` just
describes the circumstances under which it is true that `Zs`
is the result of appending `Xs` and `Ys`.

**So, start by trying `append([1, 2], [3, 4, 5], Zs).`**
**Then, start asking for stranger things.**

We'll try to show off two interesting features of Prolog:

- Prolog determines variables' values using "unification".
  (In fact, it can use unification to check if any two 
  expressions can be made equal to each other and keep
  track of the constraints needed to make that so.)
- A Prolog "program" is a description of the world plus a
  query. Prolog "runs the program" by searching for conditions
  that can make that query true and giving them to you.
  (You can even ask Prolog to keep searching after
  it finds the first set of conditions that work.) 

Class Notes for Today
=====================

1. Let's spend a few minutes with the [Syllabus](/syllabus.html).
   (Especially important: **do not come to class if you are sick!!**)
2. TODO: "In-class Exercises 1" is on PrairieLearn and due Thu Sep 16.
   Grading is exremely lenient (see the "max points" field on each question).
   Use this to learn in class and prepare for the quiz!
3. Quiz 1 is coming up on Friday September 17 (during class time).
4. Assignment 1 is due Sep 23 on PrairieLearn. Submit by Sep 16 for a chance at some extra credit.
5. To reduce Covid transmission risk, please sit in the same area each lecture when possible. 
6. Before next class:
   + access PrairieLearn and Piazza from the [Resources](/resources.html) page
   + get Haskell set up so you can try things out; the [Resources](/resources.html) page has help
   + work through [CIS 194 Lecture 1](https://www.cis.upenn.edu/~cis194/spring13/lectures/01-intro.html) up to the "GHCi" section

Appendix
========

Monday Morning Haskell recently released a [In-Place QuickSort in Haskell](https://mmhaskell.com/blog/2021/8/20/quicksort-video) video. If you haven't already studied Haskell, the latter 2/3 of the video is challenging. If you want to give it a try, you can think of a "monad" _for this context_ as a special structure for assembling computations-that-produce-values-when-run that ensures you don't accidentally mistake those computations with the values they produce.

Here are some longer/different versions of the code above.

```haskell
-- | @qsort xs@ should produce a sorted version of xs.
--
-- Haskell's doctest can turn these @>>>@ examples into
-- runnable tests. It actually also has a powerful testing
-- system where you assert logical expressions as properties.
--
-- >>> qsort []
-- []
--
-- >>> qsort [2, 4, 6, 0, 1]
-- [0,1,2,4,6]
--
-- >>> qsort "mindblowing"
-- "bdgiilmnnow"
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (p:ps) =
   qsort [s | s <- ps, s < p]
   ++ [p]
   ++ qsort [b | b <- ps, b >= p]

-- | @genList cap seed@ takes a seed value and returns an unending
-- list of (pseudo-)random Int values in the range [0, cap).
--
-- Here's a more idiomatic version that "chains" together existing
-- functions using the function composition operator @.@.
genList :: Int -> Int -> [Int]
genList cap = map (`mod` cap) . randoms . mkStdGen
```
[^pivot]:
    More complex algorithms choose more cleverly than just
    "the first element" to avoid worst-case behaviour in common cases.

[^comprehensions]:
    List comprehensions actually demonstrate
    some of Haskell's fascinating type classes that support common
    and flexible ways to build large computations out of smaller ones.

[^caching]:
    `ghc` did **not** cache `last bigList` because we didn't
    attach that to a name. It **did** cache the `bigList` because in
    demanding the value of `last bigList`, we also demanded that it
    evaluate `bigList` all the way to its last element.)

