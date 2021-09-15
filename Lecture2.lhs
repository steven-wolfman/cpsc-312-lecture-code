---
number: 2
title: Intro to Haskell
published: 2021-09-10
exercises: https://ca.prairielearn.com/pl/course_instance/2333/assessment/16845
---

(Note: no strange imports this time. Plain old `ghci` should load this file just fine.)

Playing With Some Building Blocks
=================================

As with most languages, Haskell lets us give names to values:

> x = 42
> y = 'A'
> oppositeOfTrue = False

Let's play with these in `ghci`:

```haskell
y
:type y
```

Check the other variables' types and values as well.

(*Exercise!*)


Let's explore one more type:

> s = "Hello World!"
> doubleS = s ++ s

Strings in Haskell are lists of characters (written `[Char]`).[^strings]


Definition or Assignment?
-------------------------

Remember that a Haskell program is a big expression,
*not* a series of instructions (statements) to execute.

In Java `x = 5` is assignment, meaning "put the new
value `5` into `x`".

In Haskell, `x = 5` is definition: define `x` to be `5`.

So, what if we give a new value to `x`?

Try uncommenting this (remove the `{-` and `-}`) and 
loading (or reloading: `:reload`) the file:[^ghci]

> {- x = 525600 -}

Why did this happen? (*Exercise!*)



Values or Expressions
---------------------

(We will probably skip this part in lecture. If so, and if you have trouble
finding expressions that cause errors, consider trying the built-in functions
`div` or `head`.)

Now, in Haskell, do we give names to expressions or to values? Let's try these:

> n = 10 + 2
> m = 25 `div` 3  -- equivalent to div 25 3
> o = 25 / 3

Does it matter? What's the difference?

Take a few minutes to define some expressions, show their values, and show their types.
Try specifically to **define an expression that causes no error but 
then when we evaluate we get an error**.

(*Exercise!*)



Type Inference
--------------

Haskell figures out the types of your expressions. (It performs
_type inference_.) We'll explore the details of that later, with 
connections to "unification".

For now, think of it as detective work: Investigate each element of an
expression for its type "constraints": what do you know for sure about its type?
Puzzle together those constraints until you know the overall type.

(There's a challenging, *very* quiz-like exercise on this, 
which is for out-of-class. Try it then, and ask questions on Piazza!)




Building Up Functions
=====================

We've used a handful of built-in arithmetic functions, and you'll see 
more in the reading.

Let's define a few of our own simple functions.



Define a function that adds one to an `Int`
-------------------------------------------

> add1 :: Int -> Int
> add1 n = n + 1 

There's actually a built-in function that does the same. We could also
just put *its* value into our function:

> add1' :: Int -> Int
> add1' = succ


Finding the `n`th Odd Number
----------------------------

**Monday 13 Sep 2021: Stopped here in class. Left draft of `nthOdd` as an exercise.**

Now, define a function that produces the `n`th odd number:

> -- >>> nthOdd 1
> -- 1
> --
> -- >>> nthOdd 2
> -- 3
> nthOdd :: Int -> Int
> nthOdd n = 2*n - 1

(*Exercise!*)

(Hint: if you double a number `n`, it gives you the `n`th even number.)



Define an exactly-one-true function
-----------------------------------

Define a function that determines if **exactly one**
of three Boolean values is true. You'll want to use 
the `&&` (and), `||` (or), and `not` functions.

> oneTrue :: Bool -> Bool -> Bool -> Bool
> oneTrue b1 b2 b3 = (b1 && not b2 && not b3) ||
>                    (not b1 && b2 && not b3) ||
>                    (not b1 && not b2 && b3)

Building Farther Using Cases
----------------------------

One of the central mechanisms Haskell uses to make decisions
and break down data is _pattern-matching by cases_.[^cases]

In fact, built-in functions like `head` and `tail` that break up data structures 
are implemented in terms of pattern-matching:

> myHead :: [a] -> a
> myHead (x:_) = x

> myTail :: [a] -> [a]
> myTail (_:xs) = xs

`(_:_)` matches a non-empty list. `(x:_)` does the same, but defines
`x`'s value to be the head of the list.

([Guards](https://en.wikibooks.org/wiki/Haskell/Control_structures#if_and_guards_revisited)
are handy also. Learn about those from the readings!)


We can use any type of data in our cases, like bools:

> myNot True = False
> myNot False = True


Now, redefine `oneTrue` (as `oneTrue'`) except by cases instead:

> oneTrue' :: Bool -> Bool -> Bool -> Bool
> oneTrue' True False False = True
> oneTrue' False True False = True
> oneTrue' False False True = True
> oneTrue' _ _ _ = False


> secondElt (_:x:_) = x

Exercise
--------

Now, we'll try an exercise where we interpret lists of `Bool`s
as if they were single `Bool` values. Go try it out as an *exercise!*

It may help to know that you can use patterns like:

+ `[]`: the empty list
+ `(x:xs)`: a _non-empty_ list with the head `x` and tail `xs`,
+ `[True, False]`: a length-two list with `True` as its first value, and `False`
  as its second.




Lazy Evaluation, Referential Transparency, and Control Structures
=================================================================

We've mentioned before that Haskell uses "lazy evaluation", meaning loosely that it avoids 
evaluating expressions until forced to.

Let's use that to define **our own `if` expression**:

> myIf :: Bool -> a -> a -> a
> myIf True thenArg _ = thenArg
> myIf False _ elseArg = elseArg


Would this work in Java?

```java
public static int myIf(bool condition, int thenArg, int elseArg) {
   if (condition)
      return thenArg;
   else
      return elseArg;
}

myIf(a != 0, b / a, 0);
```

Let's try it in Haskell!

> a = 0
> b = 3
> result = myIf (a /= 0) (b `div` a) 0

Now type `result` in `ghci`.



Referential Transparency and No Side Effects
--------------------------------------------

But wait. If we don't even know when an expression will be evaluated...
if expressions can be evaluated "out of order" with the way we expect
them to go... then what happens with code like `x++`?[^parallelism]

`x++` could change `x`'s value at some unpredictable time in 
a Haskell program. Or it could *never* change `x`'s value, if it happened 
never to get evaluated. How can we possibly increment a variable's value
given all that?

Haskell's answer: We can't. Haskell *disallows side-effects*: effects your code has
besides computing a value, like changing the value of a variable.[^no-side-effects]

That means Haskell also offers something called **referential transparency**:
once you know an expression's value[^context], you know
that the value and expression mean the same thing. So:

+ you can freely substitute the value for the expression 
+ you can evaluate the same expression again, and you *will* get the same result
+ you can substitute in a _different_ expression if it also has the same value

That's tremendously handy for reasoning about your programs (like when
you're testing, for example!).



Recursive Functions
===================

We'll use recursion frequently in defining our functions (at least at first!).

Fortunately, most recursive functions we create do just what Haskell is good at:
break down processing of data into cases based on the structure of the data, and 
then define the result of each case based on the data from those structures.

So, let's write a couple of our own recursive functions. First, we'll double 
each element in a list. Let's break it into cases, and then figure out the cases:

> -- If we doubleAll an empty list, that's still just an empty list. 
> -- If we doubleAll on a list with x at the head and xs at the tail,
> -- we should get 2*x as the head and 
> -- the result of doubleAll on xs as the tail.
> doubleAll :: [Int] -> [Int]
> doubleAll [] = []
> doubleAll (x:xs) = 2*x : doubleAll xs


Now, let's try to intersperse a new letter between each pair of letters in a string.
For example, `intersperse c [letter1, letter2, letter3]` is 
`[letter1, c, letter2, c, letter3]`.

> -- >>> intersperse 'o' "www"
> -- "wowow"
>
> -- >>> intersperse 'x' ""
> -- ""
>
> -- >>> intersperse 'y' "p"
> -- "p"
>
> -- >>> intersperse 'y' "ab"
> -- "ayb"
>
> -- >>> intersperse 'z' "ab"
> -- "azb"
>
> intersperse :: Char -> [Char] -> [Char]
> intersperse _ [] = []
> intersperse _ [c] = [c]
> intersperse i (c1:c2:cs) = c1:i:intersperse i (c2:cs)



Over in the *exercises!* we have a mystery recursive function for you to evaluate
and a recursive function for you to define.






[^strings]: There's actually a more robust text type available and GHC support for "polymorphic" strings, 
in much the way that the number `5` came out with the type `Num p => p`, meaning "some type `p`,
where `p` is an instance of the `Num` type class, i.e., is numeric".

[^ghci]: You'll get a different result if you run `x = 525600` at the REPL prompt in `ghci`, which exposes
some interesting techniques used to make the REPL work!

[^cases]: In fact, `ghc` compiles Haskell to a Haskell-lite intermediate language called Core, 
which lacks `if` expressions and even cases in function definitions and boils them all down 
to an explicit `case` expression construct that uses pattern-matching by cases.

[^no-side-effects]: In fact, with a very small number of "functions"
you are strongly encouraged never to use, it is possible to cause side
effects. Doing so is a mess in a Haskell program for the reasons we talked about!
There are also some side-effects you can't disallow in the real world. 
For example, how long a piece of code takes to run on your computer. On the other hand,
Haskell has *brilliant* solutions for side effects like reading input and displaying output
that do *not* violate the no-side-effects rule within your program!

[^parallelism]: "We don't even know when an expression will be evaluated. Expressions might be
evaluated out of order." Does that sound a bit like the hazards of parallelism and concurrency
to you? I wonder if strictly-functional programming is **hugely** valuable in modern programming
because of the growing importance of parallelism and concurrency.

[^context]: Within a particular context, that is. For example `x + 1` in your program
may be very different from in mine, and `x + 1` in one call to a function with `x` as
a parameter may be very different from in a different call to that function.