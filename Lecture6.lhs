---
number: 6
title: I/O, Yay!!
published: 2021-10-08
exercises: https://ca.prairielearn.com/pl/course_instance/2333/assessment/16998
---

> module LectureIO where


Um, yeah. OK. Maybe I/O is not *that* scary in most languages. To be honest, though, it's pretty messy in most languages. How lovely really is `java.util.Scanner`, `std::cin`, `scanf`, etc.?

What are Monads?
================

A monad in Haskell is an instance `m` of the `Monad` type class. The type class says that `m` must be a type constructor of one argument  (like `ProVal` or list) with two operations:

`return :: a -> m a`
: This "injects" a value into the monad in some trivial way. If we think of the monad as a "container" (like a `Proval Double` is a container for a `Double` along with some provenance commentary or a `[Double]` is a container for many doubles arranged in an order), then this constructs a container capapble of holding `a`s, with nothing inside but its argument.
: 
: So, for `ProVal`, `return x` might make a new `ProVal` with `x` inside and no commentary. For lists, `return x` makes the list `[x]`.

`(>>=) :: m a -> (a -> m b) -> m b`
: Also known as "bind", this lets us build a new container (an `m b`) by taking an original container (`m a`) and a function that tells us what `m b` to "splice in" as a replacement for each `a` in the container. It makes a new container representing the result of that splicing.[^monadExtras]


You Can Check Out Anytime You Like, But You Can Never Leave[^hotelcalifornia]
-----------------------------------------------------------

If you take a close look at the signatures of `return :: a -> m a` and `(>>=) :: m a -> (a -> m b) -> m b`, you'll notice:

1. It's easy to "get a value out" of `m` and do something with it. You can use `ma >>= f`. Now, `f` can access the (or every) `a` inside the `ma` "container" and act on it.

   We can even define a new function using `return` and `>>=`:

   ```haskell
   fmap :: Monad m => (a -> b) -> (m a -> m b)
   fmap f ma = ma >>= (\a -> return (f a))
   ```

   `fmap f` lets us use *any* normal function `f` that we want as a function that operates on values in any monad instead!

   (So, we can "check out" of the `m` monad anytime we want to and get at the regular values inside.)

2. *However*, the result types of `return` and `>>=` make it so that *in the end*, anytime we operate on a value in an `m` container, the *result* ends up in an `m` container as well!

   (So, we can never really *leave* the monad, at least note with the `Monad` type class's operations.)

Haskell defines a *huge* number of operations and functions (and even special syntax[^doNotation]) we can now use to manipulate anything that instantiates `Monad`, and we can take advantage of all of them if we make a `Monad` instance.

Here's one simple one. `>>` is just like `>>=` except that when we "splice in" a `m b`, it's always  the same `m b`. It ignores the `a` it's replacing:

```haskell
(>>) :: Monad m => m a -> m b -> m b
ma >> mb   = ma >>= (\_ -> mb)
-- (\_ -> mb) is a little lambda function that
-- takes an argument, ignores it, and returns mb.
```


Our First Monad Instance
------------------------

Here's an updated version of `ProVal`:

```haskell
data ProVal a = ProVal a [String]
  deriving (Eq, Ord, Show, Read)
```

Rather than a single `String` for comments, it has a list of `String`s. That also means that a "plain" value `x` can simply have no comments: `ProVal x []`.

Let's make it an instance of `Monad`! `return` will make the simplest `ProVal` we can. `pa >>= f` will take the `a` value from `pa`, feed it to `f`, get the `ProVal b` it produces, and finally prepend all of `pa`'s comments onto the `ProVal b`'s comments. (So, we end up with the `b` value, but we carry all the provenance comments along, in the correct order.)

```haskell
instance Monad ProVal where
  ...
```

(*Two exercises.*)


Why Would You Bother, or, What's So Scary About I/O?
====================================================

The trouble with I/O (and side effects in general) in Haskell are (1) they aren't functional and (2) lazy evaluation means we don't even know when (or *if*) they happen.

But, what if we made a new monad called `IO`? An `IO a` is a container that describes "an action, possibly with side effects that, when run, produces an `a` value".

For the `IO` monad, `return x` makes a "fake" action that just produces `x` when run.

`ioa >>= f` makes a new action that, when run:

+ runs `ioa` *first*, 
+ gets the `a` it produces,
+ feeds it to `f`, and
+ runs the action that `f` returns *second*.

Did you see the trick there? `>>=` solves our two problems above: (1) It produces an action *value* without actually performing the action. No side effects! (2) But that value *represents* an action that does what we want *in the correct order*.

Now, we can build up actions like "read in the user's name `>>=` given string `s`, say hello to `s`", which produces an action that, when run, first reads in the user's name and then feeds that name into code that says hello to them.

> -- | An action to read in the user's name.
> -- Remember that ioa >> iob produces an action that
> -- performs ioa and then performs iob, producing iob's
> -- result. (So, like >>=, but ignoring ioa's value.)
> readName :: IO String
> readName = putStrLn "What is your name?" >> getLine
>
> -- | An action to say hello to s.
> -- () is a special type with only one value in it: ()
> -- It is similar in a sense to the void type in Java.
> -- (The type is called "unit" when you read it aloud.)
> sayHello :: String -> IO ()
> sayHello s = putStrLn ("Hello, " ++ s ++ "!")

Now, let's make this file a program that reads a user's name and greets them, using `readName` and `sayHello` above and `>>=`:

> -- | When you run a program in Haskell, it is `main` 
> -- that is run. Its `IO ()` action is created and then
> -- ACTUALLY RUN, which is magical and beyond the scope
> -- of normal Haskell.
> --
> -- In ghci, any `IO a` we evaluate at the prompt is also run.
> main :: IO ()
> main = greet
>
> -- | Get a user's name and greet them.
> -- We can use our helpers above to write this.
> greet :: IO ()
> greet = undefined

We'll just run it in `ghci`.[^runningInGHC]

(*Four exercises.*)


Our Game(s) With IO
===================

We should really make a version of our game(s) with a user interface. For lack of time, we're just going to give it to you for you to look through rather than go through it step-by-step. Try it out and investigate the code!

Here's a [playable game](/lecture-supplement/LectureIOGame.html) (minus one piece we suggest as an exercise for you!).





Function Composition: The Period at the End of Haskell's Sentence.
==================================================================

There is one last Haskell operator we're going to learn. It does function composition:

```haskell
(.) :: (b -> c) -> (a -> b) -> (a -> c)
(f . g) x = f (g x)
```

I intentionally wrote its type with parentheses around `a -> c` at the end, even though those aren't necessary. We think of `.` as an operator that takes two functions and "chains them together" to form a single function.

Since we often break a Haskell problem down into a series of functions operating one after another to produce an eventual value, `.` is incredibly handy to define new functions!

Here's `hasMSWin` that determines if a list of numbers contains any three that sum to 15:

> hasMSWin :: [Int] -> Bool
> hasMSWin ns = not (null (filter (== 15) (map sum (all3Lists ns))))

Let's write `hasMSWin' = ...`, where we use function composition to create a chain of functions on the right side:

> hasMSWin' :: [Int] -> Bool
> hasMSWin' = undefined

The new version means the same thing. Is is easier to read? Is it clearer?

Let's practice describing functions as "chains" of other functions!


(*Two exercises.*)



Note: `hasMSWin` needs these to work, but we won't review them:

> allSubLists :: Int -> ([a] -> [[a]])
> allSubLists 0 _ = [[]]
> allSubLists _ [] = []
> allSubLists n (a:as) | n < 0 = []
>                      | otherwise = map (a:) (allSubLists (n-1) as) ++ allSubLists n as
>
> all3Lists :: [a] -> [[a]]
> all3Lists = allSubLists 3



Appendix
========

Here are some extra notes on I/O in Haskell.

The Trouble with I/O
--------------------

Simon Peyton Jones ruefully and authoritatively describes the trouble with I/O in [Tackling the Awkward Squad](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/mark.pdf?from=https%3A%2F%2Fresearch.microsoft.com%2F%7Esimonpj%2Fpapers%2Fmarktoberdorf%2Fmark.pdf):
"A purely functional program implements a function; it has no side effect. Yet the ultimate purpose of running a program is invariably to cause some side effect: a changed file, some new pixels on the screen, a message sent, or whatever. Indeed it's a bit cheeky to call input/output "awkward" at all. I/O is the raison d'être of every program — a program that had no observable effect whatsoever (no input, no output) would not be very useful."


The Bad Old Days
----------------

We discussed (and Peyton Jones points out) lazy evaluation makes the I/O problem *even worse*. We don't know when the side effects (like I/O) will actually happen, or even *if* they will happen!

Laziness also presents one potential solution: Just model the input to the program as a potentially-infinite list of "input objects" from the world and output by the program as a potentially-infinite list of "output objects". An output could be requesting some information, which will hopefully eventually come back in on the input.

Haskell used to use this solution.. but it has *lots* of problems resolved by monads!


[^monadExtras]: In fact, there are [more requirements for a _correct_ monad instance](https://wiki.haskell.org/Typeclassopedia#Laws_3), and `Monad` has superclasses. So, we have a bit more work to do, but often this work is straightforward if we sensibly handle `return` and `>>=`.

[^hotelcalifornia]: I'm not sure what source I read that compared Monads to the Eagles' Hotel California line: "You can check out any time you like, but you can never leave!" But, it's too fun not to use!

[^doNotation]: [`do` notation](https://en.wikibooks.org/wiki/Haskell/do_notation) is a little language built into Haskell that you can use to make monadic programming look remarkably like imperative programming in Java. My advice is `do not` use it until you feel like you understand `return`, `>>=`, and `>>`.

[^runningInGHC]: To run this with `ghc`: comment out the `module` line at the top of the file, run `ghc Lecture6.lhs`, and then run the executable that produces (`./Lecture6`, in Unix).
