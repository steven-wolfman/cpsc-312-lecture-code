---
number: 5
title: Ad Hoc Polymorphism with Type Classes
published: 2021-10-04
exercises: https://ca.prairielearn.com/pl/course_instance/2333/assessment/16998
---

> -- We use a language extensions so we can put signatures in instance declarations.
> {-# LANGUAGE InstanceSigs #-}
> module Lecture5 where
> import Data.List (delete, minimumBy)

Ad Hoc Polymorphism 
===================

It's tempting but not possible to write Haskell code like: 

```haskell
asBool :: a -> Bool
asBool True = True
asBool _ = False
```

Because our function cannot "look inside" a value of universally polymorphic type like `a`. 

However, if you come from the JavaScript world, you may still long for the confusion of "truthiness", where
lots of different types of values can be considered as true or false.

More importantly, we would like to be able to write code that is polymorphic over values that 
meet certain constraints. Maybe they can be compared for which is larger (`Ord`), or we can find 
the next value after a given value (`Enum`), or we can "summarize" all the values contained inside
of some structure (`Foldable`, which expects a type constructor like list or `ProVal`).

Haskell uses *type classes* to accomplish that.


Making Truthy
-------------

Let's describe the behaviour we need to call some type "truthy":

> class Truthy a where
>   asBool :: a -> Bool

In a type signature (and lots of other places), we can constrain
an otherwise-universal type `x` to support `asBool` by saying that
it must be `Truthy`, like `Truthy x => ...`.


Using Truthy
------------

Let's create our own truthy `if`:

> iffy :: Truthy c => c -> a -> a -> a
> iffy condition thenExp elseExp = if (asBool condition) then thenExp else elseExp

Our signature says "`iffy`'s first argument is any value *that is `Truthy`*".
So, anything before the `=>` is a constraint on what would otherwise be 
universal (parametric) polymorphism.

(*Exercise.*)


Being Truthy
------------

Maybe for `Int`, zero is `False`, but any other `Int` is `True`.
Then, we want to be able to call on `iffy` like this:

> -- | myDiv is like div except rather than producing a
> -- divide-by-zero exception for a 0 on bottom, it produces
> -- Nothing. 
> myDiv :: Int -> Int -> Maybe Int
> myDiv top bottom = iffy bottom (Just (top `div` bottom)) Nothing

Haskell requires us to *explicitly* say if a type is an instance of a class.
Let's makes `Int`s truthy!

> instance Truthy Int where
>   -- What is the signature of asBool?
>   -- Remember that it's Int that's the Truthy thing here.
>   -- So, we should replace the Truthy class's type variable
>   -- with Int.
>   asBool :: Int -> Bool
>   asBool 0 = False
>   asBool _ = True


(*Two Exercises*)[^different]





What can we say in a type class?
--------------------------------

Haskell type classes are tremendously powerful. What `asBool` does looks a lot 
like what interfaces do in Java. However, Haskell type classes have many of the powers 
of Java interfaces as well as many that interfaces lack.

Consider the built-in `Bounded` class:

```haskell
class  Bounded a  where
    minBound, maxBound :: a
```

This says that anything that is `Bounded` has two special values `minBound`
and `maxBound`. The intended meaning of these are the smallest and largest 
representable values (although the typeclass cannot force that to be their 
actual meaning!).

Try out:

```haskell
minBound :: Int 
maxBound :: Char
```


We can also have default implementations, as in the built-in `Eq` class:

```haskell
class  Eq a  where
    (==) :: a -> a -> Bool 
    x == y     =  not (x /= y)

    (/=) :: a -> a -> Bool
    x /= y     =  not (x == y)
```

Note that these are defined in terms of each other. So, when we make an instance,
we have to define one or the other, or we'll just get infinite recursion!


(*Three Exercise*)



Shall We Play a Game?
=====================

Let's make the code for a game. In the Magic Sum game,
we start with the numbers 1, 2, 3, 4, 5, 6, 7, 8, and 9
available. Each round, each player gets to take one of 
the remaining numbers. The game ends when one player 
has exactly three numbers that sum to 15 (a win) or when all 
the numbers are taken and neither player has won (a tie).

Let's play a game together!


Magic Sum State
---------------

Let's represent the state of the game this way:

> -- | The state of the magic sum game, with the
> -- list of available numbers to take, the list of
> -- numbers I have taken, and the list of numbers
> -- you have taken.
> data MSState = MSState [Int] [Int] [Int]
>   deriving (Eq, Ord, Read, Show)


Then, we'll want to know when we've won, lost, or tied.

Let's start by finding all the sequences of three numbers a player has.
We'll solve a more general problem because it's easier to do:

> -- | Produce all sublists of exactly the given length
> allSubLists :: Int -> ([a] -> [[a]])
> allSubLists 0 _ = [[]] -- ONE sublist of length zero
> allSubLists _ [] = [] -- FAILURE; too few or too many elements!
> allSubLists n (a:as)
>   | n < 0 = [] -- FAILURE!
>   -- Otherwise try all the ones starting with a of length n-1
>   -- and all the ones NOT starting with a of length n.
>   | otherwise = map (a:) (allSubLists (n-1) as) ++
>                 allSubLists n as

So, sequences of length 3 come from:

> all3Lists :: [a] -> [[a]]
> all3Lists = allSubLists 3

Your turn. Use `all3Lists` to define `hasWin :: [Int] -> Bool` that determines if
the given list of numbers contains any three that sum to 15.

Try to do it in one, short line. I recommend figuring it out from "right to left".
That is, what's the first thing you do to the list of `Int`s? What's the next? Etc.

> hasMSWin :: [Int] -> Bool
> hasMSWin ns = not (null (filter (== 15) (map sum (all3Lists ns))))

(*Exercise.*)


> isMSWin, isMSLoss, isMSTie, isMSComplete :: MSState -> Bool 
> isMSWin (MSState _ ns _) = hasMSWin ns
> isMSLoss (MSState _ _ ns) = hasMSWin ns
>
> -- if it's not a win or loss, then isMSTie tells us if it's a tie
> isMSTie (MSState [] _ _) = True
> isMSTie _ = False
>
> isMSComplete ms = isMSWin ms || isMSLoss ms || isMSTie ms


Let's give a value to each completed state, where a win is 1,
a loss is -1, and a tie is 0:

> getMSValue :: MSState -> Maybe Double
> getMSValue ms | isMSWin ms = Just 1
>               | isMSLoss ms = Just (-1)
>               | isMSTie ms = Just 0
>               | otherwise = Nothing


Lastly, let's define the initial game state:

> initMSState :: MSState 
> initMSState = MSState [1..9] [] []

We know what a state in the game is now, but what's a move?[^efficiency]

Getting the Next Move
---------------------

Taking one move means (1) taking a number for yourself and
(2) making it your opponent's turn. Note: `delete :: Eq a => a -> [a] -> [a]` 
removes the first occurrence of its first argument from its second.
We'll also describe the move in English for clarity.

```haskell
-- The definition of String:
type String = [Char]
```

> type Move a = (String, a) -- a "type synonym" like String meaning [Char]
>
> nextMSStates :: MSState -> [Move MSState]
> nextMSStates (MSState pool me you) = 
>   map (\choice -> ("Take " ++ show choice, MSState (delete choice pool) you (choice:me))) pool


Exploring the Whole Game
------------------------

Let's define a tree of game moves:

> data GameTree a = GameTree a [GameTree a]
>   deriving (Eq, Ord, Show, Read)

A turn has the current state and all the game trees that come out of taking each 
possible move in the current state.

Now, we can define a function to construct the game tree:

> constructMSGameTree :: MSState -> GameTree MSState
> constructMSGameTree ms =
>   GameTree ms (map constructMSGameTree (map snd (nextMSStates ms)))

In fact, we can construct the *entire* game tree:

> msGameTree :: GameTree MSState
> msGameTree = constructMSGameTree initMSState

Because Haskell is lazy, this doesn't *do* anything yet. Indeed, Haskell will 
only create the game tree bit by bit as we explicitly explore it, which is pretty 
handy for games like chess with *immense* game trees!


Solving the Game
----------------

Who can resist? We're so close! Let's write some AI and pick the very
best possible move at any point.

We want to win, which means we want the highest possible value. After 
each of *our* moves, our opponent goes. They want to find the best 
possible value *for them*, which is the worst possible for us. We call 
this a "minimaxing" search because at each level of the search, we switch 
from minimizing the outcome (bad for us!) to maximizing the outcome (good 
for us!).

So, let's start by scoring a gametree using this minimaxing approach.
(For simplicity, if `getMSValue` fails for us on a tree with no next moves, 
we assume the state's value is 0. This shouldn't happen anyway!)

> -- | Get the game tree magic state value for this tree.
> -- That's the LARGEST value WE can earn starting from this point.
> -- If this game tree represents the end of the game, we just get 
> -- its value. Otherwise, we get the *smallest* value achievable in 
> -- any of the next states (minimizing our opponent's score) and
> -- negate it to get our score.
> getGTMSValue :: GameTree MSState -> Double
> getGTMSValue (GameTree state []) =
>   case getMSValue state of
>     Just v  -> v
>     Nothing -> 0 -- unnecessary case
> getGTMSValue (GameTree _ nexts) =
>   negate (minimum (map getGTMSValue nexts))




Now, let's find the best move in a game tree. To do that, we'll 
find the **worst** state we can put our opponent in among the possible 
next moves. The function `argmin` will help us with that. We can 
give it a "scoring" function and a bunch of values, and it gives us 
the value that scores least:

> -- | argmin f vals produces the value in vals for which f produces
> -- the smallest result. vals MUST NOT BE EMPTY.
> argmin :: Ord b => (a -> b) -> [a] -> a
> argmin f as = fst minTuple
>   where
>     -- Get tuples of the as and their values computed by f 
>     -- We put these in a variable so that Haskell will cache
>     -- them for us (call f just once per value in as), since
>     -- our f is potentially very expensive!
>     tuples = zip as (map f as)
>
>     -- Compare tuples by their f values
>     compareSnds t1 t2 = compare (snd t1) (snd t2)
>
>     -- Find the smallest tuple
>     minTuple = minimumBy compareSnds tuples


Now we're ready to pick our move:

> -- | Returns the next move to take, which will lead to the best
> -- possible outcome in the given game tree (i.e., as high a 
> -- value as possible). Produces Nothing if there are no further
> -- moves to take.
> pickBestMSMove :: MSState -> Maybe (Move MSState)
> pickBestMSMove state = case nextMSStates state of
>   [] -> Nothing
>   moves -> Just (argmin getMoveValue moves)
>     where getMoveValue move = 
>             getGTMSValue (constructMSGameTree (snd move))


Some Utility Functions
----------------------

It would be nice to play this against the computer.
We can take a move like this:

> -- | Produce the state created by taking the given number.
> -- Assumes the number is in the pool of available numbers.
> takeMSMove :: MSState -> Int -> MSState
> takeMSMove (MSState pool me you) n = MSState (delete n pool) you (n:me)

We can help the computer take a move like this:

> -- | Just for convenience, this version takes an MSState
> -- and does the tree contsruction itself and returns a
> -- plain MSState.. but produces an error if called on a
> -- terminal state.
> pickBestMSMove' :: MSState -> Move MSState 
> pickBestMSMove' state = 
>   maybe undefined id (pickBestMSMove state)

And, let's know when a state is a win, loss, or tie:

> msStatus :: MSState -> String
> msStatus state | isMSWin state = "win"
>                | isMSLoss state = "loss"
>                | isMSTie state = "tie"
>                | otherwise = "carry on"

`ghci` already makes the last value available as `it`.

Let's play!


What *is* a Game?
=================

*TODO: continue here!!*

OK, that's a big question for [another](https://courses.students.ubc.ca/cs/courseschedule?pname=subjarea&tname=subj-course&dept=CPSC&course=427) [course](https://courses.students.ubc.ca/cs/courseschedule?pname=subjarea&tname=subj-course&dept=DMED&course=503).
In our case, however, a "game state" for a two-player, turn-based game is really anything that:

+ can give us a value (1 for a win, 0 for a tie, -1 for a loss, `Nothing` for not-yet-done)
+ has an initial state
+ can tell us the next states available from some state

Could we rewrite our code for `pickBestMove` to use a type class and work on *any* game?


> class GameState a where
>   -- | Produces 1 for a win, -1 for a loss, 0 for a tie,
>   -- and Nothing for a not-yet-complete game.
>   getGameStateValue :: a -> Maybe Double
>
>   initGameState :: a
> 
>   -- | Produces a list of the moves and next game states after the current one.
>   -- If the game is complete, the list is empty.
>   nextGameStates :: a -> [Move a]

We'd best make `MSState` an instance! Not coincidentally, we happen to
have already written all these functions above:

> instance GameState MSState where
>   getGameStateValue = getMSValue
>   initGameState = initMSState
>   nextGameStates = nextMSStates

Now construct the game tree:

> constructGameTree :: GameState a => a -> GameTree a
> constructGameTree state = 
>   GameTree state (map constructGameTree (map snd (nextGameStates state)))

The entire game tree.. **of whatever type of game you're playing**!
This is a polymorphic value, much like `initGameState` is.

> gameTree :: GameState a => GameTree a
> gameTree = constructGameTree initGameState

The game *tree* value (as opposed to state).

> getGameTreeValue :: GameState a => GameTree a -> Double
> getGameTreeValue (GameTree state []) =
>   case getGameStateValue state of
>     Just v  -> v
>     Nothing -> 0 -- unnecessary case
> getGameTreeValue (GameTree _ nexts) =
>   negate (minimum (map getGameTreeValue nexts))

`argmin` never mentioned `MSState` anyway. So, that takes us to picking the best move:

> pickBestMove :: GameState a => a -> Maybe (Move a)
> pickBestMove state = case nextGameStates state of
>   [] -> Nothing
>   moves -> Just (argmin getMoveValue moves)
>     where getMoveValue move = 
>             getGameTreeValue (constructGameTree (snd move))

One of our utility functions is entirely specific to `MSState`,
but the second is not:

> pickBestMove' :: GameState a => a -> Move a 
> pickBestMove' state = 
>   maybe undefined id (pickBestMove state)

For reference, here is the implementation of `maybe`:

```haskell
maybe :: b -> (a -> b) -> Maybe a -> b
maybe _ f (Just a) = f a
maybe b _ Nothing = b
```

The Game of Nim
===============

Let's play Nim. There are three piles of coins, the first with 3 coins, the second with 4, and the third with 5.
On each turn, a player can remove any positive number of coins from a single heap. The player who wins is the one 
who removes the last coin.

Make Nim an instance of `GameState` and then solve it!

> -- | A state in the game of Nim. The integers must be zero or more.
> data NimState = NS Int Int Int
>   deriving (Eq, Ord, Read, Show)

> -- instance GameState NimState where


[^different]: What if you wanted a *different* interpretation of truthiness for `Int`s?
You cannot give two instances for the same type (in the same scope). However,
you *can* make a thinly veiled new type based on `Int` and make a new instance 
for that. See [`newtype`](https://wiki.haskell.org/Newtype) for more information.

[^efficiency]: There are *lots* of ways we can make the code above more efficient!
For example, we could note that a state in our game is never going to be a win.
The player whose turn it is has not taken a number yet. So, if the game is over,
either we're out of numbers or the other player won. Right away, we get to drop
the call to `isMSWin` from `getMSValue`. There are also *much* more efficient
ways to write `hasMSWin`. But.. let's not worry about that for now. What we 
have is just *barely* efficient enough and rather beautiful.
