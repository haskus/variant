{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- | Extensible ADT (EADT)

EADTs are \"extensible algebraic data types\": they can be transformed (by
adding or removing constructors) and their constructors are not tied to a
specific EADT type, hence we can use them as constructors of different EADTs.

EADT constructors and operations can be defined independently (even in
different modules) allowing a great modularity. As such they are an answer to
the \"expression problem\" (see __Background on EADTs__ at the bottom of this
page).

== Motivating example

Suppose we want to encode lambda-calculus using an ADT. We could use the
following one:

> data Expr n -- "n" represents a variable name
>    = Lambda n (Expr n)
>    | Var n
>    | App (Expr n) (Expr n)

We can define a pretty-print operation:

> prettyPrint :: Show n => Expr n -> String
> prettyPrint = \case
>    Var n      -> show n
>    Lambda n e -> mconcat ["\\",show n,".",prettyPrint e]
>    App e1 e2  -> mconcat ["(",prettyPrint e1,") (",prettyPrint e2,")"]

And we can test on an example:

> sampleDouble :: Expr String
> sampleDouble = Lambda "x" (Var "+" `App` Var "x" `App` Var "x")
>
> > putStrLn (prettyPrint sampleDouble)
> \"x".(("+") ("x")) ("x")

Now suppose that we want to add support for annotations. We can define a new
expression ADT with an additional constructor:

> data AExpr a n -- "n" represents a variable name, "a" represents an annotation
>    = ALambda n (AExpr a n)
>    | AVar n
>    | AApp (AExpr a n) (AExpr a n)
>    | Ann a (AExpr a n)

But now we need to rewrite our operations and expressions (such as
@prettyPrint@ and @sampleDouble@) to handle and to use the constructors of the
new expression ADT:

> prettyPrintA :: (Show n, Show a) => AExpr a n -> String
> prettyPrintA = \case
>    AVar n      -> show n
>    ALambda n e -> mconcat ["\\",show n,".",prettyPrintA e]
>    AApp e1 e2  -> mconcat ["(",prettyPrintA e1,") (",prettyPrintA e2,")"]
>    Ann a e     -> mconcat ["{",show a,"} ", prettyPrintA e]
>
> sampleDoubleA :: AExpr a String
> sampleDoubleA = ALambda "x" (AVar "+" `AApp` AVar "x" `AApp` AVar "x")
>
> sampleAnnA :: AExpr String String
> sampleAnnA = Ann "Double its input" sampleDoubleA

Now the problem is that we have two totally independent expression types
(@Expr@ and @AExpr@) with different operations (@prettyPrint@ vs
@prettyPrintA@) which can't be easily mixed. Moreover to define
@prettyPrintA@ we had to copy-paste @prettyPrint@ just to add a single case
alternative. Now suppose that we want to add a new function (e.g. to compute
free variables of an expression): should we implement it for @Expr@, for
@AExpr@, for both?

Finally suppose that we want to add some other constructors: we either get a
combinatorial explosion of ADTs and functions, or we give up on static checking
and use the \"largest\" ADT (which contains a superset of the constructors of
the others) with some conventions, e.g. comments and runtime assertions such as
\"at this point this expression shouldn't contain any annotation\" that are not
enforced by the compiler.

=== Motivating example with EADTs

The same example with EADTs would be written as follows. First we define the
EADTs:

> import Data.Variant.EADT
> import Data.Variant.EADT.TH
>
> data AbsF n e = AbsF n e deriving Functor
> data VarF n e = VarF n   deriving Functor
> data AppF   e = AppF e e deriving Functor
> data AnnF a e = AnnF a e deriving Functor
>
> eadtPattern 'AbsF "Abs"
> eadtPattern 'VarF "Var"
> eadtPattern 'AppF "App"
> eadtPattern 'AnnF "Ann"
>
> type Expr    n = EADT [AbsF n, VarF n, AppF]
> type AExpr a n = EADT [AbsF n, VarF n, AppF, AnnF a]

Then we define the @prettyPrint@ operation by using type classes:

> class PrettyPrint f where
>    prettyPrintF :: f String -> String
>
> instance Show n => PrettyPrint (VarF n) where
>    prettyPrintF (VarF n) = show n
>
> instance Show n => PrettyPrint (AbsF n) where
>    prettyPrintF (AbsF n e) = mconcat ["\\",show n,".",e]
>
> instance PrettyPrint AppF where
>    prettyPrintF (AppF e1 e2) = mconcat ["(",e1,") (",e2,")"]
>
> instance Show a => PrettyPrint (AnnF a) where
>    prettyPrintF (AnnF a e) = mconcat ["{",show a,"} ",e]
>
> prettyPrint :: BottomUpF PrettyPrint xs => EADT xs -> String
> prettyPrint e = bottomUp (toBottomUp @PrettyPrint prettyPrintF) e

We can test it with:

> sampleDouble :: Expr String
> sampleDouble = Abs "x" (Var "+" `App` Var "x" `App` Var "x")
>
> sampleAnn :: AExpr String String
> sampleAnn = Ann "Double its input" (liftEADT sampleDouble)
>
> > putStrLn (prettyPrint sampleDouble)
> \"x".(("+") ("x")) ("x")
>
> > putStrLn (prettyPrint sampleAnn)
> {"Double its input"} \"x".(("+") ("x")) ("x")

== EADT basics

EADTs are exposed by this module. The Template-Haskell helpers used to derive
pattern synonyms are in "Data.Variant.EADT.TH":

> import Data.Variant.EADT
> import Data.Variant.EADT.TH -- template-haskell helpers

=== Defining constructors

EADT constructors are data types that must have a 'Functor' type-class
instance. Fortunately defining such data types is easy thanks to the
@DeriveFunctor@ extension that automatically generates the 'Functor' instance
for us.

For instance, let's define the constructors for a list:

> {-# LANGUAGE DeriveFunctor #-}
>
> data ConsF a e = ConsF a e deriving (Functor)
> data NilF    e = NilF      deriving (Functor)

Note that __both__ data types are parameterised by @e@ even if @e@ isn't used
in the @NilF@ definition.

=== Defining pattern synonyms

We can match EADT values with the 'VF' pattern synonym (\"VF\" stands for
\"Variant Functor\"). To make the use of EADTs more pleasant, it is highly
recommended to define an additional pattern synonym for each constructor:

> pattern Cons :: ConsF a :<: xs => a -> EADT xs -> EADT xs
> pattern Cons a l = VF (ConsF a l)
>
> pattern Nil :: NilF :<: xs => EADT xs
> pattern Nil = VF NilF

These patterns hide the use of the 'VF' pattern and make the code much easier
to work with.

As this code is very straightforward to write, we provide Template-Haskell
helpers ('Data.Variant.EADT.TH.eadtPattern' and friends) to generate them
automatically. The previous patterns can be generated with:

> {-# LANGUAGE TemplateHaskell #-}
>
> import Data.Variant.EADT.TH
>
> eadtPattern 'ConsF "Cons"
> eadtPattern 'NilF  "Nil"

=== Defining the EADT

An EADT is just a type alias as in the following @List@ EADT example:

> type List a = EADT [ConsF a, NilF]

=== Creating values

Thanks to the pattern synonyms defined above, we can define values as we would
with a normal ADT:

> strList :: List String
> strList = Cons "How" (Cons "are" (Cons "you?" Nil))

In some cases we have to help the type-checker to determine some types. For
instance, in the following example it can't infer the @a@ type in @ConsF a@,
hence we have to use type ascriptions:

> intList :: List Int
> intList = Cons (10 :: Int) $ Cons (20 :: Int) $ Cons (30 :: Int) Nil

This is because the code is generic enough that the same pattern synonyms could
be used to build a heterogeneous list. For instance containing both @Int@ and
@Float@:

> mixedList :: EADT [ConsF Int, ConsF Float, NilF]
> mixedList = Cons (10 :: Int) $ Cons (5.0 :: Float) $ Cons (30 :: Int) Nil

We could also easily define another pattern synonym when we work on @List@ to
help the inference algorithm:

> -- pattern for a specific EADT: List a
> pattern ConsList :: a -> List a -> List a
> pattern ConsList a l = Cons a l

We can see that when we use it we don't need type ascriptions because the
@Int@ type is propagated:

> intList :: List Int
> intList = ConsList 10 $ ConsList 20 $ ConsList 30 Nil

The 'Data.Variant.EADT.TH.eadtPatternT' Template Haskell helper generates such
type-specialised patterns.

=== Matching values

It is easy and tempting to use the same pattern synonyms to match EADT values.
And indeed this works pretty well:

> showEADTList :: Show a => List a -> String
> showEADTList = \case
>    ConsList a l -> show a ++ " : " ++ showEADTList l
>    Nil          -> "Nil"
>    _            -> undefined
>
> > putStrLn (showEADTList strList)
> "How" : "are" : "you?" : Nil
>
> > putStrLn (showEADTList intList)
> 10 : 20 : 30 : Nil

However this approach is unsatisfactory for two reasons:

1. The pattern matching isn't safe: for now the compiler cannot use the EADT
   constructor type list to infer that the pattern-match is complete. Hence we
   need the wildcard match to avoid a warning and to use @ConsList@ to help the
   type inference. A better alternative is presented in the __Safe pattern
   matching__ section below.

2. The function isn't generic: if we would like to write a @showEADTList@
   function that also works on the heterogeneous @mixedList@ above or on any
   future EADT provided its constructors can be handled, we need to use
   another approach based on type-classes. This is presented in the
   following sections.

== Explicit recursive traversal

When we need to traverse a data structure, we can either use predefined
traversal functions (e.g., @map@, @fold@, etc.) or write the recursive function
explicitly. EADTs are no different in this regard.

In this section we explain how to write explicitly recursive functions for
EADTs: similarly to usual ADTs, it's better to use them only when generic
traversal functions (presented in following sections) don't fit the bill.

=== Traversal example

If we were to write a @show@ function for a list ADT, we could do it like this:

> data List a = Cons a (List a) | Nil
>
> showList :: Show a => List a -> String
> showList = \case
>    Nil      -> "Nil"
>    Cons a l -> show a ++ " : " ++ showList l

In @showList@ we can pattern match on the constructors of @List a@ because the
constructor list is closed. With EADTs the list of constructors isn't closed
and we want to be able to use the same code even with EADTs extended with more
constructors. To support this, we use type-classes to build the equivalent of
the @case@ in @showList@ above.

Let's define a class @MyShow@ that is very much like 'Show' and that we will
use to print any EADT value:

> class MyShow e where
>    myShow :: e -> String

We can define instances for the @List@ constructors defined above:

> instance MyShow (NilF e) where
>    myShow _ = "Nil"
>
> instance (MyShow e, Show a) => MyShow (ConsF a e) where
>    myShow (ConsF a l) = show a ++ " : " ++ myShow l

Note how each instance corresponds to an alternative in @showList@.

It also requires some additional instances to traverse the 'VariantF'
combinator datatype and the 'EADT' recursion-handling datatype:

> {-# LANGUAGE UndecidableInstances #-}
> {-# LANGUAGE FlexibleInstances #-}
>
> instance MyShow (VariantF f (EADT f)) => MyShow (EADT f) where
>    {-# INLINE myShow #-}
>    myShow (EADT e) = myShow e
>
> instance MyShow (VariantF [] e) where
>    {-# INLINE myShow #-}
>    myShow = undefined
>
> instance
>       ( MyShow (f e)
>       , MyShow (VariantF fs e)
>       ) => MyShow (VariantF (f ': fs) e)
>    where
>       {-# INLINE myShow #-}
>       myShow v = case popVariantFHead v of
>          Right u -> myShow u
>          Left  w -> myShow w

Note: this boilerplate code (hopefully always very similar and straightforward)
is the main reason you should strive to use predefined recursion schemes
instead of the explicit approach presented here.

Note: the @INLINE@ pragmas are used to ensure that in the generated code we
get the equivalent of the @case@ expression in @showList@.

Now we can test it:

> strList :: List String
> strList = Cons "How" (Cons "are" (Cons "you?" Nil))
>
> intList :: List Int
> intList = Cons (10 :: Int) $ Cons (20 :: Int) $ Cons (30 :: Int) Nil
>
> mixedList :: EADT [ConsF Int, ConsF Float, NilF]
> mixedList = Cons (10 :: Int) $ Cons (5.0 :: Float) $ Cons (30 :: Int) Nil
>
> > putStrLn (myShow strList)
> "How" : "are" : "you?" : Nil
>
> > putStrLn (myShow intList)
> 10 : 20 : 30 : Nil
>
> > putStrLn (myShow mixedList)
> 10 : 5.0 : 30 : Nil

=== Extension example

If we add a new constructor, such as @NodeF@ to build binary trees:

> data NodeF a e = NodeF a e e deriving (Functor)
>
> eadtPattern 'NodeF "Node"

We can also add a @MyShow@ instance for @NodeF@:

> instance (MyShow e, Show a) => MyShow (NodeF a e) where
>    myShow (NodeF a l1 l2) = show a ++ "\n|- " ++ indent (myShow l1)
>                                    ++ "|- " ++ indent (myShow l2)
>       where
>          indent' []     = []
>          indent' (x:xs) = x : fmap ("   "++) xs
>          indent = unlines . indent' . lines

Now we can show binary trees as well as lists:

> tree :: EADT [NodeF Int, NilF]
> tree = Node (10 :: Int)
>          (Node (5 :: Int) Nil Nil)
>          (Node (30 :: Int) Nil Nil)
>
> > putStrLn (myShow tree)
> 10
> |- 5
>    |- Nil
>    |- Nil
> |- 30
>    |- Nil
>    |- Nil

We can also mix up trees and lists by using @ConsF@ and @NodeF@ in the same
EADT:

> mixedTree :: EADT [NodeF Int, ConsF Int, NilF]
> mixedTree = Node (10 :: Int)
>          (Cons (5 :: Int) $ Cons (6 :: Int) $ Cons (7 :: Int) Nil)
>          (Node (30 :: Int) Nil Nil)
>
> > putStrLn (myShow mixedTree)
> 10
> |- 5 : 6 : 7 : Nil
> |- 30
>    |- Nil
>    |- Nil

== Constraining constructors with @:\<:@

The ':<:' type operator is used to ensure that a constructor is present in an
EADT. For example if we consider the following type signature (that will be
developed in the example below):

> distr :: (AddF :<: f, MulF :<: f) => EADT f -> Maybe (EADT f)

The constructors of @EADT f@ are not specified but the constraints
@(AddF :\<: f, MulF :\<: f)@ ensure that at least @AddF@ and @MulF@
constructors are present.

Note that to shorten a list of constraints such as
@(AddF :\<: f, MulF :\<: f)@ you can use the ':<<:' operator:
@[AddF,MulF] :\<\<: f@.

=== Transformation example

Suppose we have the following EADT for arithmetic expressions:

> {-# LANGUAGE DeriveFunctor #-}
>
> data ValF e = ValF Int deriving (Functor)
> data AddF e = AddF e e deriving (Functor)
> data MulF e = MulF e e deriving (Functor)
>
> eadtPattern 'ValF "Val"
> eadtPattern 'AddF "Add"
> eadtPattern 'MulF "Mul"
>
> type Expr = EADT [ValF, AddF, MulF]

We can define some value:

> e1 :: Expr
> e1 = Add (Val 10)
>          (Mul (Add (Val 5)
>                    (Val 10))
>               (Val 7))

We can define instances of the @MyShow@ class (defined above):

> instance MyShow (ValF e) where
>   myShow (ValF e) = show e
>
> instance MyShow e => MyShow (AddF e) where
>   myShow (AddF x y) = "(" ++ myShow x ++ " + " ++ myShow y ++ ")"
>
> instance MyShow e => MyShow (MulF e) where
>   myShow (MulF x y) = "(" ++ myShow x ++ " * " ++ myShow y ++ ")"
>
> > putStrLn (myShow e1)
> (10 + ((5 + 10) * 7))

Now we can define a transformation that distributes multiplication over
addition as follows:

> -- distribute multiplication over addition if it matches
> distr :: (AddF :<: f, MulF :<: f) => EADT f -> Maybe (EADT f)
> distr (Mul a (Add c d)) = Just (Add (Mul a c) (Mul a d))
> distr (Mul (Add c d) a) = Just (Add (Mul c a) (Mul d a))
> distr _                 = Nothing

Note that this function works on any EADT as long as it has @AddF@ and
@MulF@ constructors. We indicate such constraints with the ':<:' type operator.

Then we need a helper function that performs the traversal of the EADT:

> import Control.Arrow ((>>>))
>
> -- bottom up traversal that performs an additional bottom up traversal in
> -- the transformed sub-tree when a transformation occurs.
> bottomUpFixed :: Functor (VariantF cs) => (EADT cs -> Maybe (EADT cs)) -> EADT cs -> EADT cs
> bottomUpFixed f = project >>> fmap (bottomUpFixed f) >>> embed >>> f'
>    where
>       f' u = case f u of
>          Nothing -> u
>          Just v  -> bottomUpFixed f v
>
> -- | Distribute multiplication over addition
> distribute :: ([AddF,MulF] :<<: cs, Functor (VariantF cs)) => EADT cs -> EADT cs
> distribute = bottomUpFixed distr

Note: @bottomUpFixed@ is a generic recursion scheme over an EADT. You can read
more on this approach in the section __Recursion schemes and EADTs__ below.

Finally we can test the transformation on an example:

> > putStrLn (myShow e1)
> (10 + ((5 + 10) * 7))
>
> > putStrLn (myShow (distribute e1))
> (10 + ((5 * 7) + (10 * 7)))

=== Extensibility

Suppose we add a @PowF@ (power) constructor:

> data PowF e = PowF e e deriving (Functor)
>
> eadtPattern 'PowF "Pow"
>
> instance MyShow e => MyShow (PowF e) where
>   myShow (PowF x y) = "(" ++ myShow x ++ " ^ " ++ myShow y ++ ")"

We can now write expressions that use the @Pow@ constructor:

> type Expr2 = EADT [ValF, AddF, MulF, PowF]
>
> e2 :: Expr2
> e2 = Pow (Val 10)
>          (Mul (Add (Pow (Val 5) (Val 8))
>                    (Val 10))
>               (Val 7))

We can check that our distribution function still works on this new type of
expression without being modified at all:

> > putStrLn (myShow (distribute e2))
> (10 ^ (((5 ^ 8) * 7) + (10 * 7)))

== Recursion schemes and EADTs

Traversing an EADT explicitly (see __Explicit recursive traversal__ above) can
be tedious. Another approach consists in using dedicated composable combinators
called /recursion schemes/.

The well known @map@ and @fold@ functions are examples of recursion schemes
for lists: these functions handle the recursive traversal of the data
structure and are parameterized by the functions performing the actual work.
Recursion schemes are a generalization of this approach.

A good introduction to recursion schemes can be found here:
<https://blog.sumtypeofway.com/an-introduction-to-recursion-schemes/>

See also: <https://bartoszmilewski.com/2013/06/10/understanding-f-algebras/>

This package re-exports the @recursion-schemes@ machinery (e.g.,
'Data.Functor.Foldable.cata', 'Data.Functor.Foldable.ana',
'Data.Functor.Foldable.para', 'Data.Functor.Foldable.apo', etc.) and provides
in addition the simpler 'BottomUpF' / 'BottomUp' / 'bottomUp' helpers in
"Data.Variant.Functor" to dispatch a type-class method to every constructor in
the EADT.

=== Catamorphism: Show example

The 'EADTShow' class shipped with this module is the canonical example. We can
roll our own to see how it works:

> class FunctorShow (f :: Type -> Type) where
>   functorShow :: f String -> String

We can define instances for @NilF@ and @ConsF@:

> instance FunctorShow NilF where
>   functorShow _ = "Nil"
>
> instance Show a => FunctorShow (ConsF a) where
>   functorShow (ConsF a l) = show a ++ " : " ++ l

Note that there is no recursive call in the definition of the @ConsF@ instance:
it is because we are going to use a recursion scheme that will handle the
recursion.

Finally we can define a generic @myShow@ function that uses the catamorphism
recursion scheme with the @functorShow@ class method.

> myShow :: BottomUpF FunctorShow xs => EADT xs -> String
> myShow = bottomUp (toBottomUp @FunctorShow functorShow)

We can test it:

> intList :: List Int
> intList = Cons (10 :: Int) $ Cons (20 :: Int) $ Cons (30 :: Int) Nil
>
> mixedList :: EADT [ConsF Int, ConsF Float, ConsF String, NilF]
> mixedList = Cons @Int 10 $ Cons @Float 5.0 $ Cons "Test" Nil
>
> > putStrLn $ myShow intList
> 10 : 20 : 30 : Nil
>
> > putStrLn $ myShow mixedList
> 10 : 5.0 : "Test" : Nil

This is exactly how 'eadtShow' is implemented in this module: see 'EADTShow'
and 'eadtShow'.

=== Catamorphism: List mapping example

Similarly to the example above, suppose that we want to implement mapping over
an EADT list. We can use the following type-class:

> class MapEADT a xs (f :: Type -> Type) where
>   -- map the outer constructor of an EADT
>   mapEADT1 :: (a -> a) -> f (EADT xs) -> EADT xs

We need some instances to handle our EADT constructors:

> instance (NilF :<: xs) => MapEADT a xs NilF where
>   mapEADT1 _ NilF = Nil
>
> instance (ConsF a :<: xs) => MapEADT a xs (ConsF a) where
>   mapEADT1 f (ConsF a x) = Cons (f a) x

Now we can define the @mapEADT@ function by using the catamorphism combinator
@'Data.Functor.Foldable.cata'@ together with 'BottomUp' and 'toBottomUp':

> mapEADT :: ( BottomUpF (MapEADT a xs) xs
>            ) => (a -> a) -> EADT xs -> EADT xs
> mapEADT f = bottomUp (toBottomUp @(MapEADT a xs) (mapEADT1 f))

We can test it:

> intList :: List Int
> intList = Cons (10 :: Int) $ Cons (20 :: Int) $ Cons (30 :: Int) Nil
>
> > putStrLn $ myShow $ mapEADT ((+5) :: Int -> Int) intList
> 15 : 25 : 35 : Nil

== Safe pattern matching with @>:>@

Suppose we have the following @List@ EADT:

> data ConsF a l = ConsF a l deriving (Functor)
> data NilF    l = NilF      deriving (Functor)
>
> eadtPattern 'ConsF "Cons"
> eadtPattern 'NilF  "Nil"
>
> type List a = EADT [ConsF a, NilF]
>
> -- pattern for a specific EADT: List a
> pattern ConsList :: a -> List a -> List a
> pattern ConsList a l = Cons a l

Using classic pattern matching on @List@ constructors as we do below isn't
really typesafe because the compiler cannot detect that the pattern matching is
complete, hence we have the choice between a warning or adding a wildcard
match:

> showEADTList :: Show a => List a -> String
> showEADTList = \case
>    ConsList a l -> show a ++ " : " ++ showEADTList l
>    Nil          -> "Nil"
>    _            -> undefined -- this line avoids the warning but is unsafe
>                              -- if we add constructors in the future

A safe alternative is to rely on multi-continuations: we can transform any
@EADT [A,B,C]@ into a function whose type is @(A -> r, B -> r, C -> r) -> r@
with the @('Data.Variant.ContFlow.>:>')@ operator. Then we can safely provide
a function per constructor as in a pattern-matching.

=== Explicit recursion example

> import Data.Variant.ContFlow
>
> showCont' l = l >:>
>    ( \(ConsF a r) -> show a ++ " : " ++ showCont' r -- explicit recursion
>    , \NilF        -> "Nil"
>    )
>
> > showCont' intList
> "10 : 20 : 30 : Nil"

=== Recursion schemes (catamorphism)

> showCont l = l >:>
>    ( \(ConsF a r) -> show a ++ " : " ++ r -- no explicit recursion
>    , \NilF        -> "Nil"
>    )
>
> > cata showCont intList
> "10 : 20 : 30 : Nil"

== EADT constructor removal/transformation

Removing constructors from an EADT is equivalent to transforming every instance
of these constructors into other constructors of another EADT.

We consider 3 cases:

1. Fixed input EADT type; fixed list of constructors to act on
2. Generic input EADT type; fixed list of constructors to act on
3. Generic input EADT type; extensible list of constructors to act on

Note in the 3 cases we need to specify the resulting EADT type as it could be
anything fulfilling the constraints.

=== Fixed input, fixed matches

If the type of the input EADT is fixed, we can use safe pattern-matching with
the @('Data.Variant.ContFlow.>:>')@ operator as follows:

> -- replace Even and Odd constructors with a Cons constructor
> removeOddEven l = l >:>
>    (\(EvenF a r) -> Cons a r
>    ,\(OddF  a r) -> Cons a r
>    ,\NilF        -> Nil
>    )
>
> eo :: EADT [EvenF Int, OddF Int, NilF]
> eo = Even (10 :: Int) $ Odd (5 :: Int) $ Odd (7 :: Int) Nil
>
> > eadtShow (cata removeOddEven eo :: List Int)
> "10 : 5 : 7 : Nil"

Note that @removeOddEven@ only works on a specific EADT. If we want it to work
on any EADT that contains @EvenF@ and @OddF@ constructors, read the following
sections.

=== Generic input, fixed matches

If we want @removeOddEven@ to work on input EADTs of any type, we can extract
the constructors that we are interested in with 'splitVariantF' and lift the
left-over constructors with 'liftVariantF' as follows:

> removeOddEven x = case splitVariantF @[EvenF Int, OddF Int] x of
>    -- replace Even and Odd constructors with a Cons constructor
>    Right v        -> v >:>
>                         ( \(EvenF a l) -> Cons a l
>                         , \(OddF a l)  -> Cons a l
>                         )
>    -- do nothing to the other constructors
>    Left leftovers -> EADT (liftVariantF leftovers)
>
> eo1 :: EADT [EvenF Int, OddF Int, NilF]
> eo1 = Even (10 :: Int) $ Odd (5 :: Int) $ Odd (7 :: Int) Nil
>
> > eadtShow (cata removeOddEven eo1 :: List Int)
> "10 : 5 : 7 : Nil"
>
> -- additional `ConsF Int` constructor
> eo2 :: EADT [ConsF Int, EvenF Int, OddF Int, NilF]
> eo2 = Even (10 :: Int) $ Cons (5 :: Int) $ Odd (7 :: Int) Nil
>
> > eadtShow (cata removeOddEven eo2 :: List Int)
> "10 : 5 : 7 : Nil"

=== Generic input, extensible matches

If we want the @removeOddEven@ pattern match to be extensible, we can use
type-classes with an overlappable instance handling the generic case (i.e.
that only transfers constructors from one EADT to another without modifying
them).

> class RemoveOddEven ys (f :: Type -> Type) where
>    removeOddEven :: f (EADT ys) -> EADT ys
>
> -- replace Odd and Even with Cons
> instance ConsF a :<: ys => RemoveOddEven ys (OddF a) where
>    removeOddEven (OddF a l) = Cons a l
>
> instance ConsF a :<: ys => RemoveOddEven ys (EvenF a) where
>    removeOddEven (EvenF a l) = Cons a l
>
> -- handle remaining constructors generically
> instance {-# OVERLAPPABLE #-} f :<: ys => RemoveOddEven ys f where
>    removeOddEven = VF -- keep the other constructors unmodified

Then we lift it through the @VariantF@ combinator with 'BottomUp' /
'toBottomUp':

> myRemoveOddEven :: BottomUpF (RemoveOddEven ys) xs => EADT xs -> EADT ys
> myRemoveOddEven = bottomUp (toBottomUp @(RemoveOddEven ys) removeOddEven)

Test:

> eo :: EADT [EvenF Int, OddF Int, NilF]
> eo = Even (10 :: Int) $ Odd (5 :: Int) $ Odd (7 :: Int) Nil
>
> > eadtShow (myRemoveOddEven eo :: List Int)
> "10 : 5 : 7 : Nil"
>
> -- EADT with an additional `ConsF Int` constructor
> eo2 :: EADT [ConsF Int, EvenF Int, OddF Int, NilF]
> eo2 = Even (10 :: Int) $ Odd (5 :: Int) $ Cons (7 :: Int) $ Odd (7 :: Int) Nil
>
> > eadtShow (myRemoveOddEven eo2 :: List Int)
> "10 : 5 : 7 : 7 : Nil"
>
> -- EADT with an additional `ConsF String` constructor
> eo3 :: EADT [ConsF Int, EvenF Int, OddF Int, ConsF String, NilF]
> eo3 = Even (10 :: Int) $ Cons "Test" $ Odd (5 :: Int) $ Cons (7 :: Int) $ Odd (7 :: Int) Nil
>
> > eadtShow (myRemoveOddEven eo3 :: EADT [ConsF Int, ConsF String, NilF])
> "10 : \"Test\" : 5 : 7 : 7 : Nil"

We can extend @myRemoveOddEven@ to support other constructors by adding new
instances of @RemoveOddEven@ for them.

== Splitting EADT constructors

We can chose to handle only a subset of the constructors of an EADT by using
'splitVariantF'.

For instance in the following example we only handle @EvenF Int@ and
@OddF Int@ constructors. The other ones are considered as left-overs:

> alg x = case splitVariantF @[EvenF Int, OddF Int] x of
>   Right v        -> v >:>
>                        ( \(EvenF _ l) -> "Even : " ++ l
>                        , \(OddF  _ l) -> "Odd : "  ++ l
>                        )
>   Left _leftovers -> "something else"

We can test this code with:

> eo :: EADT [EvenF Int, OddF Int, NilF]
> eo = ... -- some value
>
> eo2 :: EADT [ConsF Int, EvenF Int, OddF Int, NilF]
> eo2 = Even (10 :: Int) $ Odd (5 :: Int) $ Cons (7 :: Int) $ Odd (7 :: Int) Nil
>
> > cata alg eo
> "Odd : Even : Odd : something else"
>
> > cata alg eo2
> "Even : Odd : something else"

Note that the traversal ends when it encounters an unhandled constructor.

== Background on EADTs

=== Why not Variant?

Extensible ADT (EADT) adds support for recursive datatypes to the 'V' / Variant
type (see "Data.Variant"). Indeed if we tried to define a recursive datatype
(e.g., a list) by using Variants, we would get the following error:

> data Cons a l = Cons a l
> data Nil      = Nil
>
> > type L a = V [Cons a (L a), Nil]
>
> <interactive>:19:2: error:
>     Cycle in type synonym declarations:
>       <interactive>:19:2-34: type L a = V [Cons a (L a), Nil]

The issue is that there is a cyclic definition and it isn't allowed. We could
introduce ad-hoc datatypes (e.g., @newtype L a = L (V [Cons a (L a),Nil])@) to
break this cycle but this would defeat our purpose because the datatype
wouldn't be generic anymore.

'EADT' is the datatype we use to break these cycles. By always using the same
datatype, we can provide functions that work for every EADTs. 'EADT' is very
similar to the @Fix@ datatype (fixed point of a functor). We use our own type
to declare our own instances.

For example with EADTs we just have to write the following code to declare a
@List@:

> data ConsF a l = ConsF a l deriving (Functor)
> data NilF    l = NilF      deriving (Functor)
>
> type List a = EADT [ConsF a, NilF]

=== History

==== The expression problem (1998)

In 1998, Philip Wadler defined the /Expression Problem/ as follows:

> The Expression Problem is a new name for an old problem. The goal is to
> define a datatype by cases, where one can add new cases to the datatype and
> new functions over the datatype, without recompiling existing code, and
> while retaining static type safety

See:

* <https://en.wikipedia.org/wiki/Expression_problem>
* <http://homepages.inf.ed.ac.uk/wadler/papers/expression/expression.txt>

In Haskell it is straightforward to add new functions over an ADT. Suppose we
have the following arithmetic expression ADT:

> data Expr = Val Int | Add Expr Expr

We can independently add an evaluator function, potentially in another module:

> eval :: Expr -> Int
> eval (Val x)   =  x
> eval (Add x y) = eval x + eval y

However if we want to add a new constructor to the ADT (say support for
multiplication), we have to modify both the ADT definition and the functions
using it:

> data Expr = .... | Mul Expr Expr
>
> eval :: Expr -> Int
> ....
> eval (Mul x y) = eval x * eval y

What we want is to be able to add a new independent module containing both the
@Mul@ constructor and the code to handle it, without modifying the other
modules defining the other constructors and the other code to handle them!

==== Data types à la carte (2008)

Ten years later (in 2008), Wouter Swierstra described a technique to handle
this in his well-known
<http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf Data types à la carte>
paper. The first idea is to define data constructors independently of the ADT
and to use a type parameter to leave open the ADT they are part of.

> -- Independent data constructors. Parameter `e` represents the ADT they
> -- will be part of. It is required even if it is not used in the right hand
> -- side.
> data Val e = Val Int deriving (Functor)
> data Add e = Add e e deriving (Functor)

Defining a new independent constructor is easy:

> data Mul e = Mul e e deriving (Functor)

The second idea is to use a combinator data type @:+:@:

> data (f :+: g) e = Inl (f e) | Inr (g e)
>
> instance (Functor f, Functor g) => Functor (f :+: g) where ...

It is similar to 'Either' except that it passes the same additional type
parameter to both @f@ and @g@ type constructors. It can be used to compose
independent data constructors without creating a new data type:

> type ExprF = Val :+: Add

@ExprF@ has kind @Type -> Type@ and its type parameter is used as the @e@
parameter of the independent data constructors. We can set it to arbitrary
types such as @Int@ to build valid values:

> y = Inr (Add 5 8) :: ExprF Int

However the main use of this parameter should be to indicate the type of the
expression data type we want to build, say @Expr@. Hence we would like to
write something like this:

> type Expr = ExprF Expr
>
>  > error:
>  Cycle in type synonym declarations:
>    <interactive>:12:1-22: type Expr = ExprF Expr

Oops, we can't build this cyclic (infinite) type. This leads us to the third
idea: use another data type to handle the recursive nature of the expression
type:

> newtype Expr = Expr (ExprF Expr)

We can abstract over it to use the same data type for different expression
types:

> -- `Fix` type as defined in Data.Functor.Foldable for instance
> newtype Fix f = Fix (f (Fix f))
>
> type Expr = Fix ExprF

In summary, the approach uses 3 different sorts of data types:

1. Constructor data types: @Val@, @Add@, @Mul@...
2. Combinator data type: @:+:@
3. Recursion handling data type: @Fix@

By using these different data types we have untangled the construction of ADTs
(algebraic data types) and we can freely add new constructor data types and
mix them into different algebraic data types.

Operations on these algebraic data types can be defined independently by using
type-classes and recursion schemes.

==== EADT - Extensible ADT (2018)

The EADT approach builds on Swierstra's one but it replaces the combinator
data type @:+:@ with the 'VariantF' one based on Variant (see "Data.Variant").
Similarly to the @:+:@ combinator data type, 'VariantF' passes its @e@
parameter to all of its \"member\" types and has an instance of the 'Functor'
class.

> newtype VariantF (xs :: [Type -> Type]) e = VariantF (Variant (ApplyAll e xs))
>
> -- ApplyAll e [f,g,h] ==> [f e, g e, h e]
>
> instance Functor (VariantF xs) where ....

Now instead of writing @f :+: g :+: h :+: i@ to combine constructor data types
to form an ADT we can write @VariantF [f,g,h,i]@. Just like using 'V' is more
efficient -- O(1) memory usage and (de)construction -- than using a nest of
'Either', using 'VariantF' is more efficient than using a nest of @:+:@.

Finally an EADT is just @Fix (VariantF xs)@ except that we use our own 'EADT'
newtype instead of @Fix@ in order to define our own additional (and non-orphan)
type-class instances. 'EADT' implements 'Recursive' and 'Corecursive'
type-classes from the @recursion-schemes@ package, so usual @Fix@ functions
should work on 'EADT' too.

> newtype EADT xs = EADT (VariantF xs)

The next step is to define bidirectional pattern synonyms (see __Defining
pattern synonyms__ above) that make the manipulation of EADT values very
similar to the manipulation of usual ADTs. By using Template Haskell, these
patterns can be automatically generated.

In summary EADTs provide a nicer interface and a better asymptotic
implementation in both memory and runtime execution than Data types à la carte.
In the future it would be better to have native support for all of this in the
language, especially to enhance compilation times by not using type families.

-}
module Data.Variant.EADT
   ( EADT (..)
   , (:<:)
   , (:<<:)
   , pattern VF
   , appendEADT
   , liftEADT
   , popEADT
   , contToEADT
   , contToEADTM
   , EADTShow (..)
   , eadtShow
   -- * Reexport
   , module Data.Variant.Functor
   , module Data.Variant.VariantF
   )
where

import Data.Variant
import Data.Variant.VariantF
import Data.Variant.Types
import Data.Variant.ContFlow
import Data.Variant.Functor

import GHC.TypeLits

-- $setup
-- >>> :seti -XDataKinds
-- >>> :seti -XTypeApplications
-- >>> :seti -XTypeOperators
-- >>> :seti -XFlexibleContexts
-- >>> :seti -XTypeFamilies
-- >>> :seti -XPatternSynonyms
-- >>> :seti -XDeriveFunctor
-- >>>
-- >>> import Data.Functor.Classes
-- >>>
-- >>> data ConsF a e = ConsF a e deriving (Eq,Ord,Show,Functor)
-- >>> data NilF    e = NilF      deriving (Eq,Ord,Show,Functor)
-- >>>
-- >>> instance Eq a => Eq1 (ConsF a) where liftEq cmp (ConsF a e1) (ConsF b e2) = a == b && cmp e1 e2
-- >>> instance Eq1 NilF where liftEq _ _ _ = True
-- >>>
-- >>> :{
-- >>> pattern Cons :: ConsF a :<: xs => a -> EADT xs -> EADT xs
-- >>> pattern Cons a l = VF (ConsF a l)
-- >>> pattern Nil :: NilF :<: xs => EADT xs
-- >>> pattern Nil = VF NilF
-- >>> type ListF a = VariantF '[NilF, ConsF a]
-- >>> type List a = EADT '[NilF, ConsF a]
-- >>> :}
--
-- >>>
-- >>> let a = Cons "Hello" (Cons "World" Nil) :: List String
-- >>> let b = Cons "Bonjour" (Cons "Monde" Nil) :: List String
-- >>> a == b
-- False
-- >>> a == a
-- True


-- | An extensible ADT
newtype EADT fs
   = EADT (VariantF fs (EADT fs))

type instance Base (EADT fs) = VariantF fs

instance Functor (VariantF fs) => Recursive (EADT fs) where
   project (EADT a) = a

instance Functor (VariantF fs) => Corecursive (EADT fs) where
   embed = EADT

instance Eq1 (VariantF fs) => Eq (EADT fs) where
  EADT a == EADT b = eq1 a b

instance Ord1 (VariantF fs) => Ord (EADT fs) where
  compare (EADT a) (EADT b) = compare1 a b

instance Show1 (VariantF fs) => Show (EADT fs) where
  showsPrec d (EADT a) =
    showParen (d >= 11)
      $ showString "EADT "
      . showsPrec1 11 a

-- | Constructor `f` is in `xs`
type family f :<: xs where
   f :<: xs = EADTF' f (EADT xs) xs

-- | Forall `x` in `xs`, `x :<: ys`
type family (:<<:) xs ys :: Constraint where
   '[] :<<: ys       = ()
   (x ': xs) :<<: ys = (x :<: ys, xs :<<: ys)

type EADTF' f e cs =
   ( Member f cs
   , Index (IndexOf (f e) (ApplyAll e cs)) (ApplyAll e cs) ~ f e
   , PopVariant (f e) (ApplyAll e cs)
   , KnownNat (IndexOf (f e) (ApplyAll e cs))
   , Remove (f e) (ApplyAll e cs) ~ ApplyAll e (Remove f cs)
   )

-- | Pattern-match in an extensible ADT
pattern VF :: forall e f cs.
   ( e ~ EADT cs  -- allow easy use of TypeApplication to set the EADT type
   , f :<: cs     -- constraint synonym ensuring `f` is in `cs`
   ) => f (EADT cs) -> EADT cs
pattern VF x = EADT (VariantF (VSilent x))
   -- `VSilent` matches a variant value without checking the membership: we
   -- already do it with :<:

-- | Append new "constructors" to the EADT
appendEADT :: forall ys xs zs.
   ( zs ~ Concat xs ys
   , ApplyAll (EADT zs) zs ~ Concat (ApplyAll (EADT zs) xs) (ApplyAll (EADT zs) ys)
   , Functor (VariantF xs)
   ) => EADT xs -> EADT zs
appendEADT (EADT v) = EADT (appendVariantF @ys (fmap (appendEADT @ys) v))

-- | Lift an EADT into another
liftEADT :: forall e as bs.
   ( e ~ EADT bs
   , LiftVariantF as bs e
   , Functor (VariantF as)
   ) => EADT as -> EADT bs
liftEADT = cata (EADT . liftVariantF)

-- | Pop an EADT value
popEADT :: forall f xs e.
   ( f :<: xs
   , e ~ EADT xs
   , f e :< ApplyAll e xs
   ) => EADT xs -> Either (VariantF (Remove f xs) (EADT xs)) (f (EADT xs))
popEADT (EADT v) = popVariantF v

-- | MultiCont instance
--
-- >>> let f x = toCont x >::> (const "[]", \(ConsF u us) -> u ++ ":" ++ f us)
-- >>> f a
-- "Hello:World:[]"
instance (Functor (VariantF xs), ContVariant (ApplyAll (EADT xs) xs)) => MultiCont (EADT xs) where
   type MultiContTypes (EADT xs) = ApplyAll (EADT xs) xs
   toCont  (EADT v) = variantFToCont v
   toContM f        = variantFToContM (project <$> f)

-- | Convert a multi-continuation into an EADT
contToEADT ::
   ( ContVariant (ApplyAll (EADT xs) xs)
   ) => ContFlow (ApplyAll (EADT xs) xs)
                 (V (ApplyAll (EADT xs) xs))
     -> EADT xs
contToEADT c = EADT (contToVariantF c)

-- | Convert a multi-continuation into an EADT
contToEADTM ::
   ( ContVariant (ApplyAll (EADT xs) xs)
   , Monad f
   ) => ContFlow (ApplyAll (EADT xs) xs)
                 (f (V (ApplyAll (EADT xs) xs)))
     -> f (EADT xs)
contToEADTM f = EADT <$> contToVariantFM f


-- | Show an EADT constructor.
--
-- Provide an instance per constructor and use 'eadtShow' to display the whole
-- EADT.
class EADTShow f where
   eadtShow' :: f String -> String

-- | Show an EADT
eadtShow :: forall xs. BottomUpF EADTShow xs => EADT xs -> String
eadtShow = bottomUp (toBottomUp @EADTShow eadtShow')
