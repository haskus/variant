{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns #-}

{- | Continuation-based control-flow

This module provides safe pattern matching on 'Data.Variant.V' values using
multi-continuations. Instead of pattern matching with the @V@ pattern (which
the compiler cannot check for completeness), we can provide a function per
constructor as in a pattern-match.

== Safe pattern matching with ordered continuations ('>:>')

With multi-continuations we can transform a variant @V [A,B,C]@ into a
function whose type is @(A -> r, B -> r, C -> r) -> r@. Hence the compiler
will ensure that we provide the correct number of alternatives in the
continuation tuple.

Applying a multi-continuation to a Variant is done with '>:>':

> import Data.Variant.ContFlow
>
> printV :: V [String,Int,Float] -> IO ()
> printV v = v >:>
>    ( \s -> putStrLn ("Found string: " ++ s)
>    , \i -> putStrLn ("Found int: " ++ show i)
>    , \f -> putStrLn ("Found float: " ++ show f)
>    )

== Safe pattern matching with unordered continuations ('>%:>')

By using the '>%:>' operator instead of '>:>', we can provide continuations in
any order as long as an alternative for each constructor is provided.

The types must be unambiguous as the Variant constructor types cannot be used to
infer the continuation types (as is done with '>:>'). Hence the type
ascriptions in the following example:

> printU :: V [String,Int,Float] -> IO ()
> printU v = v >%:>
>    ( \f -> putStrLn ("Found float: " ++ show (f :: Float))
>    , \s -> putStrLn ("Found string: " ++ s)
>    , \i -> putStrLn ("Found int: " ++ show (i :: Int))
>    )

-}
module Data.Variant.ContFlow
   ( ContFlow (..)
   , ContTuple
   , (>:>)
   , (>-:>)
   , (>%:>)
   , (>::>)
   , (>:-:>)
   , (>:%:>)
   , ToMultiCont
   , MultiCont (..)
   )
where

import Data.Kind
import Data.Variant.Tuple

-- | A continuation based control-flow
newtype ContFlow (xs :: [Type]) r = ContFlow (ContTuple xs r -> r)

-- | Convert a list of types into the actual data type representing the
-- continuations.
type family ContTuple (xs :: [Type]) r where
   ContTuple xs r = Tuple (ToMultiCont xs r)

type family ToMultiCont xs r where
   ToMultiCont '[] r       = '[]
   ToMultiCont (x ': xs) r = (x -> r) ': ToMultiCont xs r

-- | A multi-continuable type
class MultiCont a where
   type MultiContTypes a :: [Type]

   -- | Convert a data into a multi-continuation
   toCont :: a -> ContFlow (MultiContTypes a) r

   -- | Convert a data into a multi-continuation (monadic)
   toContM :: Monad m => m a -> ContFlow (MultiContTypes a) (m r)


-- | Bind a multi-continuable type to a tuple of continuations
(>:>) :: MultiCont a => a -> ContTuple (MultiContTypes a) r -> r
{-# INLINABLE (>:>) #-}
(>:>) a !cs = toCont a >::> cs

infixl 0 >:>

-- | Bind a single-continuable type to a 1-tuple of continuations
(>-:>) :: (MultiCont a, MultiContTypes a ~ '[b]) => a -> (b -> r) -> r
{-# INLINABLE (>-:>) #-}
(>-:>) a c = toCont a >:-:> c

infixl 0 >-:>

-- | Bind a multi-continuable type to a tuple of continuations and
-- reorder fields if necessary
(>%:>) ::
   ( MultiCont a
   , ReorderTuple ts (ContTuple (MultiContTypes a) r)
   ) => a -> ts -> r
{-# INLINABLE (>%:>) #-}
(>%:>) a !cs = toCont a >:%:> cs

infixl 0 >%:>


-- | Bind a flow to a tuple of continuations
(>::>) :: ContFlow xs r -> ContTuple xs r -> r
{-# INLINABLE (>::>) #-}
(>::>) (ContFlow f) !cs = f cs

infixl 0 >::>

-- | Bind a flow to a 1-tuple of continuations
(>:-:>) :: ContFlow '[a] r -> (a -> r) -> r
{-# INLINABLE (>:-:>) #-}
(>:-:>) (ContFlow f) c = f (MkSolo c)

infixl 0 >:-:>

-- | Bind a flow to a tuple of continuations and
-- reorder fields if necessary
(>:%:>) :: forall ts xs r.
   ( ReorderTuple ts (ContTuple xs r)
   ) => ContFlow xs r -> ts -> r
{-# INLINABLE (>:%:>) #-}
(>:%:>) (ContFlow f) !cs = f (tupleReorder cs)

infixl 0 >:%:>
