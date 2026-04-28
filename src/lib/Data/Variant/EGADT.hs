{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

{- | Extensible GADT (EGADT)

EGADTs are to GADTs what 'Data.Variant.EADT.EADT' is to plain ADTs: an EGADT
carries an extra type index just like a GADT, but its constructors are
__open__ — defined independently as standalone data types and combined into
an EGADT through a type-level list, so the same constructor can be reused in
different EGADTs and new constructors can be added without touching existing
code.

> -- closed: a fixed set of constructors, each constraining the index `t`
> data AST t where
>    Var :: String -> AST Int
>    Lam :: (AST a -> AST b) -> AST (a -> b)
>    App :: AST (a -> b) -> AST a -> AST b
>
> -- open: each constructor is its own data type; the EGADT is a type alias
> data VarF ast t where
>   VarF :: String -> VarF ast Int
> data LamF ast t where
>   LamF :: (ast a -> ast b) -> LamF ast (a -> b)
> data AppF ast t where
>   AppF :: ast (a -> b) -> ast a -> AppF ast b
>
> type AST t = EGADT [VarF, LamF, AppF] t

The first parameter of each constructor (@ast@ above) is the
\"recursion-handling\" parameter, similar to the @e@ parameter of EADT
constructors (see "Data.Variant.EADT"); t'EGADT' substitutes itself for it.
The
second parameter (@t@) is the GADT-style type index that constructors can
constrain (e.g., @VarF@ forces @t ~ Int@).

== Defining constructors

EGADT constructors use GADT syntax. They take two extra type parameters in
addition to their fields:

* the recursion parameter (@ast@): the type used for recursive sub-terms;
* the index (@t@): the type the term represents.

> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE KindSignatures #-}
>
> import Data.Kind (Type)
>
> data VarF (ast :: Type -> Type) t where
>   VarF :: String -> VarF ast Int
>
> data LamF (ast :: Type -> Type) t where
>   LamF :: (ast a -> ast b) -> LamF ast (a -> b)
>
> data AppF (ast :: Type -> Type) t where
>   AppF :: ast (a -> b) -> ast a -> AppF ast b

Like a GADT, the result type @VarF ast t@ is constrained inside the
constructor (here @t ~ Int@). Unlike a GADT, the constructor doesn't fix what
@ast@ is — the EGADT machinery will tie the recursive knot.

== Defining the EGADT

An EGADT is just a type alias listing the open constructors and exposing the
index parameter:

> type AST t = EGADT [VarF, LamF, AppF] t

== Creating values

Use the 'VF' pattern synonym to wrap a constructor value into an EGADT. The
GADT-style refinement of the index follows the constructor:

> -- a variable always has type Int
> v :: AST Int
> v = VF (VarF "x")
>
> -- (\x -> x) :: Int -> Int
> i :: AST (Int -> Int)
> i = VF (LamF (\x -> x))
>
> -- application of an `Int -> Int` to an `Int` produces an `Int`
> e :: AST Int
> e = VF (AppF i v)

Trying to build an ill-typed term (e.g., applying @v@ to itself) is a
compile-time error, just like with a regular GADT.

== Pattern matching

Match on EGADT values with the 'VF' pattern synonym. Inside each branch the
GADT refinement of @t@ is in scope, so the body can use type-class instances
or operations that depend on @t@:

> isVar :: AST t -> Bool
> isVar = \case
>    VF (VarF _) -> True  -- here t ~ Int
>    _           -> False

== Membership constraints: ':<!' and ':<<!'

The ':<!' type operator is the EGADT equivalent of
'Data.Variant.EADT.:<:' — it constrains a constructor to be present in the
constructor list:

> -- works on any EGADT that has at least the VarF constructor
> mkVar :: (VarF :<! fs) => String -> EGADT fs Int
> mkVar s = VF (VarF s)

The ':<<!' operator (analogous to 'Data.Variant.EADT.:<<:') shortens a list of
':<!' constraints:

> -- @[VarF, AppF] :<<! fs@ is shorthand for @(VarF :<! fs, AppF :<! fs)@

== Recursion schemes

EGADT supports the higher-order recursion schemes from
"Data.Variant.Functor" via its 'HRecursive' and 'HCorecursive' instances. Use
'hcata' / 'hana' / 'hpara' / 'hapo' / 'hhylo' to traverse an EGADT in a
type-indexed way.

== See also

* "Data.Variant.EADT" — extensible recursive ADTs without a GADT-style
  index. The motivation, history and design discussion there apply to EGADTs
  as well.
* "Data.Variant.VariantF" — the underlying open functor that EGADT builds on.
* "Data.Variant.Functor" — recursion schemes and the higher-order variants
  used by EGADT.

-}
module Data.Variant.EGADT where

import Unsafe.Coerce
import GHC.TypeLits
import Data.Kind
import Control.Monad

import Data.Variant
import Data.Variant.VariantF
import Data.Variant.Types
import Data.Variant.Functor

-- | An EADT with an additional type parameter
newtype EGADT fs t = EGADT (HVariantF fs (EGADT fs) t)

newtype HVariantF (fs :: [ (k -> Type) -> ( k -> Type) ]) (ast :: k -> Type) (t :: k)
  = HVariantF (VariantF (ApplyAll ast fs) t)

toHVariantAt
  :: forall i fs ast a
  .  KnownNat i
  => (Index i fs) ast a -> VariantF (ApplyAll ast fs) a
{-# INLINABLE toHVariantAt #-}
toHVariantAt a = VariantF (Variant (natValue' @i) (unsafeCoerce a))

fromHVariantAt
  :: forall i fs ast a
  .  KnownNat i
  => VariantF (ApplyAll ast fs) a -> Maybe ((Index i fs) ast a)
{-# INLINABLE fromHVariantAt #-}
fromHVariantAt (VariantF (Variant t a)) = do
  guard (t == natValue' @i)
  return (unsafeCoerce a)

type instance HBase (EGADT xs) = HVariantF xs

instance HFunctor (HVariantF xs) => HRecursive (EGADT xs) where
  hproject (EGADT a) = a

instance HFunctor (HVariantF xs) => HCorecursive (EGADT xs) where
  hembed = EGADT

type family f :<! fs :: Constraint where
  f :<! fs = ( MemberAtIndex (IndexOf f fs) f fs )

type family MemberAtIndex (i :: Nat) f fs :: Constraint where
  MemberAtIndex i f fs = ( KnownNat i, Index i fs ~ f )

type family (:<<!) xs ys :: Constraint where
  '[]       :<<! ys = ()
  (x ': xs) :<<! ys = (x :<! ys, xs :<<! ys)

-- | Pattern-match in an extensible GADT
pattern VF :: forall e a f fs.
  ( e ~ EGADT fs a  -- allow easy use of TypeApplication to set the EGADT type
  , f :<! fs
  ) => f (EGADT fs) a -> EGADT fs a
pattern VF x <- ( ( \ ( EGADT (HVariantF v) ) -> fromHVariantAt @(IndexOf f fs) @fs v ) -> Just x )
  where
    VF x = EGADT (HVariantF (toHVariantAt @(IndexOf f fs) @fs x))
