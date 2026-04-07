{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

{- | Rebindable syntax for Variant

This module provides @RebindableSyntax@-based do-notation for 'V'. We
recommend using 'Data.Variant.VEither' or "Data.Variant.Excepts" instead, which
provide proper 'Monad' instances without requiring @RebindableSyntax@.

== Do-notation with Variants

We can use do-notation with 'V' as we would with other sum types such as
'Maybe' or 'Either'. However, as we cannot have a 'Monad' instance for 'V', we
rely on the @RebindableSyntax@ extension to mimic it.

The leftmost type is extracted from the Variant with @>>=@ (or @x \<-
myVariant@ with do-notation syntax). Variant types are concatenated on the
left.

Function @foo@ in the following example composes functions returning Variants
by using do-notation:

> {-# LANGUAGE TypeApplications #-}
> {-# LANGUAGE RebindableSyntax #-}
>
> import Data.Variant
> import Data.Variant.Syntax
>
> import Prelude hiding (head,lookup,(>>=),(>>),return)
> import qualified Prelude
> import Text.Read
>
> foo :: String -> V [Integer, ParseError, LookupError Char, HeadError]
> foo str = do
>    c <- head str
>    r <- lookup c codeMap
>    parse (r ++ tail str)
>
>    where
>       codeMap :: [(Char, String)]
>       codeMap = [ ('x', "0x")
>                 , ('d', "")
>                 ]
>
>
> data ParseError = ParseError deriving Show
>
> parse :: String -> V [Integer,ParseError]
> parse s = case readMaybe s of
>    Just i  -> V @Integer i
>    Nothing -> V ParseError
>
> data HeadError = ListWasEmpty deriving Show
>
> head :: [a] -> V [a,HeadError]
> head []    = toVariantAt @1 ListWasEmpty
> head (x:_) = toVariantAt @0 x
>
> data LookupError k = KeyWasNotPresent k deriving Show
>
> lookup :: Eq k => k -> [(k,v)] -> V [v,LookupError k]
> lookup k vs = case Prelude.lookup k vs of
>    Just v  -> toVariantAt @0 v
>    Nothing -> toVariantAt @1 (KeyWasNotPresent k)

Test:

> > foo "d10"
> V @Integer 10
>
> > foo "x10"
> V @Integer 16
>
> > foo "u10"
> V @(LookupError Char) (KeyWasNotPresent 'u')
>
> > foo ""
> V @HeadError ListWasEmpty
>
> > foo "d10X"
> V @ParseError ParseError

-}
module Data.Variant.Syntax
   ( (>>=)
   , (>>)
   , return
   )
where

import Data.Variant
import Data.Variant.Types
import GHC.TypeLits

import Prelude hiding ((>>=),(>>),return)

(>>=) :: forall x xs ys. 
   ( KnownNat (Length ys)
   ) => V (x ': xs) -> (x -> V ys) -> V (Concat ys xs)
(>>=) = bindVariant

(>>) :: V xs -> V ys -> V (Concat ys xs)
(>>) = constBindVariant

return :: x -> V '[x]
return = variantFromValue
