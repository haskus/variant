{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

-- | Rebindable syntax for Variant
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
