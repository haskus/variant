{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Variant.Types
  ( natValue
  , natValue'
  , Index
  , Concat
  , Length
  , Product
  , Remove
  , Nub
  , Reverse
  , IndexOf
  , MaybeIndexOf
  , Member
  , InsertAt
  , ReplaceAt
  , IndexesOf
  , ReplaceN
  , ReplaceNS
  , Complement
  , RemoveAt
  , RemoveAt1
  , Tail
  , Constraint
  , ConstraintAll1
  )
where

import GHC.TypeLits
import Data.Kind
import Data.Proxy

-- | Get a Nat value
natValue :: forall (n :: Nat) a. (KnownNat n, Num a) => a
{-# INLINABLE natValue #-}
natValue = fromIntegral (natVal (Proxy :: Proxy n))

-- | Get a Nat value as a Word
natValue' :: forall (n :: Nat). KnownNat n => Word
{-# INLINABLE natValue' #-}
natValue' = natValue @n

-- | Indexed access into the list
type Index (n :: Nat) (l :: [k]) = Index' n l l

-- | Indexed access into the list
type family Index' (n :: Nat) (l :: [k]) (l2 :: [k]) :: k where
   Index' 0 (x ': _ ) _  = x
   Index' n (_ ': xs) l2 = Index' (n-1) xs l2
   Index' n '[]       l2 = TypeError ( 'Text "Index "
                                ':<>: 'ShowType n
                                ':<>: 'Text " out of bounds for list:"
                                ':$$: 'Text " "
                                ':<>: 'ShowType l2 )

-- | Concat two type lists
type family Concat (xs :: [k]) (ys :: [k]) :: [k] where
   Concat '[] '[]      = '[]
   Concat '[] ys       = ys
   Concat (x ': xs) ys = x ': Concat xs ys

-- | Get list length
type family Length (xs :: [k]) :: Nat where
   Length xs = Length' 0 xs

type family Length' n (xs :: [k]) :: Nat where
   Length' n '[]       = n
   Length' n (x ': xs) = Length' (n+1) xs
                                             
-- | Product of two lists
type family Product (xs :: [Type]) (ys :: [Type]) :: [Type] where
   Product '[] ys    = '[]
   Product xy '[]    = '[]
   Product (x:xs) ys = Concat (Product' x ys) (Product xs ys)

type family Product' (x :: Type) (ys :: [Type]) :: [Type] where
   Product' x '[]       = '[]
   Product' x (y ': ys) = (x,y) ': Product' x ys

-- | Remove `a` in `l`
type family Remove (a :: k) (l :: [k]) :: [k] where
   Remove a '[]       = '[]
   Remove a (a ': as) = Remove a as
   Remove a (b ': as) = b ': Remove a as


-- | Keep only a single value of each type
type family Nub (l :: [k]) :: [k] where
   Nub xs = Reverse (Nub' xs '[])

type family Nub' (as :: [k]) (xs :: [k]) :: [k] where
   Nub' '[]       xs = xs
   Nub' (x ': as) xs = Nub' (Remove x as) (x ': xs) 

-- | Reverse a list
type family Reverse (l :: [k]) :: [k] where
   Reverse l = Reverse' l '[]

type family Reverse' (l :: [k]) (l2 :: [k]) :: [k]  where
   Reverse' '[] l       = l
   Reverse' (x ': xs) l = Reverse' xs (x ': l)

-- | Get the first index of a type
type IndexOf (x :: k) (xs :: [k]) = IndexOf' (MaybeIndexOf x xs) x xs

-- | Get the first index of a type
type family IndexOf' (i :: Nat) (a :: k) (l :: [k]) :: Nat where
   IndexOf' 0 x l = TypeError ( 'ShowType x
                          ':<>: 'Text " not found in list:"
                          ':$$: 'Text " "
                          ':<>: 'ShowType l )
   IndexOf' i _ _ = i - 1

-- | Get the first index (starting from 1) of a type or 0 if none
type family MaybeIndexOf (a :: k) (l :: [k]) where
   MaybeIndexOf x xs = MaybeIndexOf' 0 x xs

-- | Helper for MaybeIndexOf
type family MaybeIndexOf' (n :: Nat) (a :: k) (l :: [k]) where
   MaybeIndexOf' n x '[]       = 0
   MaybeIndexOf' n x (x ': xs) = n + 1
   MaybeIndexOf' n x (y ': xs) = MaybeIndexOf' (n+1) x xs

-- | Constraint: x member of xs
type family Member x xs :: Constraint where
   Member x xs = MemberAtIndex (IndexOf x xs) x xs
   
type MemberAtIndex i x xs =
   ( x ~ Index i xs
   , KnownNat i
   )

-- | Constraint: all the xs are members of ys
type family Members xs ys :: Constraint where
   Members '[] ys       = ()
   Members (x ': xs) ys = (Member x ys, Members xs ys)

-- | Insert a list at n
type family InsertAt (n :: Nat) (l :: [k]) (l2 :: [k]) :: [k] where
   InsertAt 0 xs ys        = Concat ys xs
   InsertAt n (x ': xs) ys = x ': InsertAt (n-1) xs ys

-- | replace l[n] with l2 (folded)
type family ReplaceAt (n :: Nat) (l :: [k]) (l2 :: [k]) :: [k] where
   ReplaceAt 0 (x ': xs) ys = Concat ys xs
   ReplaceAt n (x ': xs) ys = x ': ReplaceAt (n-1) xs ys

-- | Get all the indexes of a type
type family IndexesOf (a :: k) (l :: [k]) :: [Nat] where
   IndexesOf x xs = IndexesOf' 0 x xs

-- | Get the first index of a type
type family IndexesOf' n (a :: k) (l :: [k]) :: [Nat] where
   IndexesOf' n x '[]       = '[]
   IndexesOf' n x (x ': xs) = n ': IndexesOf' (n+1) x xs
   IndexesOf' n x (y ': xs) = IndexesOf' (n+1) x xs

-- | replace a type at offset n in l
type family ReplaceN (n :: Nat) (t :: k) (l :: [k]) :: [k] where
   ReplaceN 0 t (x ': xs)  = (t ': xs)
   ReplaceN n t (x ': xs)  = x ': ReplaceN (n-1) t xs

-- | replace types at offsets ns in l
type family ReplaceNS (ns :: [Nat]) (t :: k) (l :: [k]) :: [k] where
   ReplaceNS '[] t l       = l
   ReplaceNS (i ': is) t l = ReplaceNS is t (ReplaceN i t l)

-- | Complement xs \ ys
type family Complement (xs :: [k]) (ys :: [k]) :: [k] where
   Complement xs '[]    = xs
   Complement xs (y:ys) = Complement (Remove y xs) ys

-- | Remove a type at index
type family RemoveAt (n :: Nat) (l :: [k]) :: [k] where
   RemoveAt 0 (x ': xs) = xs
   RemoveAt n (x ': xs) = x ': RemoveAt (n-1) xs

-- | Remove a type at index (0 == don't remove)
type family RemoveAt1 (n :: Nat) (l :: [k]) :: [k]  where
   RemoveAt1 0 xs        = xs
   RemoveAt1 1 (x ': xs) = xs
   RemoveAt1 n (x ': xs) = x ': RemoveAt1 (n-1) xs

-- | Tail of a list
type family Tail (xs :: [k]) :: [k] where
   Tail (x ': xs) = xs


-- | Build a list of constraints
-- e.g., ConstraintAll1 Eq '[A,B,C] ==> (Eq A, Eq B, Eq C)
type family ConstraintAll1 (f :: k -> Constraint) (xs :: [k]) :: Constraint where
   ConstraintAll1 f '[]       = ()
   ConstraintAll1 f (x ': xs) = (f x, ConstraintAll1 f xs)
