{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module EGADT
   ( testsEGADT
   )
where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Data.Kind
import Data.Variant.EGADT

-------------------------------
-- Typed lambda-calculus AST
-------------------------------

data LamF (ast :: Type -> Type) t where
   LamF :: (ast a -> ast b) -> LamF ast (a -> b)

data AppF (ast :: Type -> Type) t where
   AppF :: ast (a -> b) -> ast a -> AppF ast b

data VarF (ast :: Type -> Type) t where
   VarF :: String -> VarF ast Int

type AST a = EGADT '[LamF, AppF, VarF] a

-- replace any variable name with "zz"
renameVar :: AST Int -> AST Int
renameVar (VF (VarF _)) = VF (VarF "zz")
renameVar _             = error "Unhandled case"

-- application of a lambda to a variable: well-typed at `AST Int`
applyLamToVar :: AST Int
applyLamToVar = VF (AppF (VF (LamF renameVar)) (VF (VarF "a")))

-------------------------------
-- Tests
-------------------------------

testsEGADT :: TestTree
testsEGADT = testGroup "EGADT" $
   [ testProperty "VarF: pattern match extracts the variable name" $
        case VF (VarF "a") :: AST Int of
           VF (VarF x) -> x == "a"
           _           -> False

   , testProperty "AppF + LamF: well-typed application is constructible" $
        case applyLamToVar of
           VF (AppF _ _) -> True
           _             -> False
   ]
