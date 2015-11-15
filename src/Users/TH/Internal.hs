{-# LANGUAGE TemplateHaskell #-}

module Users.TH.Internal where

import Language.Haskell.TH

functionLevels :: Type -> Int
functionLevels = go 0
  where
    go :: Int -> Type -> Int
    go n (AppT (AppT ArrowT _) rest) =
      go (n+1) rest
    go n (ForallT _ _ rest) =
      go n rest
    go n _ =
      n

getType :: Info -> Maybe Type
getType (ClassOpI _ t _ _) = Just t
getType (DataConI _ t _ _) = Just t
getType (VarI _ t _ _)     = Just t
getType (TyVarI _ t)       = Just t
getType _                  = Nothing
