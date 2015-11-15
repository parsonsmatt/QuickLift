{-# LANGUAGE TemplateHaskell #-}

module Users.TH where

import Database.Persist.TH
import Control.Monad
import Control.Monad.Reader

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Users.TH.Internal

import Web.Users.Persistent
import Web.Users.Types

deriveReader :: Name -> DecsQ
deriveReader rd =
  mapM (decForFunc rd) 
    [ 'destroyUserBackend
    , 'housekeepBackend
    , 'getUserIdByName
    , 'getUserById
    , 'listUsers
    , 'countUsers
    , 'createUser
    , 'updateUser
    , 'updateUserDetails
    , 'authUser
    , 'deleteUser
    ]

decForFunc :: Name -> Name -> Q Dec
decForFunc reader fn = do
  info <- reify fn
  arity <- maybe (reportError "Unable to get arity of name" >> return 0)
        (return . functionLevels) 
        (getType info)
  varNames <- replicateM (arity - 1) (newName "arg")
  b <- newName "b"
  let fnName     = mkName . nameBase $ fn
      bound      = AppE (VarE '(>>=)) (VarE reader)
      binder     = AppE bound . LamE [VarP b]
      varExprs   = map VarE (b : varNames)
      fullExpr   = foldl AppE (VarE fn) varExprs
      liftedExpr = AppE (VarE 'liftIO) fullExpr
      final      = binder liftedExpr
      varPat     = map VarP varNames
  return $ FunD fnName [Clause varPat (NormalB final) []]
           
