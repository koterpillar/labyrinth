{-# Language TemplateHaskell #-}

module Peeker ( Peek
              , (~>)
              , getP
              , updP
              , getS
              , updS
              , liftP
              , listP
              , derivePeek
              ) where

import Control.Monad.State

import Language.Haskell.TH hiding (listP)

type Peek a b = a -> (b, b -> a)

(~>) :: Peek a b -> Peek b c -> Peek a c
(~>) p1 p2 x = (getP p2 y, updP p1 x . updP p2 y)
               where y = getP p1 x

getP :: Peek a b -> a -> b
getP p = fst . p

updP :: Peek a b -> a -> b -> a
updP p = snd . p

getS :: (MonadState a m) => Peek a b -> m b
getS p = get >>= return . getP p

updS :: (MonadState a m) => Peek a b -> b -> m ()
updS p v = modify $ flip (updP p) v

liftP :: Peek a a
liftP x = (x, id)

listP :: Int -> Peek [a] a
listP i l = (l !! i, \y -> take i l ++ [y] ++ drop (i + 1) l)

derivePeek :: Name -> Q [Dec]
derivePeek rec = do
    TyConI (DataD _ _ _ [RecC _ fields] _) <- reify rec
    decls <- mapM (derivePeekFor rec) fields
    return $ concat decls

derivePeekFor :: Name -> (Name, a, Type) -> Q [Dec]
derivePeekFor rec fieldDec = do
    let (field, _, fieldType) = fieldDec
    let fieldName = nameBase field
    let (accessorName, "_") = splitAt (length fieldName - 1) fieldName
    let accessor = mkName accessorName
    accessorType <- [t| Peek $(conT rec) $(return fieldType) |]
    param <- newName "param"
    update <- newName "update"
    let updateBody = LamE [VarP update] $
                        RecUpdE (VarE param) [(field, VarE update)]
    body <- [| ($(varE field) $(varE param), $(return updateBody)) |]
    return [ SigD accessor accessorType
           , FunD accessor [ Clause [VarP param] (NormalB body) []
                           ]
           ]
