{-# Language TemplateHaskell #-}

module Peeker ( Peek
              , (~>)
              , get
              , upd
              , liftP
              , derivePeek
              ) where

import Language.Haskell.TH

type Peek a b = a -> (b, b -> a)

(~>) :: Peek a b -> Peek b c -> Peek a c
(~>) p1 p2 x = (get p2 y, upd p1 x . upd p2 y)
               where y = get p1 x 

get :: Peek a b -> a -> b
get p = fst . p

upd :: Peek a b -> a -> b -> a
upd p = snd . p

liftP :: Peek a a
liftP x = (x, id)

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
