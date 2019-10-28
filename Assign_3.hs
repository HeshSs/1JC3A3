{- Assignment 3
 - Name: Hishmat Salehi
 - Date: 1/11/2019
 -}
module Assign_3 where

import qualified Data.Map.Strict as IM

macid :: String
macid = "salehh6"

data Poly a = X
            | Coef a
            | Sum (Poly a) (Poly a)
            | Prod (Poly a) (Poly a)
  deriving Show


newtype PolyList a = PolyList [a]
  deriving Show


{- -----------------------------------------------------------------
 - polyValue
 - -----------------------------------------------------------------
 - Description: TODO add comments on polyValue here
 -}
polyValue :: Num a => Poly a -> a -> a
polyValue p n = error "TODO: define me"

{- -----------------------------------------------------------------
 - polyListValue
 - -----------------------------------------------------------------
 - Description: TODO add comments on polyListValue here
 -}
polyListValue :: (Num a,Eq a) => PolyList a -> a -> a
polyListValue p1 n = error "TODO: define me"

{- -----------------------------------------------------------------
 - polyListSum
 - -----------------------------------------------------------------
 - Description: TODO add comments on polyListSum here
 -}
polyListSum :: (Num a,Eq a) => PolyList a -> PolyList a -> PolyList a
polyListSum p1 q1 = error "TODO: define me"


{- -----------------------------------------------------------------
 - polyListDegree
 - -----------------------------------------------------------------
 - Description: TODO add comments on polyListDegree here
 -}
polyListDegree :: (Num a,Eq a) => PolyList a -> Integer
polyListDegree p1 = error "TODO: define me"

{- -----------------------------------------------------------------
 - polyListProd
 - -----------------------------------------------------------------
 - Description: TODO add comments on polyListProd here
 -}
polyListProd :: (Num a,Eq a) => PolyList a -> PolyList a -> PolyList a
polyListProd p1 q1 = error "TODO: define me"

{- -----------------------------------------------------------------
 - polyListToPoly
 - -----------------------------------------------------------------
 - Description: TODO add comments on polyListToPoly here
 -}
polyListToPoly :: Num a => PolyList a -> Poly a
polyListToPoly p1 = error "TODO: define me"

{- -----------------------------------------------------------------
 - polyToPolyList
 - -----------------------------------------------------------------
 - Description: TODO add comments on polyToPolyList here
 -}
polyToPolyList :: (Num a,Eq a) => Poly a -> PolyList a
polyToPolyList p = error "TODO: define me"
