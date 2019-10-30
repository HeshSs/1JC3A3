{- Assignment 3
 - Name: Hishmat Salehi
 - Date: 1/11/2019
 -}
module Assign_3 where

import qualified Data.Map.Strict as IM

macid :: String
macid = "Salehh6"

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
 - Description: polyValue p n is the value of p at n
 -}
polyValue :: Num a => Poly a -> a -> a
polyValue X n = n
polyValue (Coef a) _ = a
polyValue (Sum a b) n = polyValue a n + polyValue b n
polyValue (Prod a b) n = polyValue a n * polyValue b n

{- -----------------------------------------------------------------
 - polyListValue
 - -----------------------------------------------------------------
 - Description: polyListValue p1 n is the value of the PolyList p1 at n
 -}
polyListValue :: (Num a,Eq a) => PolyList a -> a -> a
polyListValue (PolyList p1) n = if p1 == []
                                then 0
                                    else ((p1 !! lastIndex) * (n ^ lastIndex)) + polyListValue (PolyList (take lastIndex p1)) n
                                        where lastIndex = (length p1 - 1)

{- -----------------------------------------------------------------
 - polyListSum
 - -----------------------------------------------------------------
 - Description: Returns the sum of the polynomials represented by p1 and q1.
 -}
polyListSum :: (Num a,Eq a) => PolyList a -> PolyList a -> PolyList a
polyListSum (PolyList p1) (PolyList q1) = if length p1 > length q1
                                          then PolyList ((zipWith (+) p1 q1) ++ (drop (length q1) p1))
                                             else PolyList ((zipWith (+) p1 q1) ++ (drop (length p1) q1))

{- -----------------------------------------------------------------
 - polyListDegree
 - -----------------------------------------------------------------
 - Description: Returns the degree of the polynomial represented by p1.
 -}
polyListDegree :: (Num a,Eq a) => PolyList a -> Integer
polyListDegree (PolyList p1) = if p1 == [] || (zipWith (+) p1 p1) == p1
                               then 0
                                   else toInteger (length p1 - 1)

{- -----------------------------------------------------------------
 - polyListProd
 - -----------------------------------------------------------------
 - Description: Returns the product of the polynomials represented by p1 and q1.
 -}
polyListProd :: (Num a,Eq a) => PolyList a -> PolyList a -> PolyList a
polyListProd (PolyList p1) (PolyList q1) = if p1 == [] || q1 == []
                                           then PolyList []
                                               else if length p1 > length q1
                                                    then polyListSum (PolyList ((replicate lastIndexQ 0) ++ (map (*(q1 !! lastIndexQ)) p1))) (polyListProd (PolyList p1) (PolyList (take lastIndexQ q1)))
                                                        else polyListSum (PolyList ((replicate lastIndexP 0) ++ (map (*(p1 !! lastIndexP)) q1))) (polyListProd (PolyList (take lastIndexP p1)) (PolyList q1))
                                                            where lastIndexP = (length p1 - 1)
                                                                  lastIndexQ = (length q1 - 1)

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
