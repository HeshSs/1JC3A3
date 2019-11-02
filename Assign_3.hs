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
                                   else if p1 !! (length p1 - 1) == 0
                                        then polyListDegree (PolyList (take (length p1 - 1) p1))
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
 - toPolyConverter
 - -----------------------------------------------------------------
 - Description: Converts part of a polynomial with coefficient c and power i (c * x ^ i) to Poly 
 -} 
toPolyConverter :: (Num a,Eq a) => Int -> a -> Poly a
toPolyConverter i c = if i == 0
                      then (Prod (Coef 1) (Coef c))
                          else (Prod X (toPolyConverter (i-1) c))

{- -----------------------------------------------------------------
 - polyListToPoly
 - -----------------------------------------------------------------
 - Description: Converts a PolyList of form (PolyList [a0, a1,....,an]) to Poly
 -}
polyListToPoly :: (Num a,Eq a) => PolyList a -> Poly a
polyListToPoly (PolyList p1) = if length p1 == 1 
                               then toPolyConverter lastIndex (p1 !! lastIndex)
                                   else if length p1 == 0
                                        then error "Enter a non-empty PolyList"
                                            else (Sum (polyListToPoly (PolyList (take lastIndex p1))) (toPolyConverter lastIndex (p1 !! lastIndex)))
                                                  where lastIndex = (length p1 - 1)

{- -----------------------------------------------------------------
 - polyToPolyList
 - -----------------------------------------------------------------
 - Description: TODO add comments on polyToPolyList here
 -}
polyToPolyList :: (Num a,Eq a) => Poly a -> PolyList a
polyToPolyList p = error "TODO: define me"

{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 -
 - -----------------------------------------------------------------
 - - Function: polyValue
 - - Test Case Number: 1
 - - Input: polyValue (polyListToPoly (PolyList [1,2,3,4])) 1
 - - Expected Output: 10
 - - Actual Output: 10
 - -----------------------------------------------------------------
 - - Function: polyValue
 - - Test Case Number: 2
 - - Input: polyValue (polyListToPoly (PolyList [1,2,3,4])) 100
 - - Expected Output: 4030201
 - - Actual Output: 4030201
 - -----------------------------------------------------------------
 - - Function: polyValue
 - - Test Case Number: 3
 - - Input: polyValue (polyListToPoly (PolyList [1,2,3])) (-10)
 - - Expected Output: 281
 - - Actual Output: 281
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyListValue
 - - Test Case Number: 1
 - - Input: polyListValue (PolyList [1,2,3,4]) 1
 - - Expected Output: 10
 - - Actual Output: 10
 - -----------------------------------------------------------------
 - - Function: polyListValue
 - - Test Case Number: 2
 - - Input: polyListValue (PolyList [1,2,3,4]) 100
 - - Expected Output: 4030201
 - - Actual Output: 4030201
 - -----------------------------------------------------------------
 - - Function: polyListValue
 - - Test Case Number: 3
 - - Input: polyListValue (PolyList [1,2,3]) (-10)
 - - Expected Output: 281
 - - Actual Output: 281
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyListSum
 - - Test Case Number: 1
 - - Input: polyListSum (PolyList [1,2,3]) (PolyList [1,2,3])
 - - Expected Output: PolyList [2,4,6]
 - - Actual Output: PolyList [2,4,6]
 - -----------------------------------------------------------------
 - - Function: polyListSum
 - - Test Case Number: 2
 - - Input: polyListSum (PolyList [3,2,1]) (PolyList [1,2,3,4])
 - - Expected Output: PolyList [4,4,4,4]
 - - Actual Output: PolyList [4,4,4,4]
 - -----------------------------------------------------------------
 - - Function: polyListSum
 - - Test Case Number: 3
 - - Input: polyListSum (PolyList [0,0,0,0,0]) (PolyList [1,2,3,4])
 - - Expected Output: PolyList [1,2,3,4,0]
 - - Actual Output: PolyList [1,2,3,4,0]
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyListDegree
 - - Test Case Number: 1
 - - Input: polyListDegree (PolyList [1,2,3,4,5])
 - - Expected Output: 4
 - - Actual Output: 4
 - -----------------------------------------------------------------
 - - Function: polyListDegree
 - - Test Case Number: 2
 - - Input: ppolyListDegree (PolyList [0,0,0,0,0])
 - - Expected Output: 0
 - - Actual Output: 0
 - -----------------------------------------------------------------
 - - Function: polyListDegree
 - - Test Case Number: 3
 - - Input: polyListDegree (polyListSum (PolyList [0,0,0,0,0]) (PolyList [1,2,3,4]))
 - - Expected Output: 3
 - - Actual Output: 3
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyListProd
 - - Test Case Number: 1
 - - Input: polyListProd  (PolyList [1,1,1,1]) (PolyList [1,1,1,0,1])
 - - Expected Output: PolyList [1,2,3,3,3,2,1,1]
 - - Actual Output: PolyList [1,2,3,3,3,2,1,1]
 - -----------------------------------------------------------------
 - - Function: polyListProd
 - - Test Case Number: 2
 - - Input: polyListProd  (PolyList [1,2,3,4]) (PolyList [1,0,1,0])
 - - Expected Output: PolyList [1,2,4,6,3,4,0]
 - - Actual Output: PolyList [1,2,4,6,3,4,0]
 - -----------------------------------------------------------------
 - - Function: polyListProd
 - - Test Case Number: 3
 - - Input: polyListProd  (PolyList [1,2,3,4]) (PolyList [0,0,0,0,1])
 - - Expected Output: PolyList [0,0,0,0,1,2,3,4]
 - - Actual Output: PolyList [0,0,0,0,1,2,3,4]
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyListToPoly
 - - Test Case Number: 1
 - - Input: polyListToPoly (PolyList [1,2,3,4])
 - - Expected Output: Sum (Sum (Sum (Prod (Coef 1) (Coef 1)) (Prod X (Prod (Coef 1) (Coef 2)))) (Prod X (Prod X (Prod (Coef 1) (Coef 3))))) (Prod X (Prod X (Prod X (Prod (Coef 1) (Coef 4)))))
 - - Actual Output: Sum (Sum (Sum (Prod (Coef 1) (Coef 1)) (Prod X (Prod (Coef 1) (Coef 2)))) (Prod X (Prod X (Prod (Coef 1) (Coef 3))))) (Prod X (Prod X (Prod X (Prod (Coef 1) (Coef 4)))))
 - -----------------------------------------------------------------
 - - Function: polyListToPoly
 - - Test Case Number: 2
 - - Input: polyListToPoly  (PolyList [0,0])
 - - Expected Output: Sum (Prod (Coef 1) (Coef 0)) (Prod X (Prod (Coef 1) (Coef 0)))
 - - Actual Output: Sum (Prod (Coef 1) (Coef 0)) (Prod X (Prod (Coef 1) (Coef 0)))
 - -----------------------------------------------------------------
 - - Function: polyListToPoly
 - - Test Case Number: 3
 - - Input: polyListToPoly  (PolyList [4,3,2])
 - - Expected Output: Sum (Sum (Prod (Coef 1) (Coef 4)) (Prod X (Prod (Coef 1) (Coef 3)))) (Prod X (Prod X (Prod (Coef 1) (Coef 2))))
 - - Actual Output: Sum (Sum (Prod (Coef 1) (Coef 4)) (Prod X (Prod (Coef 1) (Coef 3)))) (Prod X (Prod X (Prod (Coef 1) (Coef 2))))

 -}