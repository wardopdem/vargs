{-# LANGUAGE TemplateHaskell, ExtendedDefaultRules, FlexibleInstances, GADTs #-} 

import Data.Function.Vargs

data Expr = Num Double | Op (Double -> Double -> Double)

workBPN' :: [Expr] -> Double
workBPN' [Num x] = x
workBPN' (Op o : Num x : Num y : r) = workBPN' $  (Num $ o x y) : r

$( return [] )

defVargsFun "workBPN" 'workBPN' 
    (''Integer, [| Num . fromIntegral |])
    ([t| Double -> Double -> Double |], [| Op |])

main = do
    print $ workBPN' [Op (*), Num 5, Num 5]
    print $ workBPN (**)  5 5

