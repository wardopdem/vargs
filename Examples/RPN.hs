{-# LANGUAGE TemplateHaskell, FlexibleInstances, ExtendedDefaultRules, GADTs #-} -- 

import Data.Function.Vargs

data Expr = Num Double | Op (Double -> Double -> Double)

workBPN' :: [Expr] -> Double
workBPN' = head . foldr rpnStep []

rpnStep :: Expr -> [Double] -> [Double]
rpnStep (Num n) stack = n : stack
rpnStep (Op f) (x:y:stack) = (f x y) : stack

$( return [] )

defVargsFun "workBPN" 'workBPN' 
    (''Integer, [| Num . fromIntegral |])
    (''String,  [| Num . fst . head . (reads :: ReadS Double) |])
    (Genz [t| Double -> Double -> Double |], [| Op |])
    ([t| [Double] |], [| Num . sum |])

main = do
    print $ workBPN' [Op (*), Num 7, Op (*), Num 5, Num 5]
    print $ workBPN (*) 7 (*) [1::Double, 2, 3] "5"

