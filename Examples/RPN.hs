{-# LANGUAGE TemplateHaskell, FlexibleInstances, ExtendedDefaultRules, GADTs #-} -- 

import Data.Function.Vargs

data Expr = Num Double | Op (Double -> Double -> Double)

calcRPN' :: [Expr] -> Double
calcRPN' = head . foldr rpnStep [] . reverse

rpnStep :: Expr -> [Double] -> [Double]
rpnStep (Num n) stack = n : stack
rpnStep (Op f) (x:y:stack) = (f x y) : stack

-- $( return [] )

defVargsFun "calcRPN" ['calcRPN']
    (''Integer, [| Num . fromIntegral |])
    (''String,  [| Num . fst . head . (reads :: ReadS Double) |])
    (Genz [t| Double -> Double -> Double |], [| Op |])
    ([t| [Double] |], [| Num . sum |])

main = do
    print $ calcRPN' [Num 5, Num 5, Op (*)]
    print $ calcRPN [1::Double,2,3] "5" (+) 7 (*) -- [1::Double,2,3]
    print $ calcRPN 5 8 (*) 2 (+)
