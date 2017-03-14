{-# LANGUAGE TemplateHaskell, ExtendedDefaultRules, FlexibleInstances #-} 

import Data.Function.Vargs

tester' q (x : y : z) =  putStrLn $ q ++ "x = " ++ x ++ " and y = " ++ y ++ " and rest = " ++ show z

$( return [] )

defVargsFun "tester" 'tester'
        (''Integer,     [| ("Int " ++) . show |])
        (''(),          [| const "NULL" |])
        ([t| [Int] |],  [| ("[Int] " ++) . show |])

main = do
    tester "abcdef" "qwe" 100500 () [1::Int,2,3] :: IO()

