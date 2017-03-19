{-# LANGUAGE TemplateHaskell, ExtendedDefaultRules, FlexibleInstances #-} 

import Data.Function.Vargs

--tester' :: Show a => a -> [String] -> IO ()
tester' q (x : y : z) =  
    putStrLn $ "Fixed parameter " ++ q ++ ", next x = " ++ x ++ " and y = " ++ y 
               ++ " and rest = " ++ show z

$( return [] )

defVargsFun "tester" ['tester']
        (''Integer,     [| ("Int " ++) . show |])
        (''(),          [| const "NULL" |])
        ([t| [Int] |],  [| ("[Int] " ++) . show |])
        ([t| [Double] |],  [| ("[Double] " ++) . show |])

main :: IO()
main = do
    tester "<const>" "qwe" 100500 () [1::Int,2,3] [4::Double,5,6] 

