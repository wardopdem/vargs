{-# LANGUAGE TemplateHaskell, FlexibleInstances, ExtendedDefaultRules, ExistentialQuantification #-} 

import Data.Function.Vargs

class PfVal a where
    doFmt :: String -> a -> (String, String)

instance PfVal Integer where
    doFmt fmt x = 
        let (b, s) = span (/= '%') fmt
        in  (b ++ show x, tail . tail $ s)

instance PfVal Double where
    doFmt fmt x = 
        let (b, s) = span (/= '%') fmt
        in  (b ++ show x, tail . tail $ s)

instance PfVal String where
    doFmt fmt x = 
        let (b, s) = span (/= '%') fmt
        in  (b ++ x, tail . tail $ s)

data PfValWrap = forall a. PfVal a => Val a

sprintf' :: String -> [PfValWrap] -> String
sprintf' fmt vs = 
    uncurry (flip (++)) $ foldl step (fmt, "") vs
        where step :: (String, String) -> PfValWrap -> (String, String)
              step (fmt, r) (Val f) = let (r', fmt') = doFmt fmt f in (fmt', r ++ r')

$( return [] )

defVargsFun "sprintf" 'sprintf' [''Integer, ''Double, ''String]

main = do
    return()
    putStrLn $ sprintf' "Number one is %d, number two is %f and string is \"%s\"" [Val 100, Val 123.456, Val "ok"]
    putStrLn $ sprintf "Number one is %d, number two is %f and string is \"%s\"" 100 123.456 "ok"
