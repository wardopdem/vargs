{-# LANGUAGE TemplateHaskell, FlexibleInstances, ExtendedDefaultRules, ExistentialQuantification #-} 

import Data.Function.Vargs

type FmtRes = (String, String)

class PfVal a where
    doFmt :: FmtRes -> a -> FmtRes

instance PfVal Integer where
    doFmt (fmt, res) x = 
        let (b, s) = span (/= '%') fmt
        in  (res ++ (tail . tail $ s), b ++ show x)

instance PfVal Double where
    doFmt (fmt, res) x = 
        let (b, s) = span (/= '%') fmt
        in  (res ++ (tail . tail $ s), b ++ show x)

instance PfVal String where
    doFmt (fmt, res) x = 
        let (b, s) = span (/= '%') fmt
        in  (res ++ (tail . tail $ s), b ++ x)

data PfValWrap = forall a. PfVal a => Val a

printf_IO :: String -> [PfValWrap] -> String
printf_IO fmt vs = 
    uncurry (flip (++)) $ foldl step (fmt, "") vs where step fmt (Val f) = doFmt fmt f

printf_String :: String -> [PfValWrap] -> IO ()
printf_String fmt = putStrLn . printf_IO fmt

$( return [] )

defVargsFun "printf" ['printf_IO, 'printf_String] [''Integer, ''Double, ''String]

main :: IO ()
main = do
    let fmt = "Number one is %d, number two is %f and string is \"%s\""
    putStrLn $ printf_IO fmt [Val 100, Val 123.456, Val "ok"]
    putStrLn $ printf fmt 100 123.456 "ok"
    printf fmt 100 123.456 "ok"
