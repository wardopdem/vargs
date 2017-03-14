{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

module Data.Function.Vargs where 

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

class ArgSrc a where
    toArg :: a -> Q (Type, Exp)

instance ArgSrc (Name, ExpQ) where
    toArg (n, e) = do e' <- e; return (ConT n, e')

instance ArgSrc (TypeQ, ExpQ) where
    toArg (t, e) = do t' <- t; e' <- e; return (t', e')  

class ArgProc a where
    prc :: String -> Name -> [Q (Type, Exp)] -> a

instance ArgProc DecsQ where
    prc = defVargsFun'

instance (ArgSrc a, ArgProc r) => ArgProc (a -> r) where
    prc fn e sts = prc fn e . (: sts) . toArg

defVargsFun' :: String -> Name -> [Q (Type, Exp)] -> DecsQ
defVargsFun' fn srcFn sts = do
    argPrc <- nn "ArgPrc"
    argSrc <- nn "ArgSrc"
    prc    <- nn "prc"
    acc    <- nn "acc"
    toArg  <- nn "toArg"
    sts'   <- sequenceQ sts
    (t, t', rt, cnt_at, nms) <- splitType srcFn
    let ptv_a  = PlainTV $ mkName "a"
        cnt_a  = ConT nm_a
        vrt_a  = VarT nm_a
        vrt_r  = VarT nm_r
        cnt_ap = ConT argPrc
        cnt_as = ConT argSrc
        nm     = mkName fn

    return $ [cls argPrc [ptv_a] [] [SigD prc t ]] ++

             [cls argSrc [ptv_a] [] [SigD toArg (AppT (AppT ArrowT vrt_a) cnt_at)]] ++

             [inst [] (AppT cnt_ap rt) 
                    [FunD prc [Clause (map VarP nms)
                        (NormalB $ conv [foldl AppE (VarE srcFn) (map VarE nms), VarE 'reverse]) []]]] ++

             [inst [AppT cnt_as vrt_a, AppT cnt_ap vrt_r]  (AppT cnt_ap (AppT (AppT ArrowT vrt_a) vrt_r))  
                    [FunD prc [Clause (map VarP nms ++ [VarP acc])
                        (NormalB $ conv [foldl AppE (VarE prc) (map VarE nms), conv [consTo acc, VarE toArg]]) []]]] ++


             [inst [] (AppT cnt_as cnt_at) [ValD (VarP toArg) (NormalB (VarE 'id)) []]] ++

             [inst [] (AppT cnt_as t) [ValD (VarP toArg) (NormalB e) []] | (t, e) <- sts'] ++

             [SigD nm $ ForallT [ptv_a] [AppT cnt_ap vrt_a] t',
              FunD nm [Clause (map VarP nms) (NormalB $ foldl1 AppE ([VarE prc] ++ map VarE nms ++ [ListE []])) []]] 

        where nn pref = newName $ pref ++ "_"  ++ fn
              conv    = foldr1 $ \x r -> InfixE (Just x) (VarE '(.)) (Just r) -- строит из списка [Exp] конвеер: exp1 . exp2 . ...
              cls     = ClassD []  
              inst    = InstanceD Nothing 
              consTo  = InfixE Nothing (ConE '(:)) . Just . VarE 
              nm_a    = mkName "a"
              nm_r    = mkName "r"

defVargsFun :: ArgProc a => String -> Name -> a
defVargsFun fn e  = prc fn  e []

funTypes :: Type -> [Type]
funTypes (AppT a@(AppT ArrowT _) x) = a : funTypes x
funTypes x = [x]

splitType 
    :: Name             
    -> Q (Type,         
          Type,         
          Type, 
          Type, [Name]
         )
splitType nm = do
    VarI _ tp _ <- reify nm
    let (rt, ctx) = case tp of
                ForallT _ c t' -> (t', Just c)
                t' -> (t', Nothing)
        tps = funTypes rt
        t =  mkType $ init tps ++ [VarT $ mkName "a"]
        t' = mkType $ (init . init $ tps) ++ [VarT $ mkName "a"]
    return (mbWithCtx t ctx, mbWithCtx t' ctx, last tps, 
            let AppT ArrowT (AppT ListT at) = last . init $ tps in at,
            map mkName $ ["a" ++ show i | i <- [1 .. length tps - 2]])

        where mkType = foldr1 AppT
              mbWithCtx t ctx = maybe t (\c -> ForallT [] c t) ctx