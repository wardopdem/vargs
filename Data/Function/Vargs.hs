{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

module Data.Function.Vargs where 

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import qualified Data.Map as M
import Control.Monad(when)
import Data.Maybe

{------ ArgSrc ----}

type InstMaker = Type -> Type -> Name -> DecsQ
type InstMakerQ = Q InstMaker

class ArgSrc a where
    toArg :: a -> InstMakerQ

mkTpHndl :: Cxt -> Type -> Exp -> InstMakerQ
mkTpHndl c t e = return $ \cnt_as cnt_at mt -> do
    return [InstanceD Nothing c (AppT cnt_as t) [ValD (VarP mt) (NormalB e) []]]

instance ArgSrc (Name, ExpQ) where
    toArg (n, e) = do 
        e' <- e 
        mkTpHndl [] (ConT n) e'

instance ArgSrc (TypeQ, ExpQ) where
    toArg (t, e) = do 
        t' <- t 
        e' <- e 
        mkTpHndl [] t' e'

concIms :: [InstMaker] -> InstMakerQ
concIms ims =  
    return $ \cnt_as cnt_at mt -> do
        decs <- sequenceQ $ map (\i -> i cnt_as cnt_at mt) ims 
        return $ concat decs
               
instance ArgSrc ([Name], ExpQ) where
    toArg (ns, e) = do 
        e' <- e
        ims <- sequenceQ [mkTpHndl [] (ConT n) e' | n <- ns]
        concIms ims

instance ArgSrc [Name] where
    toArg ns = do
        ims <- sequenceQ [mkTpHndl' (ConT n) | n <- ns]
        concIms ims
            where mkTpHndl' t = return $ \cnt_as cnt_at mt -> do
                    wc  <- wrapCons cnt_at  
                    return [InstanceD Nothing [] (AppT cnt_as t) [ValD (VarP mt) (NormalB wc) []]]
                  wrapCons (ConT cn) = do
                    TyConI (DataD _ _ _ _ [ForallC _ _ (NormalC cn' _)] _) <- reify cn
                    conE cn'

newtype Genz = Genz TypeQ

instance ArgSrc (Genz, ExpQ) where
    toArg (Genz tq, e) = do 
        t <- tq 
        e' <- e
        let (ctx, t') = genzType t 
        mkTpHndl ctx t' e'

{------ ArgProc ----}

class ArgProc a where
    prc :: String -> Name -> [InstMakerQ] -> a

instance ArgProc DecsQ where
    prc = defVargsFun'

instance (ArgSrc a, ArgProc r) => ArgProc (a -> r) where
    prc fn e sts = prc fn e . (: sts) . toArg

{------- defVargsFun -------}

defVargsFun' :: String -> Name -> [InstMakerQ] -> DecsQ
defVargsFun' fn srcFn sts = do
    argPrc <- nn "ArgPrc"
    argSrc <- nn "ArgSrc"
    prc    <- nn "prc"
    acc    <- nn "acc"
    toArg  <- nn "toArg"
    (t, t', rt, cnt_at, nms) <- splitType srcFn

    let cnt_a  = ConT nm_a
        vrt_a  = VarT nm_a
        vrt_r  = VarT nm_r
        cnt_ap = ConT argPrc
        cnt_as = ConT argSrc

    sts'  <- sequenceQ sts 
    insts <- sequenceQ $ map (\i -> i cnt_as cnt_at toArg) sts' 

    return $ [cls argPrc [ptv_a] [] [SigD prc t ]] ++

             [cls argSrc [ptv_a] [] [SigD toArg (AppT (AppT ArrowT vrt_a) cnt_at)]] ++

             [inst [] (AppT cnt_ap rt) 
                    [FunD prc [Clause (map VarP nms)
                        (NormalB $ conv [foldl AppE (VarE srcFn) (map VarE nms), VarE 'reverse]) []]]] ++

             [inst [AppT cnt_as vrt_a, AppT cnt_ap vrt_r]  (AppT cnt_ap (AppT (AppT ArrowT vrt_a) vrt_r))  
                    [FunD prc [Clause (map VarP nms ++ [VarP acc])
                        (NormalB $ conv [foldl AppE (VarE prc) (map VarE nms), conv [consTo acc, VarE toArg]]) []]]] ++

             [inst [] (AppT cnt_as cnt_at) [ValD (VarP toArg) (NormalB (VarE 'id)) []]] ++

             concat insts ++

             [SigD nm $ ForallT [ptv_a] [AppT cnt_ap vrt_a] t',
              FunD nm [Clause (map VarP nms) (NormalB $ foldl1 AppE ([VarE prc] ++ map VarE nms ++ [ListE []])) []]] 

        where nn pref = newName $ pref ++ "_"  ++ fn
              conv    = foldr1 $ \x r -> InfixE (Just x) (VarE '(.)) (Just r) -- строит из списка [Exp] конвеер: exp1 . exp2 . ...
              cls     = ClassD []  
              inst    = InstanceD Nothing 
              consTo  = InfixE Nothing (ConE '(:)) . Just . VarE 
              nm      = mkName fn
              nm_a    = mkName "a"
              ptv_a   = PlainTV nm_a
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
--    VarI _ tp _ <- reify nm

    r <- reify nm

    let tp' = case r of  
                VarI _ tp _ -> Just tp
                _ -> Nothing
    
    when (isNothing tp' ) $ fail $ "\"" ++ nameBase nm ++ "\" is not a function!"
    let tp = fromJust tp'

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

genzType :: Type -> ([Type], Type)
genzType t@(ConT nm) = ([], t)
genzType tp =
    let (t, m, tvs) = work tp (M.empty :: M.Map Name String) tpVars 
    in (map mkCtx (M.assocs m), t)   
        where work (ConT nm) m tvs =
                case M.lookup nm m of
                    Just s -> (VarT $ mkName s, m, tvs)
                    Nothing -> let (s : tvs') = tvs 
                               in (VarT $ mkName s, M.insert nm s m, tvs')
              work (AppT t1 t2) m tvs = 
                let (t', m', tvs') = work t1 m tvs
                in  let (t'', m'', tvs'') = work t2 m' tvs'
                    in (AppT t' t'', m'', tvs'')
              work t m tvs = (t, m, tvs)
              mkCtx (nm, s) = AppT (AppT EqualityT (VarT $ mkName s)) (ConT nm)
              tpVars = map (:[]) ['a'..'z']  
