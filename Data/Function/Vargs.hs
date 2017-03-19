{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

{- |
Module      : Data.Function.Vargs
Description : Support variable number of function parameters
Copyright   : (c) Wardopdem 2017                  
License     : GPL-3
Maintainer  : wardopdem@gmail.com                 
Stability   : experimental
Portability : non-portable (GHC extesions)

Types and functions for gereration declarations 
for realizations variable number of function parameters.
-}

module Data.Function.Vargs (
    -- * Функция для реализации переменнго числа параметров.
    defVargsFun,
    -- * Классы и типы для реализации механизма переменного числа параметров
    InstMaker, InstMakerQ, InstSrc(..),  ArgProc(..), Genz
) where 

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import qualified Data.Map as M

-- | Генератор экземпляров
type InstMaker = Type -> Type ->  Name -> DecsQ   
-- ^ * :: класс 
-- * -> тип 
-- * -> метод 
-- * -> декларации экземпляров

type InstMakerQ = Q InstMaker

-- | Источник генератора экземпляров - тип, на основе которого можно построить процедуру генерации экземпляров
class InstSrc a where
    -- | Создание генератора экземпляров класса типов элементов списка парметров 
    --  для типа @a@
    toMaker :: a -> InstMakerQ -- ^ генератор экземпляров для заданного типа

mkTpHndl :: Cxt -> Type -> Exp -> InstMakerQ
mkTpHndl c t e = return $ \cnt_as cnt_at mt -> do
    return [InstanceD Nothing c (AppT cnt_as t) [ValD (VarP mt) (NormalB e) []]]

-- | Для пар вида: @(''Type,  [| doIt |])@
instance InstSrc (Name, ExpQ) where
    toMaker (n, e) = do 
        e' <- e 
        mkTpHndl [] (ConT n) e'

-- | Для пар вида: @([t| [Int] |],  [| doIt |])@ 
instance InstSrc (TypeQ, ExpQ) where
    toMaker (t, e) = do 
        t' <- t 
        e' <- e 
        mkTpHndl [] t' e'

-- | Комбинатор генераторов экземпляров: строит новый генератор, 
-- который выполняет заданные списком генераторы и объединяет их результаты 
concIms :: [InstMaker] -> InstMakerQ
concIms ims =  
    return $ \cnt_as cnt_at mt -> do
        decs <- sequenceQ $ map (\i -> i cnt_as cnt_at mt) ims 
        return $ concat decs
               
-- | Для пар вида: @([''Type1, ''Type2, ...],  [| doIt |])@ 
instance InstSrc ([Name], ExpQ) where
    toMaker (ns, e) = do 
        e' <- e
        ims <- sequenceQ [mkTpHndl [] (ConT n) e' | n <- ns]
        concIms ims

-- | Для списка имён типов вида @[''Integer, ''Double, ...]@.
-- Подразумевается, что тип списка параметров имеет 
-- единственный конструктор с единственным аргументом (тип-обёртка) - этот конструктор 
-- и используется в качестве "тела" метода создания значений из исходных типов
instance InstSrc [Name] where
    toMaker ns = do
        ims <- sequenceQ [mkTpHndl' (ConT n) | n <- ns]
        concIms ims
            where mkTpHndl' t = return $ \cnt_as cnt_at mt -> do
                    wc  <- wrapCons cnt_at  
                    return [InstanceD Nothing [] (AppT cnt_as t) [ValD (VarP mt) (NormalB wc) []]]
                  wrapCons (ConT cn) = do
                    TyConI (DataD _ _ _ _ [ForallC _ _ (NormalC cn' _)] _) <- reify cn
                    conE cn'

-- | Обёртка для декларации необходимости генерации "обобщённого" экзепляра вида 
--
-- @a ~ T1, b ~ T2 => C a b ...@
newtype Genz = Genz TypeQ

-- | Для пар вида  (Genz [t| ... |], [| expr |]).
-- Строит генератор экземпляров вида 
--  @a ~ T1, b ~ T2, ... => C a b ...@
-- для типов вида  Tp T1 T2 ... (например Double -> Double -> Double)
instance InstSrc (Genz, ExpQ) where
    toMaker (Genz tq, e) = do 
        t <- tq 
        e' <- e
        let (ctx, t') = genzType t 
        mkTpHndl ctx t' e'
            where
              -- | Генерация для типа вида: 
              --  
              --    Q T1 T2 ... 
              --
              --   пары вида (псевдокод):
              -- 
              --  ([a ~ T1, b ~ T2, ... ], Q a b ...)
              --
              -- т.е. контекста типов и полиморфного типа в этом контексте.
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

-- | Обработчик переменного числа параметров функции defVargsFun
class ArgProc a where
    -- | Реализация механизма накопления/обработки параметров (InstMakerQ)
    prc :: String       -- ^ Имя создаваемой функции-обёртки над @prc@.     
        -> [Name]       -- ^ Список имён функций, которые будут использованы в терминальных 
                        -- экземплярах (отдельный экземпляр для каждой функции). Каждая из функций должна иметь 
                        -- идентичный набор типов параметров и уникальный тип возврата.
        -> [InstMakerQ] -- ^ Поставщики информации для генераторов экземпляров. 
        -> a           

-- | терминальный обработчик - генератор всех необходимых деклараций
instance ArgProc DecsQ where
    prc = defVargsFun'

-- | "Магия" переменного числа параметров
instance (InstSrc a, ArgProc r) => ArgProc (a -> r) where
    -- | Накапливаем генераторы экземпляров для типов, допускающих конвертацию 
    -- в тип параметра функции с переменным числом параметров
    prc fn e sts = prc fn e . (: sts) . toMaker

-- | Генератор деклараций для реализации переменного числа 
-- параметров - основная экспортируемая функция (с переменным числом параметров).
defVargsFun :: ArgProc a => String -> [Name] -> a
defVargsFun fn e  = prc fn  e []

-- | Фактическая  генерация деклараций для реализации переменного числа параметров
defVargsFun' :: String        -- ^ Имя для создаваемой функци с переменным числом параметров (далее, Функция).
             -> [Name]        -- ^ Список имён функций, которые быдут выступать в качестве обработчиков параметров.
                              -- Каждая из функций должна иметь идентичный набор типов параметров и уникальный тип возврата.
             -> [InstMakerQ]  -- ^ Список генераторов экземпляров для "неявного приведения" заданных типов в классу параметров 
                              -- создаваемой функции
             -> DecsQ         -- ^ Список необходимых деклараций классов, экземпляров и функций
defVargsFun' fn srcFns sts = do
    -- Генерация имён необходимых классов и методов
    argPrc <- nn "ArgPrc" -- Основной класс для обраобтки переменного числа параметров (далее, Класс).
    argSrc <- nn "ArgSrc" -- Класс для типов, допускающихся в качестве параметров Функции (далее, Источник).
    prc    <- nn "prc"    -- Ключевой (и единственный) метод Класса.
    acc    <- nn "acc"    -- Параметр-накопитель (список) параметров.
    toArg  <- nn "toArg"  -- Ключевой (и единственный) метод Источника: создание значения 
                          -- типа Элемент (см. ниже) из значения типа класса Источник
        -- Для первой функции - остальные должны отличаться только типом возврата
    funs@((t,      -- исходный тип функции с типом возврата заменённым на @a@                                              
           t',     -- исходный тип функции без последнего параметра (списка) и с типом возврата заменённым на @a@
           rt,     -- результирующий тип функции                                                                             
           cnt_at, -- тип элементов списка для типа последнего параметра (далее, Элемент)
           nms     -- список имён параметров вида a1, a2, ... 
           ) : _) <- sequenceQ $ map splitType srcFns -- Разбираем типы функций - финальных обработчиков параметров
    -- Используемые типы
    let cnt_a  = ConT nm_a
        vrt_a  = VarT nm_a
        vrt_r  = VarT nm_r
        cnt_ap = ConT argPrc
        cnt_as = ConT argSrc
    -- Извлекаем генераторы из монады Q
    sts'  <- sequenceQ sts 
    -- Запускаем генераторы для построения деклараций экземпляров класса Источник
    insts <- sequenceQ $ map (\i -> i cnt_as cnt_at toArg) sts' 
    -- Возвращаем список всех необходимых деклараций
    return $ -- Класс
             [cls argPrc [SigD prc t]] ++
             -- Класс для типов, допускающихся в качестве параметров Функции (далее, Источник)
             [cls argSrc [SigD toArg (AppT (AppT ArrowT vrt_a) cnt_at)]] ++
             -- Экземпляры Класса для терминальных типов: ключевой метод (prc_xxx) вызывает одну из 
             -- функций srcFns (туб тип возврата которой фигурирует в экземпляре)
             [inst [] (AppT cnt_ap rt') 
                    [FunD prc [Clause (map VarP nms')
                        (NormalB $ conv [foldl AppE (VarE srcFn) (map VarE nms'), VarE 'reverse]) []]] 
                | (srcFn, (_, _, rt', _, nms')) <- zip srcFns funs] ++
             -- Экземпляр Класса для "магии" накопления параметров типов класса Источник
             [inst [AppT cnt_as vrt_a, AppT cnt_ap vrt_r]  (AppT cnt_ap (AppT (AppT ArrowT vrt_a) vrt_r))  
                    [FunD prc [Clause (map VarP nms ++ [VarP acc])
                        (NormalB $ conv [foldl AppE (VarE prc) (map VarE nms), conv [consTo acc, VarE toArg]]) []]]] ++
             -- Экземпляр класса Источник для самого типа Элемент
             [inst [] (AppT cnt_as cnt_at) [ValD (VarP toArg) (NormalB (VarE 'id)) []]] ++
             -- Экземпляры класса Источник для всех запрошенных типов
             concat insts ++
             -- Декларация и определение Функции
             [SigD nm $ ForallT [ptv_a] [AppT cnt_ap vrt_a] t',
              FunD nm [Clause (map VarP nms) (NormalB $ foldl1 AppE ([VarE prc] ++ map VarE nms ++ [ListE []])) []]] 

        where nn pref = newName $ pref ++ "_"  ++ fn
              -- Cтроит из списка [Exp] конвеер: exp1 . exp2 . ...
              conv    = foldr1 $ \x r -> InfixE (Just x) (VarE '(.)) (Just r) 
              cls c   = ClassD [] c [ptv_a] []
              inst    = InstanceD Nothing 
              consTo  = InfixE Nothing (ConE '(:)) . Just . VarE 
              nm      = mkName fn
              nm_a    = mkName "a"
              ptv_a   = PlainTV nm_a
              nm_r    = mkName "r"
              -- Разбиение типа функции вида T1 -> T2 -> ... -> [A] -> R на составные элементы, генерация вспомогательных имён
              splitType 
                  :: Name                               -- ^ Имя функции
                  -> Q (Type, Type, Type, Type, [Name]) 
                  -- ^ * исходный тип функции с типов возврата заменённым на @a@
                  -- * исходный тип функции без последнего параметра (массива) и с типов возврата заменённым на @a@
                  -- * результирующий тип функции
                  -- * тип элементов списка для типа последнего параметра: тип A для функци вида T1 -> T2 -> ... -> [A] -> R
                  -- * список имён параметров вида a1, a2, ... количеством равным число параметов в исходном типе-функции - 1
                  --
                  -- Пример:
                  --
                  --  для функции типа: Int -> String -> [Double] -> IO () 
                  --  получим: ( Int -> String -> [Double] -> a, Int -> String -> a, R, A. ['a1, 'a2] )
              splitType nm = do
                  VarI _ rt _ <- reify nm
                  let tps = funTypes rt
                      t   = mkType . init $ tps
                      t'  = mkType . init . init $ tps
                  return (t, t', last tps, 
                          let AppT ArrowT (AppT ListT at) = last . init $ tps in at, -- выделяем тип элемента списка
                          map mkName $ ["a" ++ show i | i <- [1 .. length tps - 2]]) 
                      where -- | Собираем обратнофункциональный тип из сипска типов элементов и результата (обратная funTypes).
                            --
                            -- Пример (псевдокод): mkType [a, b-> c, [d], r] ==> a -> (b -> c) -> [d] -> r
                            mkType = foldr1 AppT . (++ [VarT $ mkName "a"])
                            -- | Разбиваем тип функции на типы апраметров и результата.
                            --
                            -- Пример (псевдокод): funTypes a -> (b -> c) -> [d] -> r ==> [a, b-> c, [d], r]
                            funTypes (AppT a@(AppT ArrowT _) x) = a : funTypes x
                            funTypes x = [x]
