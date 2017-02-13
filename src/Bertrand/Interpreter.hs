-- {-# LANGUAGE TupleSections #-}
-- {-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE LambdaCase #-}

module Bertrand.Interpreter
    (evalShow
    )where

import Control.Monad
import Control.Monad.Extra
import Control.Monad.Loops
-- import Control.Monad.Plus
import qualified Control.Monad.State as ST
import Data.Bool
import Data.Either
-- import Data.List
import Data.List.Extra
import qualified Data.Map as Map
import Data.Maybe
-- import Data.Monoid
import Data.Tree
-- import Data.Tuple

import Bertrand.Data

import Debug.Trace


--------------------------------------------------------------------------------
type Interpreter = ST.StateT [Envir] (Either IError)

type Evaluator a b = a -> Interpreter b

data IError = IError
    deriving (Eq, Show)

-- type Result = ([Expr], [Expr])
--
-- result :: [Expr] -> [Expr] -> Result
-- result ts ns = (ts, ns)
-- whnf :: Expr -> Result
-- whnf e = result [] [e]
-- thunks :: Result -> [Expr]
-- thunks = fst
-- whnfs :: Result -> [Expr]
-- whnfs = snd
-- rmap :: (Expr -> Expr) -> Result -> Result
-- rmap f (ts, ns) = (map f ts, map f ns)

--------------------------------------------------------------------------------
data Thunk a = Thunk a
             | WHNF a

instance Functor Thunk where
    f `fmap` Thunk a = Thunk $ f a
    f `fmap` WHNF a  = WHNF  $ f a

--------------------------------------------------------------------------------
-- data Result a = Result [OR (Result a)]
--               | Pure a
--
-- instance Monoid (Result a) where
--     mempty = Result []
--     ra `mappend` rb = case (toList ra, toList rb) of
--         ([], _)  -> rb
--         (_, [])  -> ra
--         (as, bs) -> Result $ as ++ bs
--
-- instance Functor Result where
--     f `fmap` Pure a    = Pure (f a)
--     f `fmap` Result os = Result $ map (fmap (fmap f)) os
--
-- -- instance Applicative Result where
-- --     pure = Result . pure . pure
-- --     Result fs <*> Result os = Result $ map (<*>) fs <*> os
--
-- instance Show a => Show (Result a) where
--     show (Pure a)   = show a
--     -- show (Result []) = "⊥"
--     show (Result os) = intercalate "<<" (map show os)
--
-- -- pureR :: OR a -> Result a
-- -- pureR = Result . pure
--
-- toList :: Result a -> (Result a)]
-- toList = \case
--     Pure a    -> [pureOR a]
--     Result os -> os
--
-- lastR :: Result a -> OR a
-- lastR = \case
--     Pure a       -> pureO a
--     -- Result []    -> Nothing
--     Result (o:_) -> fmap lastR o

-- filterR :: (a -> Bool) -> Result a -> Result a
-- filterR f = \case
--     Pure a    -> if f a then Pure a else mzero
--     Result os ->

--------------------------------------------------------------------------------

-- type Lazy = ST.StateT ([Expr], [Expr]) Interpreter
-- type Lazy = ST.StateT [Expr] Interpreter

-- type Result = [Thunk Expr]

evaluate :: Evaluator a b -> [Envir] -> a -> Either IError b
evaluate f s a = ST.evalStateT (f a) s
                --   in traceShow s' b

-- subState :: ST.State s a -> ST.State s a
-- subState f = ST.StateT $ \s -> let (_, a) = runState f s
--                            in  (s, a)
subEnvir :: (Int, Envir) -> Interpreter a -> Interpreter a
subEnvir (i, env) f = ST.StateT
    (\s -> let (xs, ys) = splitAtEnd i s
           in do
               (a, ys') <- ST.runStateT f (env:ys)
               return (a, xs ++ tail ys'))

envirs :: Interpreter [Envir]
envirs = ST.get

envirsNum :: Interpreter Int
envirsNum = (\xs -> length xs - 1) <$> ST.get

-- evalLazy :: Lazy a -> Expr -> Interpreter a
-- evalLazy f e = ST.evalStateT f ([e], [])
-- evalLazy f e = ST.evalStateT f [e]

-- evalCompleted :: Lazy Bool
-- evalCompleted = null <$> ST.get

-- partitionThunks :: [Thunk a] -> ([a], [a])
-- partitionThunks = foldr (\a (ts, ns) -> case a of
--                              Thunk a -> (a:ts, ns)
--                              WHNF a  -> (ts, a:ns) ) ([], [])
-- thunks :: [Thunk a] -> [a]
-- thunks = mapMaybe (\case Thunk a -> Just a
--                          WHNF _  -> Nothing)


-- modify :: (Envir -> Envir) -> Interpreter a ()
-- modify f _ = ST.modify (mapSnd f)

-- emapM :: Evaluator Expr Expr -> Evaluator Expr Expr
-- emapM f (Cons s as)= Cons s <$> mapM f as
-- emapM f (App a b) = do a' <- f a
--                        b' <- f b
--                        return $ App a' b'
-- emapM f (Lambda a b) = do a' <- f a
--                           b' <- f b
--                           return $ Lambda a' b'
-- emapM f (Bind s a)   = Bind s <$> f a
-- emapM f (Comma as)   = Comma <$> mapM f as
-- emapM f (Decl ds a)  = do ds' <- mapM f ds
--                           a' <- f a
--                           return $ Decl ds' a'
-- emapM f (Env r a)    = Env r <$> f a
-- emapM f a            = return a


-- new :: Interpreter Envir Ref
-- new env = do
--     m <- get
--     let (m', ref) = newMemory env m
--     put m'
--     return ref
--
-- read :: Interpreter Ref Envir
-- read ref = do
--     m <- get
--     return $ readMemory ref m
--
-- write :: (Envir -> Envir) -> Interpreter Ref ()
-- write f ref = do
--     m <- get
--     put $ writeMemory f ref m

--------------------------------------------------------------------------------

evalShow :: Expr -> String
evalShow e = traceShow e show $ evaluate evalAll [] (traceShowId $ envir 0 e)
-- evalShow :: Expr -> String
-- evalShow = let (m, ref) = singleton Root
--            in show . interpret (\e -> do
--                a <- envir ref e
--                traceShow (e, a) evalTo ref a) m
--             --    traceShow a return ()
--             --    as <- repeatM (evalStep ref) 5 a
--             --    return $ trace (unlines $ map show $ init as) last as) m
--
--            --

envir :: Int -> Expr -> Expr
envir i e = case e of
    (Decl [] a)  -> envir i a
    (Decl ds a)  -> Env (i, envir (i + 1) `fmapE` toEnvir ds) (envir (i + 1) a)
    -- (Decl ds a)  -> let (as, bs) = partitionBinds ds
    --                 in Env (i, Envir (map (envir (i + 1)) as)
    --                                  (binds $ map (envir (i + 1)) bs))
    --                        (envir (i + 1) a)
    (Lambda a b) -> Lambda (envir (i + 1) a) (envir (i + 1) b)
    _            -> emap (envir i) e

toEnvir :: [Expr] -> Envir
toEnvir es = let (as, bs) = partitionEithers $ map eitherBind es
             in  Envir as (binds bs)
    where
        eitherBind :: Expr -> Either Expr Expr
        eitherBind e = let (f, e') = detachEnv e
                     in case e' of
                         Bind _ _ -> Right $ emap f e'
                         _        -> Left e
        binds :: [Expr] -> Binds
        binds = foldr (\(Bind s e) -> Map.insertWith (++) s [e]) mempty

subEnv :: Functor f => Evaluator Expr (f Expr) -> Evaluator Expr (f Expr)
subEnv f = \case
    Env (i, _) (Env (j, env) a) | j <= i
            -> subEnv f $ Env (j, env) a
    Env t a -> fmap (Env t) <$> subEnvir t (f a)
    a       -> f a

evalAll :: Evaluator Expr Expr
evalAll e = do
    e' <- eval e
    evalF e'
    where
        evalF = \case
            Env t a -> Env t <$> subEnvir t (evalF a)
            Const s es -> Const s <$> mapM evalAll es
            Lambda a b -> Lambda  <$> evalAll a <*> evalAll b
            e          -> return e

eval :: Evaluator Expr Expr
eval e =
    -- do
   -- envs <- envirs
   -- (\e' -> trace (show e ++ "\n  => " ++ show e') e') <$>
    case e of
    -- Env (i, _) (Env (j, env) a) | j <= i
    --            -> eval $ Env (j, env) a
    Env t a    -> Env t <$> subEnvir t (eval a)
    Var s      -> bind s >>= maybe (return e) eval
    Const s es -> bind s >>= maybe (evalSystemF e)
                                   (\e' -> (\e'' -> foldl App e'' es) <$> eval e')

    App a b -> do
        -- envs <- envirs
        -- traceShow ("App", envs, a, b) return ()
        as <- commaToList <$> eval a
        cs <- mapM (evalSystemF . (`applyCons` b)) (filter isConst as)
        fs <- lambdaTree $ filter isLambda as
        ls <- applyTree (fs, b)
        let es = cs ++ ls
        -- traceShow ("App1", envs, as, fs) return ()
        -- envs <- envirs
        -- traceShow ("App2", envs, a, b, es) return ()
        if null cs && null fs
        then return e
        else eval $ Comma es

    Comma es -> do
        es' <- nub <$> mapM eval es
        return $ case es' of
            [a] -> a
            _   -> Comma es'

    _ -> return e

applyCons :: Expr -> Expr -> Expr
applyCons a b = let (f, Const s es) = detachEnv a
                in  Const s $ map f es ++ [b]

evalSystemF :: Evaluator Expr Expr
evalSystemF e = let (f, Const s es) = detachEnv e
                in case s of
                    '#':xs -> fromMaybe e <$>
                              Map.findWithDefault (const $ return Nothing) xs funcs es
                    _      -> return e
    where
        funcs :: Map.Map String (Evaluator [Expr] (Maybe Expr))
        funcs = Map.fromList
            [("intplus",  func2 (\x y -> Just $ Const (show $ x + y) [])),
             ("intminus", func2 (\x y -> Just $ Const (show $ x - y) [])),
             ("intmultiply", func2 (\x y -> Just $ Const (show $ x * y) []))
            ]

        func2 :: (Read a, Read b) => (a -> b -> Maybe Expr) -> Evaluator [Expr] (Maybe Expr)
        func2 f = \case
            [a, b] -> do
                (_, a') <- detachEnv <$> eval a
                (_, b') <- detachEnv <$> eval b
                return $ case (a', b') of
                    (Const as [], Const bs [])
                        -> case (readMaybe as, readMaybe bs) of
                               (Just x, Just y) -> f x y
                               _ -> Nothing
                    _ -> Nothing
            _ -> return Nothing

lambdaTree :: Evaluator [Expr] (Forest Expr)
-- lambdaTree es = return $ map (`Node` []) es
lambdaTree = foldM (curry addForest) []

addForest :: Evaluator (Forest Expr, Expr) (Forest Expr)
-- addForest = undefined
addForest (fe, e) = do
    (ls, rs)       <- partitionEithers <$> mapM
                      (\t@(Node a _) -> bool (Left t) (Right t) <$> imply (e, a)) fe
    (imps, eqs)    <- partitionEithers <$> mapM
                      (\t@(Node a _) -> bool (Left t) (Right t) <$> imply (a, e)) rs
    (uneqs, uimps) <- partitionEithers <$> mapM
                      (\t@(Node a _) -> bool (Left t) (Right t) <$> imply (a, e)) ls
    if not $ null eqs
    then let Node _ xs = head eqs
         in  return $ Node e xs : fe
    else if not $ null imps
    then (++ uneqs) <$> mapM (\(Node a xs) -> Node a <$> addForest (xs, e)) imps
    else return $ Node e uimps : uneqs

    where
        imply :: Evaluator (Expr, Expr) Bool
        imply (a, b) = let
            (fa, Lambda c _) = detachEnv a
            (fb, Lambda d _) = detachEnv b
            in isJust <$> match (fb d, fa c)

applyTree :: Evaluator (Forest Expr, Expr) [Expr]
applyTree (fs, e) = concat <$> mapM
    (\(Node a bs) ->
        applyLambda (a, e) >>=
        maybe (return [])
              (\x -> defaultl x <$> applyTree (bs, e))
    ) fs
    where
        defaultl :: a -> [a] -> [a]
        defaultl a [] = [a]
        defaultl _ as = as

applyLambda :: Evaluator (Expr, Expr) (Maybe Expr)
applyLambda (a, b) =
  --   do
  -- envs <- envirs
  -- (\m -> traceShow ("apply", envs, a, b, m) m) <$>
    case a of
    Env t@(i, _) a' -> do
        b' <- downEnv (i, b)
        fmap (Env t) <$> subEnvir t (applyLambda (a', b'))
    Lambda c d -> do
        i <- envirsNum
        m <- fmap toEnvir <$> match (c, b)
        return $ fmap (\env -> Env (i + 1, env) d) m
    where
        downEnv :: Evaluator (Int, Expr) Expr
        downEnv (i, e) = do
            envs <- reverse . dropEnd i . indexedR <$> envirs
            return $ foldr Env e envs

match :: Evaluator (Expr, Expr) (Maybe [Expr])
match (a, b) = do
    (fa, ea) <- detachEnv <$> eval a
    case ea of
        Var "_" -> return $ Just []
        Var s   -> do
            i <- envirsNum
            return $ if envirsNumOf i (fa ea) >= i
                     then Just [Bind s b]
                     else Just []
        Const xs as -> do
            (fb, eb) <- detachEnv <$> eval b
            case eb of
                Const ys bs | xs == ys && length as == length bs -> do
                    ms <- mapM match $ zip (map fa as) (map fb bs)
                    return $ if all isJust ms
                             then Just $ concat $ catMaybes ms
                             else Nothing
                Comma bs -> undefined
                _ -> return Nothing
        Comma as -> do
            ms <- mapM (\a' -> match (fa a', b)) as
            return $ if any isJust ms
                     then Just $ concat $ catMaybes ms
                     else Nothing
        App _ _ -> return Nothing
        _       -> return Nothing
    where
        envirsNumOf :: Int -> Expr -> Int
        envirsNumOf i = \case
            Env (j, _) e -> envirsNumOf (min i j) e
            _            -> i

bind :: Evaluator String (Maybe Expr)
bind s = do
    envs <- indexed . reverse <$> envirs
    return $ case concatMap
        (\(i, Envir as bs) ->
            map (Env (i, Envir as bs)) $ findBind s bs) envs of
        [] -> Nothing
        es -> Just $ Comma es

commaToList :: Expr -> [Expr]
commaToList = \case
    Env t e  -> map (Env t) $ commaToList e
    Comma es -> concatMap commaToList es
    e        -> [e]

detachEnv :: Expr -> (Expr -> Expr, Expr)
detachEnv = \case
    Env t a -> let (f, a') = detachEnv a
               in  (Env t . f, a')
    a       -> (id, a)

isLambda :: Expr -> Bool
isLambda e = case snd $ detachEnv e of
    Lambda _ _ -> True
    _          -> False

isConst :: Expr -> Bool
isConst e = case snd $ detachEnv e of
    Const _ _ -> True
    _         -> False

--
-- evalStep :: Evaluator Expr Expr
-- -- eval r (Var s)
-- evalStep r e =
--     (\e' -> trace (show r ++ "<" ++ show e ++ " => " ++ show e' ++ ">") e') <$>
--     case e of
--     -- Var s     -> commaWith e . concatMap (Map.findWithDefault [] s . binds) <$> envirs r
--     Var s -> commaWith e . concatMap ((\(r, es) -> map (Env r) es) . lookupName s)
--              <$> envirs r
--
--     -- App (Env r' a') b -> evalStep r' (App a' b)
--     App a b -> do
--          as <- toList <$> evalTillLambda r a
--          let cs = map (`appCons` b) (filter isCons as)
--          fs <- lambdaTree r $ filter isLambda as
--          ls <- applyTree r (fs, b)
--         --  ls    <- mapM (\l -> apply r (l, b)) (filter isLambda as)
--          return $ Comma $ cs ++ ls
--         --  return $ Comma $ map (`appCons` b)   (filter isCons as)
--         --                ++ map (`appLambda` b) (filter isLambda as)
--     -- --              a' <- evalTillLambda r a
--     -- --              case a' of
--     -- --                  Lambda c d -> match
--
--     Env _ (Env ref a) -> evalStep r $ Env ref a
--
--     Env ref a | r == ref  -> evalStep ref a
--               | otherwise -> Env ref <$> evalStep ref a
--
--     Cons ('#':s) es -> systemF s <$> mapM (evalTo r) es
--     Cons s es -> Cons s <$> mapM (evalStep r) es
--
--     Comma [e] -> evalStep r e
--     Comma es  -> Comma <$> mapM (evalStep r) (nub es)
--
--     _ -> return e
--
--     where
--         commaWith :: Expr -> [Expr] -> Expr
--         commaWith e []  = e
--         commaWith _ [e] = e
--         commaWith _ es  = Comma es
--
--         lookupName :: String -> (Ref, Envir) -> (Ref, [Expr])
--         lookupName s = mapSnd $ concat . Map.lookup s . binds
--
--         toList :: Expr -> [Expr]
--         toList (Env r e)  = Env r `map` toList e
--         toList (Comma es) = es
--         toList e          = [e]
--
--         -- hideEnv :: Expr -> Expr
--         -- hideEnv (Env r a) = hideEnv $ emap (Env r) a
--         -- hideEnv a = a
--
--         isLambda :: Expr -> Bool
--         isLambda (Env _ a)    = isLambda a
--         isLambda (Lambda _ _ _) = True
--         isLambda _            = False
--
--         isCons :: Expr -> Bool
--         isCons (Env _ a)  = isCons a
--         isCons (Cons _ _) = True
--         isCons _          = False
--
--         appCons :: Expr -> Expr -> Expr
--         appCons a b = let (f, Cons s as) = detachEnv a
--                       in  Cons s $ map f as ++ [b]
--         -- appCons (Cons s es) e = Cons s $ es ++ [e]
--
--         lambdaTree :: Evaluator [Expr] (Forest Expr)
--         lambdaTree r =
--                         -- (\as -> trace ("t:" ++ drawForest (map (fmap show) as)) as) <$>
--                          foldM (curry $ addForest r) []
--             where
--                 addForest :: Evaluator (Forest Expr, Expr) (Forest Expr)
--                 -- f :: Forest Expr -> Expr -> State (Memory Envir) (Forest Expr)
--                 addForest r (ts, e) = let
--                     (re, e') = detachRef r e
--                     in case e' of
--                         Lambda _ a _ -> do
--                             -- a == b, a /= b, a => b, a <= b
--                             (xs, ys, zs, ws) <- foldM
--                                 (\(xs, ys, zs, ws) t@(Node x _) ->
--                                     let (rx, Lambda _ b _) = detachRef r x
--                                         a' = Env re a
--                                         b' = Env rx b
--                                     in do
--                                         aimpb <- isJust <$> imply r (a', b')
--                                         bimpa <- isJust <$> imply r (b', a')
--                                         return $ case (aimpb, bimpa) of
--                                             (True,  True)  -> (t:xs, ys, zs, ws)
--                                             (False, False) -> (xs, t:ys, zs, ws)
--                                             (True,  False) -> (xs, ys, t:zs, ws)
--                                             (False, True)  -> (xs, ys, zs, t:ws)
--                                 ) ([], [], [], []) ts
--                             if not $ null xs
--                             then let Node _ xs' = head xs
--                                  in return $ Node e xs' : ts
--                             else if not $ null zs
--                             then (++ ys) <$> mapM (\(Node z zs') -> Node z <$> addForest r (zs', e)) zs
--                             else return $ Node e ws : ys
--
--                         _ -> return ts
--
--         applyTree :: Evaluator (Forest Expr, Expr) [Expr]
--         applyTree r (xs, e) = concat <$> mapM
--             (\(Node a bs) -> apply r (a, e) >>=
--                 traceShow ('@', r, a, e)
--                              maybe (return [])
--                                    (\x -> defaultl x <$> applyTree r (bs, e))
--             ) xs
--
--         -- applyTree :: Forest Expr -> Expr
--         -- applyTree = Comma . concatMap
--         --             (\(Node a xs) -> maybe [] (defaultl a $ applyTree xs) apply  )
--             where
--                 defaultl :: a -> [a] -> [a]
--                 defaultl a [] = [a]
--                 defaultl _ as = as
--
--         apply :: Evaluator (Expr, Expr) (Maybe Expr)
--         apply r (a, b) = let (r', Lambda re c d) = detachRef r a
--                         --  in fmap (maybe (Env r' d) (`Env` d)) <$> match r' (re, c, Env r b)
--                          in fmap (bool (Env r' d) (Env re d)) <$> match r' (re, c, Env r b)
--         -- apply r (a, b) = case a of
--         --     Env r' a'  -> apply r' (a', Env r b)
--         --     -- Lambda c d -> return (Just d)
--         --     Lambda c d -> fmap (maybe d (`Env` d)) <$> match r (c, b)
--
--         match :: Evaluator (Ref, Expr, Expr) (Maybe Bool)
--         match r (re, a, b) = do
--             -- traceShow (a, b) return ()
--             m <- imply r (b, a)
--
--             maybe (return Nothing) (\es ->
--                 case es of
--                     [] -> return $ Just False
--                     _  -> do
--                         write (\(Envir ref _ xs) -> traceShow ("E", xs) Envir ref [] (toBinds es)) re
--                         -- r' <- new (Envir r [] $ toBinds es)
--                         -- return $ Just $ Just r'
--                         return $ Just True
--                   ) m
--
--         -- a => b, a ⊆ b
--         imply :: Evaluator (Expr, Expr) (Maybe [Expr])
--         imply r t = let
--             (a, b) = t
--             (fa, a') = detachEnv a
--             (fb, b') = detachEnv b
--             in case (a', b') of
--             (_, App _ _) -> do
--                 (fb', b'') <- detachEnv <$> evalTo r b
--                 case b'' of
--                     App _ _ -> return Nothing
--                     _        -> imply r (a, fb' b'')
--
--             (_, Cons xs bs) -> do
--                 (fa', a'') <- detachEnv <$> evalTillCons r a
--                 case a'' of
--                     Cons ys as | xs == ys && length as == length bs -> do
--                         ms <- mapM (imply r) $ zip (map fa' as) (map fb bs)
--                         if Nothing `elem` ms
--                         then return Nothing
--                         else return $ Just $ concat $ catMaybes ms
--                     Comma as -> do
--                         ms <- mapM (imply r . (, b) . fa') as
--                         if all isJust ms
--                         then return $ Just $ concat $ catMaybes ms
--                         else return Nothing
--                     _ -> return Nothing
--
--             (Bytes i, Bytes j) | i == j -> return $ Just []
--
--             (_, Comma bs) -> do
--                 ms <- mapM (imply r . (, a) . fb) bs
--                 if any isJust ms
--                 then return $ Just $ concat $ catMaybes ms
--                 else return Nothing
--
--             (_, Var "_") -> return $ Just []
--             (_, Var s) -> return $ Just [Bind s a]
--
--             (x, y) -> trace ("failed:" ++ show (x, y)) return Nothing
--
--         evalTillTerm :: Evaluator Expr Expr
--         evalTillTerm r a = evalStep r a >>=
--             (\a' -> if a == a'
--                     then return a
--                     else case snd $ detachRef r a' of
--                         App _ _ -> evalTillTerm r a'
--                         _       -> return a )
--
--         evalTillCons :: Evaluator Expr Expr
--         evalTillCons r a = evalStep r a >>=
--             (\a' -> let (r', a'') = detachRef r a' in case a'' of
--                 Cons s _ | head s /= '#' -> return a''
--                 Comma as -> do
--                     as' <- mapM (evalTillCons r') as
--                     return $ Env r' $ case as' of
--                         [x] -> x
--                         _   -> Comma as'
--                 _ | a == a' -> return a
--                 _           -> evalTillCons r a')
--
--
--         detachRef :: Ref -> Expr -> (Ref, Expr)
--         detachRef _ (Env r a) = case a of
--             Env _ _ -> detachRef r a
--             _       -> (r, a)
--         detachRef r a = (r, a)
--
--         detachEnv :: Expr -> (Expr -> Expr, Expr)
--         detachEnv (Env r a) = case a of
--             Env _ _ -> detachEnv a
--             _       -> (Env r, a)
--         detachEnv a = (id, a)
--
--         ref :: Ref -> Expr -> Ref
--         ref r e = fst $ detachRef r e
--
--
--         evalTillLambda :: Evaluator Expr Expr
--         evalTillLambda r a = let (r', b) = detachRef r a in case b of
--             Var _ -> do
--                 a' <- evalStep r a
--                 if a == a' then return a'
--                            else evalTillLambda r a'
--             -- Env _ _ -> evalStep r e >>= evalTillLambda r
--             App _ _  -> evalStep r a >>= evalTillLambda r
--             Comma as -> do
--                 as' <- mapM (evalTillLambda r') as
--                 return $ Env r' $ Comma as'
--             _ -> return a
--
--         exprs :: Expr -> [Expr]
--         exprs (Comma es) = es
--         exprs (Env r a)  = Env r <$> exprs a
--         exprs a          = [a]
--
--         systemF :: String -> [Expr] -> Expr
--         systemF s [a, b] | s `elem` ss =
--             case (snd $ detachEnv a, snd $ detachEnv b) of
--                 (Cons "_Int" [Bytes i], Cons "_Int" [Bytes j])
--                                -> Cons "_Int" [Bytes $ (fromJust $ lookup s fs) i j]
--                 (Comma as, b') -> Comma $ map (\a' -> systemF s [a', b']) as
--                 (a', Comma bs) -> Comma $ map (\b' -> systemF s [a', b']) bs
--                 _              -> Comma []
--             where
--                 ss = map fst fs
--                 fs = [("intplus", (+)), ("intminus", (-)), ("intmultiply", (*))]
--
--         systemF s es = Cons ('#':s) es
--
-- envirs :: Interpreter Ref [(Ref, Envir)]
-- envirs r = read r >>= \e -> case e of
--     Root            -> return [(r, Root)]
--     Envir ref es bs -> ((r, e):) <$> envirs ref

--------------------------------------------------------------------------------

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)

tmap :: (a -> b) -> (a, a) -> (b, b)
tmap f (a, b) = (f a, f b)

may :: Maybe a -> b -> (a -> b) -> b
may m b f = maybe b f m

repeatM :: Monad m => (a -> m a) -> Int -> a -> m [a]
repeatM f i a
    | i <= 0 = return []
    | otherwise = do
        a' <- f a
        as <- repeatM f (i - 1) a'
        return $ a' : as

indexed :: [a] -> [(Int, a)]
indexed = zip [0..]

indexedR :: [a] -> [(Int, a)]
indexedR xs = zip (iterate (\i -> i - 1) (length xs - 1)) xs

readMaybe :: Read a => String -> Maybe a
readMaybe = fmap fst . listToMaybe . reads
