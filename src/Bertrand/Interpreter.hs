{-# LANGUAGE TupleSections #-}
-- {-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE LambdaCase #-}

module Bertrand.Interpreter
    (evalShow,
     reasonShow
    )where

import Control.Applicative
import Control.Monad
import Control.Monad.Extra
import Control.Monad.Free
-- import Control.Monad.Loops
-- import Control.Monad.Plus
import Control.Monad.Reader
-- import Control.Monad.State
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
-- Data types --

type Interpreter = Reader EnvirS

type EnvirS = ([Envir], [String])

sempty = ([], [])

-- data IError = EmptyError
--             | Paradox Expr Expr
--     deriving (Eq, Show)

type Evaluator a b = a -> Interpreter b

type Lazy a = Free [] a

thunks :: [Lazy a] -> Lazy a
thunks = Free


type Reasoner a b = EnvirS -> a -> Lazy b

--------------------------------------------------------------------------------
-- Interpreter --

evaluate :: Evaluator a b -> a -> b
evaluate f a = runReader (f a) sempty
                --   in traceShow s' b
evalWith :: EnvirS -> Evaluator a b -> a -> b
evalWith envs f a = runReader (f a) envs

subEnvir :: (Int, Envir) -> Interpreter a -> Interpreter a
subEnvir (i, env) = local (\(es, ss) -> let (xs, ys) = splitAtEnd i es
                                        in  (env:ys, ss))
-- subEnvir (i, env) f = reader
--     (\(es, ss) -> let (xs, ys) = splitAtEnd i es
--            in runReader f (env:ys, ss))
        --    in do
        --        (a, (ys', _)) <- runStateT f (env:ys, ss)
        --        return (a, (xs ++ tail ys', ss)))

-- subScope :: String -> Interpreter a -> Interpreter a
-- subScope s f = StateT
--     (\(es, ss) -> do
--                (a, (es', _)) <- runStateT f (es, s:ss)
--                return (a, (es', ss)))

envirs :: Interpreter [Envir]
envirs = fst <$> ask

-- varsInScope :: Interpreter [String]
-- varsInScope = snd <$> get

-- envirsWith :: (Expr -> Expr) -> Interpreter [Envir]
-- envirsWith f = envirsOf $ f (Var "")
--     where
--         envirsOf :: Expr -> Interpreter [Envir]
--         envirsOf = \case
--             Env t e  -> subEnvir t (envirsOf e)
--             Var "" -> envirs

envirsNum :: Interpreter Int
envirsNum = (\xs -> length xs - 1) <$> envirs

envir :: Expr -> Expr
envir = fst . setParamScope 0 . declToEnvir 0
    where
        declToEnvir :: Int -> Expr -> Expr
        declToEnvir i e = case e of
            (Decl [] a)  -> declToEnvir i a
            (Decl ds a)  -> Env (i, (Env (i + 1, eempty) . declToEnvir (i + 2)) `fmapE` toEnvir ds)
                                (declToEnvir (i + 1) a)
            -- (Decl ds a)  -> let (as, bs) = partitionBinds ds
            --                 in Env (i, Envir (map (envir (i + 1)) as)
            --                                  (binds $ map (envir (i + 1)) bs))
            --                        (envir (i + 1) a)
            (Lambda a b) -> Lambda (declToEnvir (i + 1) a) (declToEnvir (i + 1) b)
            _            -> emap (declToEnvir i) e


        -- setParam :: Expr -> Expr
        -- setParam = \case
        --     Env (j, env) e ->
        --         Env (j, (setParam . setParamScope j) `fmapE` env) (setParam $ setParamScope j e)
        --     e -> emap setParam e

        setParamScope :: Int -> Expr -> (Expr, [String])
        setParamScope i = \case
            Env (j, env) e
                       -> let (e', ss) = setParamScope i e
                              env' = fmapE (fst . setParamScope (j + 1) . setParam ss i) env
                              ss' = concatMap (snd . setParamScope (j + 1)) (toList env)
                          in (Env (j, env') e', ss')
            Param 0 s  -> (Param i s, [s])
            Cons s es  -> let ts = map (setParamScope i) es
                          in  (Cons s (map fst ts), concatMap snd ts)
            App a b    -> let (a', as) = setParamScope i a
                              (b', bs) = setParamScope i b
                          in  (App a' b', as ++ bs)
            Lambda a b -> let (a', as) = setParamScope i a
                              (b', bs) = setParamScope i b
                          in  (Lambda a' b', as ++ bs)
            Bind _ a   -> setParamScope i a
            Comma es   -> let ts = map (setParamScope i) es
                          in  (Comma (map fst ts), concatMap snd ts)
            e          -> (e, [])

        setParam :: [String] -> Int -> Expr -> Expr
        setParam ss i = \case
            Param 0 s | s `elem` ss
                -> Param i s
            e   -> emap (setParam ss i) e

        -- getParams :: Expr -> [String]
        -- getParams = \case
        --     Param _ s    -> [s]
        --     Cons _ es  -> concatMap getParams es
        --     App a b    -> getParams a ++ getParams b
        --     Lambda a b -> getParams a ++ getParams b
        --     Bind _ a   -> getParams a
        --     Comma es   -> concatMap getParams es
        --     Env _ a    -> getParams a
        --     _          -> []

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

detachEnv :: Expr -> (Expr -> Expr, Expr)
detachEnv = \case
    Env t a -> let (f, a') = detachEnv a
               in  (Env t . f, a')
    a       -> (id, a)

-- modify :: (Envir -> Envir) -> Interpreter a ()
-- modify f _ = ST.modify (mapSnd f)

--------------------------------------------------------------------------------
-- Show function --

evalShow :: Expr -> String
evalShow e = show $ evaluate evalAll (traceShowId $ envir e)

reasonShow :: Expr -> String
reasonShow e = traceShow e $ show $ evaluate boolean (traceShowId $ envir e)

evalAll :: Evaluator Expr Expr
evalAll e = do
    e' <- eval e
    evalF e'
    where
        evalF = \case
            Env t a -> Env t <$> subEnvir t (evalF a)
            Cons s es -> Cons s <$> mapM evalAll es
            Lambda a b -> Lambda  <$> evalAll a <*> evalAll b
            e          -> return e

--------------------------------------------------------------------------------
-- Evaluator --

eval :: Evaluator Expr Expr
eval e =
    -- do
   -- envs <- envirs
   -- (\e' -> trace (show e ++ "\n  => " ++ show e') e') <$>
    case e of
    -- Env (i, _) (Env (j, env) a) | j <= i
    --           -> eval $ Env (j, env) a
    Env t a   -> Env t <$> subEnvir t (eval a)
    Var s     -> bind s >>= maybe (return e) eval
    Param _ s -> bind s >>= maybe (return e) eval
    Cons s es -> bind s >>= maybe (evalSystemF e)
                                   (\e' -> (\e'' -> foldl App e'' es) <$> eval e')
    App a b   -> do
        -- envs <- envirs
        -- traceShow ("App", envs, a, b) return ()
        as <- commaToList <$> eval a
        cs <- mapM (evalSystemF . (`applyCons` b)) (filter isCons as)
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
applyCons a b = let (f, Cons s es) = detachEnv a
                in  Cons s $ map f es ++ [b]

evalSystemF :: Evaluator Expr Expr
evalSystemF e = let (f, Cons s es) = detachEnv e
                in case s of
                    '#':xs -> fromMaybe e <$>
                              Map.findWithDefault (const $ return Nothing) xs funcs es
                    _      -> return e
    where
        funcs :: Map.Map String (Evaluator [Expr] (Maybe Expr))
        funcs = Map.fromList
            [("intplus",  func2 (\x y -> Just $ Cons (show $ x + y) [])),
             ("intminus", func2 (\x y -> Just $ Cons (show $ x - y) [])),
             ("intmultiply", func2 (\x y -> Just $ Cons (show $ x * y) []))
            ]

        func2 :: (Read a, Read b) => (a -> b -> Maybe Expr) -> Evaluator [Expr] (Maybe Expr)
        func2 f = \case
            [a, b] -> do
                (_, a') <- detachEnv <$> eval a
                (_, b') <- detachEnv <$> eval b
                return $ case (a', b') of
                    (Cons as [], Cons bs [])
                        -> case (readMaybe as, readMaybe bs) of
                               (Just x, Just y) -> f x y
                               _ -> Nothing
                    _ -> Nothing
            _ -> return Nothing

lambdaTree :: Evaluator [Expr] (Forest Expr)
lambdaTree = foldM (curry addForest) []

addForest :: Evaluator (Forest Expr, Expr) (Forest Expr)
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

isLambda :: Expr -> Bool
isLambda e = case snd $ detachEnv e of
    Lambda _ _ -> True
    _          -> False

isCons :: Expr -> Bool
isCons e = case snd $ detachEnv e of
    Cons _ _ -> True
    _         -> False

--------------------------------------------------------------------------------
-- Pattern matching --

match :: Evaluator (Expr, Expr) (Maybe [Expr])
match (a, b) = do
    (fa, ea) <- detachEnv <$> eval a
    case ea of
        Var "_" -> return $ Just []
        Var s -> do
            i <- envirsNum
            return $ if envirsNumOf i (fa ea) >= i
                     then Just [Bind s b]
                     else Just []
        Param _ s -> do
            i <- envirsNum
            return $ if envirsNumOf i (fa ea) >= i
                     then Just [Bind s b]
                     else Just []
        Cons xs as -> do
            (fb, eb) <- detachEnv <$> eval b
            case eb of
                Cons ys bs | xs == ys &&
                              length as == length bs -> do
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

-- matchR :: Evaluator (Expr, Expr) Bool
-- matchR (a, b) = do
--     (fa, ea) <- detachEnv <$> eval a
--     (fb, eb) <- detachEnv <$> eval b
--     case (ea, eb) of
--         (Var "_", _) -> return False
--         (Var xs, Var ys) | xs == ys
--             -> return True
--         (Param xs, _)
--             -> do
--             es <- concatMap toExprs <$> envirs
--             ds <- concatMapM (`search` fa ea) es
--             and <$> mapM reason (ds <*> [fb eb])
--         (Cons xs as, Cons ys bs) | xs == ys &&
--                                    length as == length bs
--             -> and <$> mapM matchR (zip (map fa as) (map fb bs))
--         (App ax ay, App bx by)
--             -> (&&) <$> matchR (fa ax, fb bx) <*> matchR (fa ay, fb by)
--         _   -> return False

-- step :: (a -> Lazy b) -> Lazy a -> Lazy b
-- step f l = l >>= ()

--------------------------------------------------------------------------------
-- Reasoner --

-- infer :: Evaluator Expr Bool
-- infer a = reason' a >>= (\case
--     Pure a -> return a
--     Thunk ms -> foldReason ms )
--     where
--         foldReason :: [Interpreter (Lazy Bool)] -> Interpreter Bool
--         foldReason [] = return False
--         foldReason (m:ms) = m >>= (\case
--             Pure True  -> return True
--             Pure False -> foldReason ms
--             Thunk [] -> trace "No" (foldReason ms)
--             Thunk ms'  -> foldReason (ms ++ ms'))

infer :: Evaluator Expr Bool
infer e = do
    envs <- ask
    return $ case reason envs e of
        Pure a -> a
        Free s -> foldReason s
    where
        foldReason :: [Lazy Bool] -> Bool
        foldReason [] = False
        foldReason (l:ls) = case l of
            Pure True  -> True
            Pure False -> foldReason ls
            Free ls'   -> foldReason (nub $ ls ++ ls')

        -- foldReason :: [Lazy Bool] -> Bool
        -- foldReason = foldl (\t -> \case
        --     Pure t' -> t || t'
        --     Free ls -> t || foldReason ls) False

boolean :: Evaluator Expr Boolean
boolean e = do
    let (f, e') = detachEnv e
    t <- infer e
    if t then return T
         else do
            --  return U
         f <- infer $ f (App (Var "~") e')
         if f then return F
              else return U

reason :: Reasoner Expr Bool
reason envs e = traceShow ('R', e, toExprs envs) $ case e of
    Env t e -> reason (subEnvirR t envs) e
    e       -> thunks $ map (\d -> matchR envs (d, e))
                            (toExprs envs)

matchR :: Reasoner (Expr, Expr) Bool
matchR envs (a, b) = traceShow ('M', a, b) $ (\t -> traceShow (t, a, b) t) <$> let
    (fa, ea) = detachEnv $ evalWith envs eval a
    (fb, eb) = detachEnv $ evalWith envs eval b
    in case (ea, eb) of
        (Var "_", _)
            -> pure True
        (Var xs, Var ys) | xs == ys
            -> pure True
        (Param _ xs, Param _ ys) | xs == ys &&
                           xs `elem` varsInScope envs
            -> pure True
        (Param i xs, _) ->
        -- ????
            -- traceShow ('V', a, b, toExprs $ envirsWith fa envs, takeE i $ envirsWith fa envs) $
            thunks $ map (\d -> ($ fb eb) <$>
                                search (subScopeR xs envs) (d, fa ea)
                                >>= reason envs)
                         (takeE i $ envirsWith fa envs)
        (App ax ay, App bx by) -> do
            ta <- matchR envs (fa ax, fb bx)
            if ta then matchR envs (fa ay, fb by)
                  else pure False
            -- (&&) <$> matchR envs (fa ax, fb bx)
            --      <*> matchR envs (fa ay, fb by)
        _ -> pure False

search :: Reasoner (Expr, Expr) (Expr -> Expr)
search envs (d, a) =
    -- traceShow ('s', a, d) $
    -- fmap (\f -> traceShow ('S', a, d, f (Var "@")) f) $
    matchR envs (d, a)
    >>= \t -> if t
        then return id
        else let (fd, d') = detachEnv d
             in case d' of
                --  Env t d'  ->
                --     (Env t .) <$> search (subEnvirR t envs) (d', a)
                 App dx dy ->
                    -- ((\f g e -> App (f e) (g e)) <$>
                    -- search envs (fd dx, a) <*> search envs (fd dy, a))
                    -- <|>
                        ((\f e -> fd $ App (f e) dy) <$> search envs (fd dx, a))
                    <|> ((\g e -> fd $ App dx (g e)) <$> search envs (fd dy, a))
                 _ -> empty

-- search' :: Reasoner (Expr, Expr) (Expr -> Expr)
-- search' (d, a) = fmap (traceShow ('T', d, a)) <$> do
--     lt <- matchR' (d, a)
--     traceShow ('S', d, a) <$> return $ do
--         t <- lt
--         if t then return id
--              else case a of
--                  Env t a' ->
--                      (Env t .) <$>
--                      thunk (subEnvir t $ search' (d, a'))
--                  App xa ya ->
--                      thunk (search' (d, xa)) <|>
--                      thunk (search' (d, ya))
--                  _ -> empty

-- matchR' :: Reasoner (Expr, Expr) Bool
-- matchR' (a, b) =
--     fmap (\t -> traceShow ('M', a, b, t) t) <$>
--         do
--     (fa, ea) <- detachEnv <$> eval a
--     (fb, eb) <- detachEnv <$> eval b
--     envs <- envirs
--     traceShow ('m', a, b, envs) $ case (ea, eb) of
--         (Var "_", _) -> return $ Pure True
--         (Var xs, Var ys) | xs == ys
--             -> return $ traceShow ('C', xs) Pure True
--         (Param xs, _) -> do
--             -- ss <- varsInScope
--             -- if (\t -> traceShow (ss, ea, eb, t) t) $ ea == eb && xs `elem` ss
--             -- then return $ traceShow ('v', xs) Pure True
--             -- else do
--                 es <- concatMap toExprs <$> envirsWith fa
--                 es' <- concatMap toExprs <$> envirs
--                 traceShow ('V', fa ea, fb eb, es, es') return $ msum $ Pure False : map (\d ->
--                     thunk $ (>>= thunk . reason') . fmap ($ fb eb)
--                     <$> search' (d, fa ea)
--                     ) es
--                 -- ls <- mapM (\d -> fmap ($ fb eb) <$>
--                 --                   search' (d, fa ea)) es
--                 -- traceShow ('V', es) return $ Thunk $ map (return . (>>= thunk . reason')) ls
--         --     -- and <$> mapM reason (ds <*> [fb eb])
--         -- (Cons xs as, Cons ys bs) | xs == ys &&
--         --                            length as == length bs
--         --     -> bool [] [Pure ()] . all notNull
--         --        <$> mapM matchR' (zip (map fa as) (map fb bs))
--         (App ax ay, App bx by) -> traceShow ('P', a, b) $ do
--             -- t <- fmap traceShowId <$> matchR' (fa ax, fb bx)
--             u <- fmap traceShowId <$> matchR' (fa ay, fb by)
--             return $ u >>= (\x -> traceShow ('t', x) $ return x)
--             -- return $ (\x y -> traceShow ('T', x, y) x) <$> t <*> (Pure False)
--             -- return $ (\x y -> traceShow ('A', a, b, x, y) (x && y)) <$> t <*> u
--         --     -> undefined ((&&)
--         --         <$> matchR' (fa ax, fb bx)
--         --         <*> matchR' (fa ay, fb by))
--         _   -> return $ traceShow ("false", a, b) Pure False


subEnvirR :: (Int, Envir) -> EnvirS -> EnvirS
subEnvirR (i, env) (es, ss) = (env : takeEnd i es, ss)

subScopeR :: String -> EnvirS -> EnvirS
subScopeR s = mapSnd (s:)

envirsWith :: (Expr -> Expr) -> EnvirS -> EnvirS
envirsWith f envs = envirsOf envs $ f (Var "")
    where
        envirsOf :: EnvirS -> Expr -> EnvirS
        envirsOf envs = \case
            Env t e  -> envirsOf (subEnvirR t envs) e
            Var "" -> envs

toExprs :: EnvirS -> [Expr]
toExprs = concatMap (\(Envir xs _) -> xs) . fst

takeE :: Int -> EnvirS -> [Expr]
takeE i (envs, _) = concatMap (\(Envir xs _) -> xs) $ dropEnd i envs

varsInScope :: EnvirS -> [String]
varsInScope = snd

-- reason' :: Reasoner Expr Bool
-- reason' = \case
--     Env t e -> subEnvir t $ reason' e
--     e -> fmap (\t -> traceShow ('R', e, t) t) <$> do
--         es <- concatMap toExprs <$> envirs
--         return (fmap (\t -> traceShow ('E', e, es, t) t) <$> Thunk $ return (Pure False) : map (\a -> matchR' (a, e)) es)

-- search :: Expr -> Evaluator Expr [Expr -> Expr]
-- search d a = traceShow ("T", d, a) $ do
--     t <- matchR (a, d)
--     if t then return [id]
--          else case a of
--          Env t a'  -> fmap (Env t .)  <$> subEnvir t (search d a')
--          Cons s as -> do
--              xs <- mapM (search d) as
--              if all null xs
--              then return []
--              else let fss = zipWith (\a fs -> if null fs then [const a]
--                                                          else fs) as xs
--                   in return $ map (\fs e -> Cons s (fs <*> [e])) $ sequence fss
--          App a b -> do
--              fs <- search d a
--              gs <- search d b
--              return $ case (fs, gs) of
--                  ([], []) -> []
--                  (_,  []) -> map (\f e -> App (f e) b) fs
--                  ([], _ ) -> map (\g e -> App a (g e)) gs
--                  _        -> [\e -> App (f e) (g e) | f <- fs, g <- gs]
--          _ -> return []


-- reason :: Evaluator Expr Bool
-- reason = \case
--     -- Env (i, _) (Env (j, env) e) | j <= i
--     --         -> reason $ Env (j, env) e
--     Env t e -> subEnvir t (reason e)
--     e -> traceShow ("A", e) $ do
--         -- es' <- map toExprs <$> envirs
--         es <- concatMap toExprs <$> envirs
--         traceShow es orM $ map (\a -> matchR (a, e)) es


-- reasonOn :: Expr -> Evaluator Expr Bool
-- reasonOn (Env (i, _) d) = reasonOn' i d
--     where
--         reasonOn' :: Int -> Expr -> Evaluator Expr Bool
--         reasonOn' i d a = matchR (d, a)

--------------------------------------------------------------------------------
-- Utility functions --

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)

-- tmap :: (a -> b) -> (a, a) -> (b, b)
-- tmap f (a, b) = (f a, f b)

-- may :: Maybe a -> b -> (a -> b) -> b
-- may m b f = maybe b f m

-- repeatM :: Monad m => (a -> m a) -> Int -> a -> m [a]
-- repeatM f i a
--     | i <= 0 = return []
--     | otherwise = do
--         a' <- f a
--         as <- repeatM f (i - 1) a'
--         return $ a' : as

indexed :: [a] -> [(Int, a)]
indexed = zip [0..]

indexedR :: [a] -> [(Int, a)]
indexedR xs = zip (iterate (\i -> i - 1) (length xs - 1)) xs

readMaybe :: Read a => String -> Maybe a
readMaybe = fmap fst . listToMaybe . reads

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither a (Just b) = Right b
maybeToEither a Nothing = Left a

notNull :: [a] -> Bool
notNull = not . null
