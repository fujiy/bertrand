{-# LANGUAGE TupleSections #-}
-- {-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module Bertrand.Interpreter
    (evalShow,
     reasonShow
    )where

import Control.Applicative
import Control.Monad
import Control.Monad.Extra
-- import Control.Monad.Free
-- import Control.Monad.Loops
-- import Control.Monad.Plus
import Control.Monad.Reader
-- import Control.Monad.State
import Data.Bool
import Data.Char (isLower)
import Data.Either
-- import Data.List
import Data.List.Extra
-- import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
-- import Data.Monoid
import Data.Tree
-- import Data.Tuple

import Bertrand.Data
-- import Bertrand.BFSearch

import Debug.Trace


--------------------------------------------------------------------------------
-- Data types --

type Interpreter = Reader EnvirS

type EnvirS = [Envir]

-- data IError = EmptyError
--             | Paradox Expr Expr
--     deriving (Eq, Show)

type Evaluator a b = a -> Interpreter b

-- type Lazy a = Free [] a
--
-- thunks :: [Lazy a] -> Lazy a
-- thunks = Free
--
--
-- type Reasoner a b = EnvirS -> a -> Lazy b

--------------------------------------------------------------------------------
-- Interpreter --

evaluate :: Evaluator a b -> a -> b
evaluate f a = runReader (f a) empty
                --   in traceShow s' b
evalWith :: EnvirS -> Evaluator a b -> a -> b
evalWith envs f a = runReader (f a) envs

subEnvir :: Envir -> Interpreter a -> Interpreter a
subEnvir env = local (\envs -> env : dropWhile (>= env) envs)

envirs :: Interpreter [Envir]
envirs = ask

currentDepth :: Interpreter Int
currentDepth = depth . head <$> ask

diveEnv :: ((Expr -> Expr) -> a -> a) -> Evaluator Expr a -> Evaluator Expr a
diveEnv f g = \case
    Env env a -> f (envir env) <$> subEnvir env (diveEnv f g a)
    e         -> g e

diveEnv' :: ((Expr -> Expr) -> a -> a) -> (Expr -> a) -> (Expr -> a)
diveEnv' f g = \case
    Env env a -> f (envir env) $ diveEnv' f g a
    e         -> g e

tunnelEnv :: ((Expr -> Expr) -> a -> a) -> Evaluator (Expr, Expr) a -> Evaluator (Expr, Expr) a
tunnelEnv f g (a, b) = case a of
    Env env a' -> do
        i <- currentDepth
        f (envir env) <$>
            subEnvir env (tunnelEnv f g (a', envir mempty{depth = i + 1} b))
    _          -> g (a, b)

envir :: Envir -> Expr -> Expr
envir env = \case
    Env env' a | depth env >= depth env'
             -> Env env' a
    System s -> System s
    a        -> Env env a

-- modify :: (Envir -> Envir) -> Interpreter a ()
-- modify f _ = ST.modify (mapSnd f)

--------------------------------------------------------------------------------
-- Show function --

evalShow :: Expr -> String
evalShow e = show $ evaluate evalAll e

reasonShow :: Expr -> String
-- reasonShow e = show $ evaluate boolean (envir e)
reasonShow e = undefined

evalAll :: Evaluator Expr [Expr]
evalAll e = do
    es <- eval e
    flip concatMapM es $ diveEnv fmap $ \case
        App a b -> zipWith App <$> evalAll a <*> evalAll b
        a       -> return [a]

-- evalAll e = do
--     e' <- eval e
--     evalF e'
--     where
--         evalF = \case
--             Env t a -> Env t <$> subEnvir t (evalF a)
--             Cons s es -> Cons s <$> mapM evalAll es
--             Lambda a b -> Lambda  <$> evalAll a <*> evalAll b
--             e          -> return e

--------------------------------------------------------------------------------
-- Evaluator --

eval :: Evaluator Expr [Expr]
eval e =
    -- (\e' -> trace (unwords [show e, "=>" ,show e']) e') <$>
    (diveEnv map $ \case
    -- Env env a -> map (envir env) <$> subEnvir env (eval a)
    Id s -> do
        envs <- filter ((>= -1) . depth) <$> envirs
        -- envs <- envirs
        bs <- concatMap (\env -> map (depth env + 1 ,) $ lookups s $ binds env)
              <$> envirs
        if notNull bs
        then
            -- (\es' -> trace (show (Id s) ++ "\n=> " ++ show es' ++ "\n!  " ++ show envs) es') <$>
            (mapEval $ map (\(i, e) -> envir mempty{depth = i + 1} e) bs)
        else return [Id s]
    App a b -> do
        envs <- filter ((>= -1) . depth) <$> envirs
        -- envs <- envirs
        as <- eval a
        -- ss <- evalSystem $ App a b
        -- let ss = []
        -- ss <- return []
        let ls = filter isLambda as
            ss = filter isSystemF as
        if null ls && null ss
        then return $ map (flip App b) as
        else
            -- (\es' -> trace (show (App a b) ++ "\n=> " ++ show es' ++ "\n!  " ++ show envs) es') <$>
             do
            bs <- if null ss then return [b] else eval b
            fs <- lambdaTree ls
            es <- nub <$> concatMapM (\b' -> applyTree (fs, b')) bs
            let ss' = catMaybes (applySystemF <$> ss <*> bs)
            (ss' ++) <$> concatMapM eval es
            -- envs <- filter ((>= 0) . depth) <$> envirs
            -- return $ traceShow (e, es, ss, e', envs) e'
    -- App a b -> do
    --     as <- eval a
    --     bs <- eval b
    --     concat <$> sequence ((\a' b' ->
    --         let (f, e) = detach a'
    --         in case e of
    --             Lambda c d -> do
    --                 i <- currentDepth
    --                 m <- match (c, b')
    --                 ds <- eval d
    --                 return $ maybe []
    --                     (\s -> map (Env mempty{binds = s, depth = i}) ds)
    --                     m
    --             _ -> return [App a' b']
    --         ) <$> as <*> bs)
    a -> return [a]) e

    where
        mapEval :: Evaluator [Expr] [Expr]
        mapEval = concatMapM eval

        isLambda :: Expr -> Bool
        isLambda = diveEnv' (const id) $ \case
            Lambda _ _ -> True
            _          -> False

        isSystemF :: Expr -> Bool
        isSystemF = diveEnv' (const id) $ \case
            System (Func _ _) -> True
            _                 -> False

        applyTree :: Evaluator (Forest Expr, Expr) [Expr]
        applyTree (fs, e) = concat <$> mapM
            (\(Node a bs) ->
                apply (a, e) >>=
                maybe (return [])
                      (\x -> defaultL x <$> applyTree (bs, e))
            ) fs
            where
                defaultL :: a -> [a] -> [a]
                defaultL a [] = [a]
                defaultL _ as = as

        apply :: Evaluator (Expr, Expr) (Maybe Expr)
        apply (a, b) = do
            i <- currentDepth
            es <- filter ((>= 0) . depth) <$> envirs
            envs <- takeWhile (\env -> depth env >= depthOf i a) <$> envirs
            let b' = foldl (flip Env) b envs
            -- (\e' -> trace (show (a, b) ++ "\n=> " ++ show e' ++ "\n  " ++ show es) e') <$>
            tunnelEnv fmap (
                \(Lambda a b, e) -> do
                    envs <- filter ((>= 0) . depth) <$> envirs
                    i <- currentDepth
                    m <- match (a, e)
                    return $ (\s -> Env mempty{binds = s, depth = i + 1} b) <$> m)
                (a, b')
        -- apply (a, b) = do
        --     i <- currentDepth
        --     let (f, Lambda c d) = detachEnv a
        --         j = depthOf i f
        --     m <- match (f c, Env mempty{depth = i} b)
        --     return $
        --         (\s -> f $ Env mempty{binds = s, depth = j + 1} d) <$> m
        depthOf :: Int -> Expr -> Int
        depthOf i e = fromJust $ f e <|> Just i
            where
                f (Env env a) = f a <|> Just (depth env)
                f a = Nothing

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

        applySystemF :: Expr -> Expr -> Maybe Expr
        applySystemF a = diveEnv' (const id) $ \case
            System b ->
                diveEnv' (const id) (\(System (Func _ f)) -> f b) a
            _ -> Nothing

-- evalSystem :: Evaluator Expr [Expr]
-- evalSystem e = do
--     es <- sequence <$> mapM eval (toList e)
--     envs <- envirs
--     traceShow (e, envs) catMaybes <$> forM es
--         (\ys -> return $ case map detach ys of
--             [Id "#intadd", Id n, Id m] -> Id <$> func2a (+) n m
--             [Id "#intsub", Id n, Id m] -> Id <$> func2a (-) n m
--             [Id "#intmul", Id n, Id m] -> Id <$> func2a (*) n m
--             _ -> Nothing)

    -- where
    --     func2a :: (Integer -> Integer -> Integer) -> String -> String -> Maybe String
    --     func2a f n m = (\x y -> show $ f x y) <$> readI n <*> readI m
    --
    --
    --     readI :: String -> Maybe Integer
    --     readI = readMaybe

-- evalSystemF :: Evaluator Expr Expr
-- evalSystemF e = let (f, Cons s es) = detachEnv e
--                 in case s of
--                     '#':xs -> fromMaybe e <$>
--                               Map.findWithDefault (const $ return Nothing) xs funcs es
--                     _      -> return e
--     where
--         funcs :: Map.Map String (Evaluator [Expr] (Maybe Expr))
--         funcs = Map.fromList
--             [("intplus",  func2 (\x y -> Just $ Cons (show $ x + y) [])),
--              ("intminus", func2 (\x y -> Just $ Cons (show $ x - y) [])),
--              ("intmultiply", func2 (\x y -> Just $ Cons (show $ x * y) []))
--             ]
--
--         func2 :: (Read a, Read b) => (a -> b -> Maybe Expr) -> Evaluator [Expr] (Maybe Expr)
--         func2 f = \case
--             [a, b] -> do
--                 (_, a') <- detachEnv <$> eval a
--                 (_, b') <- detachEnv <$> eval b
--                 return $ case (a', b') of
--                     (Cons as [], Cons bs [])
--                         -> case (readMaybe as, readMaybe bs) of
--                                (Just x, Just y) -> f x y
--                                _ -> Nothing
--                     _ -> Nothing
--             _ -> return Nothing

--------------------------------------------------------------------------------
-- Pattern matching --

match :: Evaluator (Expr, Expr) (Maybe [(String, Expr)])
match (a, b) = eval a >>= mconcatMapM
    (tunnelEnv (const id)
        (\(a, b) -> case a of
            Id s -> do
                var <- isVar s
                if isWildcard s then return $ Just []
                else if var     then return $ Just [(s, b)]
                else eval b >>= mconcatMapM
                    (return . diveEnv' (const id) (\case
                        Id s' | s == s' -> Just []
                        _               -> Nothing ))
            System s ->
                eval b >>= mconcatMapM
                    (return . diveEnv' (const id) (\case
                        System s' | s == s' -> Just []
                        _                   -> Nothing ))
            App _ _ ->
                eval b >>= mconcatMapM (\b' ->
                    let as = toList a
                        bs = toList b'
                    in if length as == length bs
                        then fmap concat . sequence <$>
                            mapM match (zip as bs)
                        else return Nothing)

            _ -> return Nothing ) . (, b))
    where
        isVar :: Evaluator String Bool
        isVar s | length s < 4 && all isLower s
                = return True
        isVar s = elem s . concatMap vars <$> envirs

        isWildcard :: String -> Bool
        isWildcard s = head s == '_'

-- match (a, b) = do
--     (fa, ea) <- detachEnv <$> eval a
--     case ea of
--         Var "_" -> return $ Just []
--         Var s -> do
--             i <- envirsNum
--             return $ if envirsNumOf i (fa ea) >= i
--                      then Just [Bind s b]
--                      else Just []
--         Param _ s -> do
--             i <- envirsNum
--             return $ if envirsNumOf i (fa ea) >= i
--                      then Just [Bind s b]
--                      else Just []
--         Cons xs as -> do
--             (fb, eb) <- detachEnv <$> eval b
--             case eb of
--                 Cons ys bs | xs == ys &&
--                               length as == length bs -> do
--                     ms <- mapM match $ zip (map fa as) (map fb bs)
--                     return $ if all isJust ms
--                              then Just $ concat $ catMaybes ms
--                              else Nothing
--                 Comma bs -> undefined
--                 _ -> return Nothing
--         Comma as -> do
--             ms <- mapM (\a' -> match (fa a', b)) as
--             return $ if any isJust ms
--                      then Just $ concat $ catMaybes ms
--                      else Nothing
--         App _ _ -> return Nothing
--         _       -> return Nothing
--
--         where
--             envirsNumOf :: Int -> Expr -> Int
--             envirsNumOf i = \case
--                 Env (j, _) e -> envirsNumOf (min i j) e
--                 _            -> i

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

-- infer :: Evaluator Expr Bool
-- infer e = do
--     envs <- ask
--     return $ case reason envs e of
--         Pure a -> a
--         Free s -> foldReason s
--     where
--         foldReason :: [Lazy Bool] -> Bool
--         foldReason [] = False
--         foldReason (l:ls) = case l of
--             Pure True  -> True
--             Pure False -> foldReason ls
--             Free ls'   -> foldReason (ls ++ ls')

        -- foldReason = foldl (\t -> \case
        --     Pure t' -> t || t'
        --     Free ls -> t || foldReason ls) False

-- boolean :: Evaluator Expr Boolean
-- boolean e = do
--     let (f, e') = detachEnv e
--     t <- infer e
--     if t then return T
--          else do
--             --  return U
--          f <- infer $ f (App (Var "~") e')
--          if f then return F
--               else return U
--
-- reason :: Reasoner Expr Bool
-- reason envs e = case e of
--     Env t e -> reason (subEnvirR t envs) e
--     e       ->
--     -- traceShow ('R', e) $
--                thunks $ map (\d -> matchR True envs (d, e))
--                             (toExprs envs)
--
-- matchR :: Bool -> Reasoner (Expr, Expr) Bool
-- matchR mp envs (a, b) =
--     -- traceShow ('M', a, b) $
--     -- (\t -> traceShow ('M', t, a, b) t) <$>
--     let
--     (fa, ea) = detachEnv $ evalWith envs eval a
--     (fb, eb) = detachEnv $ evalWith envs eval b
--     in case (ea, eb) of
--         (Var "_", _)
--             -> pure True
--         (Var xs, Var ys) | xs == ys
--             -> pure True
--         -- (Param _ xs, Param _ ys) | xs == ys &&
--         --                            xs `elem` varsInScope envs
--         --     -> pure True
--         (Param i xs, _) | mp ->
--         -- ????
--             -- traceShow ('V', a, b, toExprs $ envirsWith fa envs, takeE i $ envirsWith fa envs) $
--             -- thunks $ map (\d -> ($ fb eb) <$>
--             --                     search (subScopeR xs envs) (d, fa ea)
--             --                     >>= reason envs)
--             --              (takeE i $ envirsWith fa envs)
--             thunks $ map (\d -> ($ fb eb) <$>
--                                 search envs (d, fa $ Var xs)
--                                 >>= reason envs)
--                          (traceShowId $ toExprs $ envirsWith fa envs)
--         (App ax ay, App bx by) -> do
--             ta <- matchR mp envs (fa ax, fb bx)
--             if ta then matchR mp envs (fa ay, fb by)
--                   else pure False
--             -- (&&) <$> matchR envs (fa ax, fb bx)
--             --      <*> matchR envs (fa ay, fb by)
--         _ -> pure False
--
-- search :: Reasoner (Expr, Expr) (Expr -> Expr)
-- search env (d, a) =
--     -- traceShow ('s', a, d) $
--     -- fmap (\f -> traceShow ('S', a, d, f (Var "@")) f) $
--     -- if (d, a) `elem` searching env
--     -- then empty
--     -- else let env' = addSearching (d, a) env in
--     matchR False env (d, a) >>= \t ->
--         if t
--         then return id
--         else let (fd, d') = detachEnv d
--              in case d' of
--                --  Env t d'  ->
--                --     (Env t .) <$> search (subEnvirR t envs) (d', a)
--                  App dx dy ->
--                     -- ((\f g e -> App (f e) (g e)) <$>
--                     -- search envs (fd dx, a) <*> search envs (fd dy, a))
--                     -- <|>
--                          ((\f e -> fd $ App (f e) dy) <$> search env (fd dx, a))
--                      <|> ((\g e -> fd $ App dx (g e)) <$> search env (fd dy, a))
--                  _ -> empty
--
-- subEnvirR :: (Int, Envir) -> EnvirS -> EnvirS
-- subEnvirR (i, env) es = env : takeEnd i es
--
-- envirsWith :: (Expr -> Expr) -> EnvirS -> EnvirS
-- envirsWith f envs = envirsOf envs $ f (Var "")
--     where
--         envirsOf :: EnvirS -> Expr -> EnvirS
--         envirsOf envs = \case
--             Env t e  -> envirsOf (subEnvirR t envs) e
--             Var "" -> envs
--
-- toExprs :: EnvirS -> [Expr]
-- toExprs = concatMap (\(Envir xs _) -> xs)

-- takeE :: Int -> EnvirS -> [Expr]
-- takeE i envs = concatMap (\(Envir xs _) -> xs) $ dropEnd i envs

-- searching :: EnvirS -> [(Expr, Expr)]
-- searching = snd



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

lookups :: Eq a => a -> [(a, b)] -> [b]
lookups x = mapMaybe $ \(a, b) ->
                if x == a then Just b
                          else Nothing

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
