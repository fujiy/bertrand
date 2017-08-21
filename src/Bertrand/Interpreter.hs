{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveFunctor #-}

module Bertrand.Interpreter
    (evalShow
    )where

import Prelude hiding (lookup)
import Control.Applicative
import Control.Monad
import Control.Monad.Extra
-- import Control.Monad.Free
-- import Control.Monad.Loops
-- import Control.Monad.Plus
import Control.Monad.Reader
import Control.Monad.State
import Data.Bool
import Data.Char
import Data.Either
import Data.Foldable (asum)
import Data.List hiding (lookup)
-- import Data.List.Extra
import Data.Map (Map, lookup, fromListWith)
import Data.Maybe
-- import Data.Monoid
import Data.Tree
-- import Data.Tuple

import Bertrand.Data
-- import Bertrand.BFSearch

import Debug.Trace


--------------------------------------------------------------------------------
-- Data types --

type Interpreter = StateT ReasonS (Reader EnvirS)

type ReasonS = [Expr]
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

data Thunk a = Pure a
             | Thunk (ThunkList a) deriving Functor

type ThunkList a = [Interpreter (Thunk a)]

instance Applicative Thunk where
    pure = Pure
    Pure f   <*> Pure b   = Pure $ f b
    Pure f   <*> Thunk bs = Thunk $ fmap (fmap f) <$> bs
    Thunk fs <*> b        = Thunk $ fmap (<*> b) <$> fs

instance Alternative Thunk where
    empty = Thunk []
    Pure a   <|> Pure b   = Thunk [pure $ Pure a, pure $ Pure b]
    Pure a   <|> Thunk bs = Thunk $ pure (Pure a) : bs
    Thunk as <|> Pure b   = Thunk $ as ++ [pure $ Pure b]
    Thunk as <|> Thunk bs = Thunk $ as ++ bs

instance Monad Thunk where
    return = pure
    Pure a   >>= f = f a
    Thunk as >>= f = Thunk $ fmap (>>= f) <$> as

--------------------------------------------------------------------------------
-- Interpreter --

evaluate :: Evaluator a b -> a -> b
evaluate f a = runReader (evalStateT (f a) empty) empty
-- evaluate = undefined
                --   in traceShow s' b
-- evalWith :: EnvirS -> Evaluator a b -> a -> b
-- evalWith envs f a = runReader (f a) envs

subEnvir :: Envir -> Interpreter a -> Interpreter a
subEnvir env = local (\envs -> env : dropWhile (>= env) envs)

envirs :: Interpreter [Envir]
envirs = ask

currentDepth :: Interpreter Int
currentDepth = depth . head <$> ask

-- envirsWith :: (Envir -> [Expr]) -> Interpreter [Expr]
-- envirsWith f = concatMap (\env ->
--                    map (shield $ depth env + 1) $ f env)
--                <$> envirs

diveEnvM :: ((Expr -> Expr) -> a -> a) -> Evaluator Expr a -> Evaluator Expr a
diveEnvM f g = \case
    Env env a -> f (envir env) <$> subEnvir env (diveEnvM f g a)
    e         -> g e

diveEnv :: ((Expr -> Expr) -> a -> a) -> (Expr -> a) -> Expr -> a
diveEnv f g = \case
    Env env a -> f (envir env) $ diveEnv f g a
    e         -> g e

diveEnvM_ :: Evaluator Expr a -> Evaluator Expr a
diveEnvM_ = diveEnvM (const id)

diveEnv_ :: (Expr -> a) -> Expr -> a
diveEnv_ = diveEnv (const id)

tunnelEnv :: ((Expr -> Expr) -> a -> a) -> Evaluator (Expr, Expr) a -> Evaluator (Expr, Expr) a
-- tunnelEnv f g (a, b) = do
--   envs <- envirs
--   i <- currentDepth
--   -- traceShow ('t', i, a, b, envs) $
--   case a of
--     Env env a' -> do
--         i <- currentDepth
--         f (envir env) <$>
--             subEnvir env (tunnelEnv f g (a', shield i b))
--     _          -> g (a, b)

tunnelEnv f g (a, b) = do
  envs <- envirs
  b' <- shieldBy a b
  diveEnvM f
    (\a' -> do
        envs' <- envirs
        -- traceShow ('t', a, b, b', envs, envs') 
        g (a', b')) a


shieldBy :: Expr -> Evaluator Expr Expr
shieldBy a b = case a of
    Env env a' -> do
        i <- currentDepth
        if depth env > i
        then shield i <$> shieldBy a' b
        else foldl (flip Env) <$> shieldBy a' b <*>
                 (takeWhile (\e -> depth e >= depth env) <$> envirs)
    _ -> return b


envir :: Envir -> Expr -> Expr
envir env = \case
    -- Env env' a | depth env >= depth env'
    --          -> Env env' a
    System s -> System s
    a        -> Env env a

shield :: Int -> Expr -> Expr
shield i = envir mempty{depth = i + 1}

shieldWith :: Envir -> Expr -> Expr
shieldWith env = shield $ depth env

--------------------------------------------------------------------------------
-- Show function --

evalShow :: Expr -> String
evalShow e = show $ evaluate evalAll e

evalAll :: Evaluator Expr [Expr]
evalAll e = do
    es <- eval e
    flip concatMapM es $ diveEnvM fmap $ \case
        App a b -> zipWith App <$> evalAll a <*> evalAll b
        a       -> return [a]

--------------------------------------------------------------------------------
-- Evaluator --

eval :: Evaluator Expr [Expr]
eval e =
    -- (\e' -> trace (unwords [show e, "=>" ,show e']) e') <$>
    (
    diveEnvM map $ \case
    Id s -> do
        -- envs <- filter ((>= -1) . depth) <$> envirs
        -- envs <- envirs
        bs <- concatMap (\env -> map (shieldWith env) .
                      concat . lookup s $ binds env) <$> envirs
        if notNull bs
        then concatMapM eval bs
            -- (\es' -> trace (show (Id s) ++ "\n=> " ++ show es' ++ "\n!  " ++ show envs) es') <$>
        else return [Id s]
    App a b -> do
        -- envs <- filter ((>= -1) . depth) <$> envirs
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
    a -> return [a] ) e

    where
        -- mapEval :: Evaluator [Expr] [Expr]
        -- mapEval = concatMapM eval

        isLambda :: Expr -> Bool
        isLambda = diveEnv_ $ \case
            Lambda _ _ -> True
            _          -> False

        isSystemF :: Expr -> Bool
        isSystemF = diveEnv_ $ \case
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
            envs <- envirs
            -- envs <- takeWhile (\env ->
            --             depth env >= depthOf i a) <$> envirs
            let b' = foldl (flip Env) b envs
            -- (\e' -> trace (show (a, b) ++ "\n=> " ++ show e' ++ "\n  " ++ show es) e') <$>
            tunnelEnv fmap (
                \(Lambda a b, e) -> do
                    i <- currentDepth
                    envs' <- envirs
                    m <- fmap fromList <$> match (a, e)
                    -- traceShow ('a', i, a, b', e, m, envs, envs')
                    return $ (\x -> Env
                        mempty{binds = x, depth = i + 1} b) <$> m)
                (a, b)

        depthOf :: Int -> Expr -> Int
        depthOf i e = fromJust $ f e <|> Just i
            where
                f (Env env a) = f a <|> Just (depth env)
                f a = Nothing

        lambdaTree :: Evaluator [Expr] (Forest Expr)
        lambdaTree = foldM (curry addForest) []

        addForest :: Evaluator (Forest Expr, Expr) (Forest Expr)
        addForest (fe, e) = do
            (ls, rs) <-
                partitionEithers <$> mapM (\t@(Node a _) ->
                    bool (Left t) (Right t) <$> imply (e, a)) fe
            (imps, eqs) <-
                partitionEithers <$> mapM (\t@(Node a _) ->
                    bool (Left t) (Right t) <$> imply (a, e)) rs
            (uneqs, uimps) <-
                partitionEithers <$> mapM (\t@(Node a _) ->
                    bool (Left t) (Right t) <$> imply (a, e)) ls
            if not $ null eqs
            then let Node _ xs = head eqs
                 in  return $ Node e xs : fe
            else if not $ null imps
            then (++ uneqs) <$> mapM (\(Node a xs) ->
                     Node a <$> addForest (xs, e)) imps
            else return $ Node e uimps : uneqs

            where
                imply :: Evaluator (Expr, Expr) Bool
                imply (a, b) = let
                    (fa, Lambda c _) = detachEnv a
                    (fb, Lambda d _) = detachEnv b
                    in isJust <$> match (fb d, fa c)

        applySystemF :: Expr -> Expr -> Maybe Expr
        applySystemF a = diveEnv_ $ \case
            System b ->
                diveEnv_ (\(System (Func _ f)) -> f b) a
            _ -> Nothing

        fromList :: [(String, Expr)] -> Map String [Expr]
        fromList = fromListWith (++) . map (mapSnd pure)

evalAfter :: Monoid a => Expr -> Evaluator Expr a -> Interpreter a
evalAfter a f = eval a >>= mconcatMapM f

--------------------------------------------------------------------------------
-- Pattern matching --

match :: Evaluator (Expr, Expr) (Maybe [(String, Expr)])
match (a, b) = evalAfter a $
    tunnelEnv (const id) (\(a, b) -> case a of
        Id s -> do
            let wc = head s == '_'
                s' = if wc then tail s else s
            var <- isVar s'
            if null s' then return $ Just []
            else if var then do
                cs <- concatMap (\env -> map (shieldWith env) .
                              concat . lookup s' $ cstrs env) <$> envirs
                let ds = map ($ b) . catMaybes $ map (search s') cs
                bool Nothing (Just $ if wc then [] else [(s', b)])
                    <$> allM infer ds
            else evalAfter b $
                return . diveEnv_ (\case
                    Id s'' | s' == s'' -> Just []
                    _                  -> Nothing )

        System s -> evalAfter b $
            return . diveEnv_ (\case
                System s' | s == s' -> Just []
                _                   -> Nothing )

        App _ _ -> evalAfter b $ \b' ->
            let as = toList a
                bs = toList b'
            in if length as == length bs
                then fmap concat . sequence <$>
                    mapM match (zip as bs)
                else return Nothing

        _ -> return Nothing ) . (, b)
    where
        search :: String -> Expr -> Maybe (Expr -> Expr)
        search s = diveEnv (\f -> fmap (f .)) $ \case
            Id s' | s == s' -> Just id
            App a b -> case (search s a, search s b) of
                (Nothing, Nothing) -> Nothing
                (Just fa, Nothing) -> Just $ (`App` b) . fa
                (Nothing, Just fb) -> Just $ App a . fb
                (Just fa, Just fb) -> Just $ \e -> App (fa e) (fb e)
            _ -> Nothing

isVar :: Evaluator String Bool
isVar s | length s < 4 && all isLower s
        = notElem s . concatMap (snd . vars) <$> envirs
isVar s = elem s . concatMap (fst . vars) <$> envirs

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

-- search e = do
--     ds <- concatMap (\env ->
--         map (envir mempty{depth = depth env}) $ decls env) <$> envirs
--     catMaybes <$> mapM searchExpr ds
--     where
--         searchExpr :: Evaluator Expr (Maybe (Expr -> Expr))
--         searchExpr e = undefined


infer :: Evaluator Expr Bool
infer = diveEnvM_ (\e -> ((do
    ds <- concatMap (\env -> map (mapSnd $ shieldWith env) $
              decls env) <$> envirs
    envs <- envirs
    -- infer (App (App (Id "=>") (Id "true")) e)
    -- traceShow ('i', e, ds, envs)
    asum <$> mapM (\(ss, d) ->
        (<|>) <$> matchR ss (d, e) <*>
            (case toList d of
                b:_ | not (isName "=>" b) -> return $ pure False
                [_, b, c] -> (\x y -> (&&) <$> x <*> y) <$>
                    matchR [] (c, e) <*> matchR [] (Id "true", b)
                _ -> return $ pure False) ) ds
    ) >>= \case
    Pure a   -> return a
    Thunk fs -> go fs))

    where
        go :: Evaluator (ThunkList Bool) Bool
        go [] = return False
        go (f:fs) = f >>= \case
            Pure True  -> return True
            Pure False -> go fs
            Thunk fs'  -> go $ fs ++ fs'

        isName :: String -> Expr -> Bool
        isName s = diveEnv_ (\case
            Id s' -> s' == s
            _     -> False )

matchR :: [String] -> Evaluator (Expr, Expr) (Thunk Bool)
matchR ss = tunnelEnv (const id)
    (\(a, b) ->
        -- traceShow ('m', a, b) $
        case a of
        Id s -> do
            let wc = head s == '_'
                s' = if wc then tail s else s
            var <- isVar s'
            if null s' then return $ pure True
            else if var && s' `notElem` ss
                then pure . isJust <$> match (a, b)
            else return . pure $ diveEnv_ (\case
                Id s'' | s' == s'' -> True
                _                  -> False ) b
        App _ _ -> let
            as = toList a
            bs = toList b
            in if length as == length bs
                then andT $ mapM (matchR ss) (zip as bs)
                else return $ pure False
        _ -> return $ pure False)

        where
            andT :: Interpreter [Thunk Bool] -> Interpreter (Thunk Bool)
            andT m = m >>= \case
                []   -> return $ pure True
                t:ts -> return $ t >>= bool
                    (pure False)
                    (Thunk [andT $ return ts])

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
lookups x = map snd . filter ((== x) . fst)
-- lookups x = mapMaybe $ \(a, b) ->
--                 if x == a then Just b
--                           else Nothing

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
