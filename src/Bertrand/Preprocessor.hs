
module Bertrand.Preprocessor
    (preprocess
    ) where

import Control.Applicative
import Data.List
import Data.Maybe
import Data.Either

import Bertrand.Data

preprocess :: String -> (String, [ParseOption])
preprocess = mapFst unlines . partitionEithers . map line . lines . deleteComments


line :: String -> Either String ParseOption
line s = case words s of
    [x, n, o] | isJust m ->
        let Just num = m
        in case x of
            "infix"  -> Right $ Infix  num o
            "infixl" -> Right $ Infixl num o
            "infixr" -> Right $ Infixr num o
            "infixf" -> Right $ Infixf num o
            _        -> Left s
        where
            m = maybeRead n
    [x, o] -> case x of
            "data" -> Right $ DataCons o
            _      -> Left s

    _ -> Left s


deleteComments :: String -> String
deleteComments = id



maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (a, b) = (f a, b)
