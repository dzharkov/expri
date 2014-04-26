module Utils
    ( try
    , trim
    ) where

import Control.Monad
import Data.Char (isSpace)
trim :: String -> String
trim = f . f
    where f = reverse . dropWhile isSpace

try :: MonadPlus m => m a -> m (Maybe a)
try m = liftM Just m `mplus` return Nothing

