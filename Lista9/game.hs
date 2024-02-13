{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies #-}


import Data.Char as DChar
import System.IO (isEOF)
import Control.Monad

class Monad m => TwoPlayerGame m s a b | m -> s a b where
  moveA :: s -> m a
  moveB :: s -> m b

data Score = Awins | Bwins | Draw

-- instance

type Board = (Maybe Int, Maybe Int)