module Extra.State where

import Control.Monad.State hiding (state)

-- | Put state from Ether to StateT and return Either with result
putReturn :: Monad m => Either (a, s) (b, s) -> StateT s m (Either a b)
putReturn (Left  (a, s)) = put s >> return (Left  a)
putReturn (Right (b, s)) = put s >> return (Right b)
