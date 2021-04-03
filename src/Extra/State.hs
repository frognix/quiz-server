module Extra.State where

import Control.Monad.State hiding (state)

-- | Put state from Ether to StateT and return Either with result
putReturn :: Monad m => Either (a, s) (b, s) -> StateT s m (Either a b)
putReturn (Left  (a, s)) = put s >> return (Left  a)
putReturn (Right (b, s)) = put s >> return (Right b)

-- | Executes action on part of state that return fromUnit function
-- then add resulting state part to whole state with toUnit function
withStatePart :: Monad m => (u -> p) -> (p -> u -> u) -> StateT p m r -> StateT u m r
withStatePart fromUnit toUnit action = do
  part <- gets fromUnit
  (res, part') <- lift $ runStateT action part
  modify $ toUnit part'
  return res

-- | Executes action on first element of pair
withFst :: Monad m => StateT f m r -> StateT (f, s) m r
withFst = withStatePart fst (\f (_, s) -> (f, s))

-- | Executes action on second element of pair
withSnd :: Monad m => StateT f m r -> StateT (s, f) m r
withSnd = withStatePart snd (\s (f, _) -> (f, s))
