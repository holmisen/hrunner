module Utils where

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM p cons alt = do
  t <- p
  if t then cons else alt


andM :: Monad m => m Bool -> m Bool -> m Bool
andM l r = ifM l r (return False)


rotateL :: [a] -> [a]
rotateL []     = []
rotateL (x:xs) = xs ++ [x]
