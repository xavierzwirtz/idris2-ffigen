module FFIGen.MonadExt

export
filterM : Monad m => (a -> m Bool) -> List a -> m (List a)
filterM p [] = pure []
filterM p (x :: xs)
    = if !(p x)
         then do xs' <- filterM p xs
                 pure (x :: xs')
         else filterM p xs

export
mapM : Monad m => (a -> m b) -> List a -> m (List b)
mapM = traverse
-- mapM p [] = pure []
-- mapM p (x :: xs)
--      = do
--     x' <- p x
--     xs' <- mapM p xs
--     pure (x' :: xs')
