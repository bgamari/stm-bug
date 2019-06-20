{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Applicative
import Control.Monad.Free
import Control.Concurrent.STM

data SuspendF v next
  = forall r. StepSTM (STM r) (r -> next)
  | forall r. StepIO (IO r) (r -> next)

deriving instance Functor (SuspendF v)

newtype Widget v a = Widget { step :: Free (SuspendF v) a }
  deriving (Functor, Applicative, Monad)

effect :: STM a -> Widget v a
effect a = Widget $ liftF $ StepSTM a id

io :: IO a ->  Widget v a
io a = Widget $ liftF $ StepIO a id

comb :: Monoid v => [Free (SuspendF v) a] -> Widget v a
comb vs = io $ do
  rs <- mapM (go mempty) vs
  pure rs

  undefined

go :: v -> Free (SuspendF v) a -> IO (Either a (v, STM (Free (SuspendF v) a)))
go v (Free (StepIO a next))   = a >>= go v . next
go v (Free (StepSTM a next))  = pure $ Right (v, a >>= pure . next)
go _ (Pure a)                 = pure $ Left a

runWidget :: Widget String a -> IO a
runWidget (Widget w) = case w of
  Free (StepIO io next) -> do
    a <- io
    runWidget $ Widget $ next a

main = runWidget $ comb $ map step (replicate 10000000 (effect retry))
