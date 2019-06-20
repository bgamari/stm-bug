{-# LANGUAGE CPP          #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Main where

import           Control.Concurrent.STM

data Free f a = Pure a | Free (f (Free f a))

instance Functor f => Functor (Free f) where
  fmap f = go' where
    go' (Pure a)  = Pure (f a)
    go' (Free fa) = Free (go' <$> fa)

instance Functor f => Applicative (Free f) where
  pure = Pure
  Pure a <*> Pure b = Pure (a b)
  Pure a <*> Free mb = Free (fmap a <$> mb)
  Free ma <*> b = Free ((<*> b) <$> ma)

instance Functor f => Monad (Free f) where
  return = pure
  Pure a >>= f = f a
  Free m >>= f = Free ((>>= f) <$> m)

data SuspendF v next
  = forall r. StepSTM (STM r) (r -> next)
  | forall r. StepIO (IO r) (r -> next)

instance Functor (SuspendF v) where
  fmap f (StepSTM x g) = StepSTM x (f . g)
  fmap f (StepIO x g) = StepIO x (f . g)

newtype Widget v a = Widget { step :: Free (SuspendF v) a }
  deriving (Functor, Applicative, Monad)

effect :: STM a -> Widget v a
effect a = Widget (Free $ fmap return (StepSTM a id))

io :: IO a ->  Widget v a
io a = Widget (Free $ fmap return (StepIO a id))

comb :: Monoid v => [Free (SuspendF v) a] -> Widget v a
comb vs = io (do
  _ <- mapM (go mempty) vs
  undefined)

go :: v -> Free (SuspendF v) a -> IO (Either a (v, STM (Free (SuspendF v) a)))
go v (Free (StepIO a next))  = a >>= go v . next
go v (Free (StepSTM a next)) = pure (Right (v, a >>= pure . next))
go _ (Pure a)                = pure (Left a)

runWidget :: Widget String a -> IO a
runWidget (Widget w) = case w of
  Free (StepIO action next) -> do
    a <- action
    runWidget (Widget (next a))
  Free (StepSTM _ _ ) -> error "hi"
  Pure x -> return x

#if defined(GOOD)
main :: IO ()
#endif
main = runWidget (comb (map step (replicate 1000000 (effect retry))))
