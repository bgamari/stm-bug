{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Main where

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Trans.Class
import           Data.Typeable
import           GHC.Generics
import           System.Mem

data Free f a = Pure a | Free (f (Free f a))
  deriving (Typeable, Generic, Generic1)

instance Functor f => Functor (Free f) where
  fmap f = go where
    go (Pure a)  = Pure (f a)
    go (Free fa) = Free (go <$> fa)
  {-# INLINE fmap #-}

instance Functor f => Applicative (Free f) where
  pure = Pure
  {-# INLINE pure #-}
  Pure a <*> Pure b = Pure $ a b
  Pure a <*> Free mb = Free $ fmap a <$> mb
  Free ma <*> b = Free $ (<*> b) <$> ma

instance Functor f => Monad (Free f) where
  return = pure
  {-# INLINE return #-}
  Pure a >>= f = f a
  Free m >>= f = Free ((>>= f) <$> m)

liftF :: (Functor f, MonadFree f m) => f a -> m a
liftF = wrap . fmap return

class Monad m => MonadFree f m | m -> f where
  -- | Add a layer.
  --
  -- @
  -- wrap (fmap f x) ≡ wrap (fmap return x) >>= f
  -- @
  wrap :: f (m a) -> m a
  default wrap :: (m ~ t n, MonadTrans t, MonadFree f n, Functor f) => f (m a) -> m a
  wrap = join . lift . wrap . fmap return

instance Functor f => MonadFree f (Free f) where
  wrap = Free
  {-# INLINE wrap #-}

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
  undefined

go :: v -> Free (SuspendF v) a -> IO (Either a (v, STM (Free (SuspendF v) a)))
go v (Free (StepIO a next))  = a >>= go v . next
go v (Free (StepSTM a next)) = pure $ Right (v, a >>= pure . next)
go _ (Pure a)                = pure $ Left a

runWidget :: Widget String a -> IO a
runWidget (Widget w) = case w of
  Free (StepIO io next) -> do
    a <- io
    runWidget $ Widget $ next a

main = runWidget $ comb $ map step (replicate 10000000 (effect retry))