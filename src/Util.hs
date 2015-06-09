module Util where
import BurningPrelude
import Control.Object
import Control.Monad.Catch
import qualified Data.Map as Map
import qualified Data.Heap as Heap
import Control.Concurrent.MVar

(*-&) :: MVar s -> StateT s IO a -> IO a
v *-& m = modifyMVar v (fmap swap . runStateT m)
infix 3 *-&

-- | Maybe-like monoid to fold 0 or 1 elements
data ForOne a = Alive a | Dead | Impossible

instance Monoid (ForOne a) where
  mempty = Impossible
  mappend Impossible a = a
  mappend a Impossible = a
  mappend _ _ = Impossible

newtype Min a = Min { getMin :: Maybe a }

instance Ord a => Monoid (Min a) where
  mempty = Min Nothing
  mappend (Min Nothing) a = a
  mappend a (Min Nothing) = a
  mappend (Min (Just a)) (Min (Just b)) = Min (Just (min a b))

play :: Metric f => Float -> f Float -> f Float
play t v
  | quadrance v < t*t = zero
  | otherwise = v

accel :: V2 Float -> V2 Float
accel v = v ^* quadrance v ** 0.3

(.^) :: (MonadIO m, MonadMask m) => Instance (Public s t) m -> t a -> m a
i .^ f = i .- Operate f
infixr 3 .^

(.&) :: (MonadIO m, MonadMask m) => Instance (Public s t) m -> StateT s m a -> m a
i .& m = do
  s <- i .- Stateful get
  (a, s') <- runStateT m s
  i .- Stateful (put s')
  return a

infixr 3 .&

data Public s t a = Operate (t a) | Stateful (State s a)

(&@~) :: Monad m => s -> (forall x. t x -> StateT s m x) -> Object (Public s t) m
s0 &@~ h = go s0 where
  go s = Object $ \case
    Stateful m -> return $ fmap go (runState m s)
    Operate t -> liftM (fmap go) $ runStateT (h t) s
