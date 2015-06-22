module Lib.TPQueue where
import BurningPrelude
import Control.Concurrent.STM
import qualified Data.Heap as Heap

newtype TPQueue p a = TPQueue (TVar (Heap.Heap (Heap.Entry p a)))

newTPQueue :: STM (TPQueue p a)
newTPQueue = TPQueue <$> newTVar Heap.empty

readTPQueue :: Ord p => TPQueue p a -> STM (p, a)
readTPQueue (TPQueue th) = do
  h <- readTVar th
  case Heap.uncons h of
    Nothing -> retry
    Just (Heap.Entry p a, h') -> do
      writeTVar th h'
      return (p, a)

tryReadTPQueue :: Ord p => TPQueue p a -> STM (Maybe (p, a))
tryReadTPQueue (TPQueue th) = do
  h <- readTVar th
  case Heap.uncons h of
    Nothing -> return Nothing
    Just (Heap.Entry p a, h') -> do
      writeTVar th h'
      return (Just (p, a))

writeTPQueue :: Ord p => TPQueue p a -> p -> a -> STM ()
writeTPQueue (TPQueue th) p a = modifyTVar' th $ Heap.insert (Heap.Entry p a)