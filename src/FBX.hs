{-# LANGUAGE LambdaCase, TypeFamilies, DataKinds, TypeOperators, FlexibleContexts, OverloadedStrings, StandaloneDeriving #-}
module Codec.Container.FBX where

import qualified Data.Vector.Unboxed as UV
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Int
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Extensible.Class
import Data.Extensible.Sum
import Data.Extensible.Dictionary
import Data.Functor.Identity
import qualified Codec.Compression.Zlib as Zlib
import Control.Monad
import Control.Lens
import Unsafe.Coerce
import Debug.Trace
import qualified Data.HashMap.Strict as HM

data FBX = FBX Version Record

newtype Version = Version Word32 deriving Show

data Record = Record [Property] (HM.HashMap ByteString Record) deriving Show

data Property = Scalar (Identity :| '[Int16, Int32, Int64, Float, Double, Bool])
  | Array (UV.Vector :| '[Int32, Int64, Float, Double, Bool])
  | String ByteString
  | Raw ByteString
  deriving Show

properties :: Lens' Record [Property]
properties f (Record p r) = f p <&> \p' -> Record p' r

subnodes :: Lens' Record (HM.HashMap ByteString Record)
subnodes f (Record p r) = Record p <$> f r

instance Plated Record where
  plate = subnodes . traverse

type instance Index Record = ByteString
type instance IxValue Record = Record

instance Ixed Record where
  ix i = subnodes . ix i

instance Binary FBX where
  get = do
    "Kaydara FBX Binary  \0" <- getByteString 21
    0x1A <- getWord8
    0x00 <- getWord8
    v <- get
    rs <- replicateUntil (BS.null . fst) getRecord
    return $ FBX v (Record [] (HM.fromList rs))
  put = undefined

instance Binary Version where
  get = Version <$> getWord32le
  put (Version v) = putWord32le v

getRecord :: Get (ByteString, Record)
getRecord = do
  ofs <- getWord32le -- EndOffset
  n <- getWord32le   -- NumProperties
  l <- getWord32le   -- PropertyListLen
  nl <- getWord8     -- NameLen
  s <- getByteString (fromIntegral nl) -- Name
  pos <- bytesRead
  ps <- replicateM (fromIntegral n) get
  rs <- loopWhile (fmap (<=fromIntegral ofs - 13) bytesRead) getRecord
  return (s, Record ps (HM.fromList rs))

getInt16 :: Get Int16
getInt16 = unsafeCoerce getWord16le

getInt32 :: Get Int32
getInt32 = unsafeCoerce getWord32le

getInt64 :: Get Int64
getInt64 = unsafeCoerce getWord64le

getFloat :: Get Float
getFloat = unsafeCoerce getWord32le

getDouble :: Get Double
getDouble = unsafeCoerce getWord64le

instance Binary Property where
  get = getWord8 >>= \r -> case toEnum (fromEnum r) of
    'Y' -> fmap scalar getInt16
    'C' -> fmap scalar (get :: Get Bool)
    'I' -> fmap scalar getInt32
    'F' -> fmap scalar getFloat
    'D' -> fmap scalar getDouble
    'L' -> fmap scalar getInt64
    'f' -> fmap (Array . embed) (getVectorOf getFloat)
    'd' -> fmap (Array . embed) (getVectorOf getDouble)
    'l' -> fmap (Array . embed) (getVectorOf getInt64)
    'i' -> fmap (Array . embed) (getVectorOf getInt32)
    'b' -> fmap (Array . embed) (getVectorOf (get :: Get Bool))
    'S' -> do
      len <- getWord32le
      String <$> getByteString (fromIntegral len)
    'R' -> do
      len <- getWord32le
      Raw <$> getByteString (fromIntegral len)
    x -> do
      p <- bytesRead
      fail $ "Unknown type code " ++ show x ++ " at " ++ show p
  put = undefined

getVectorOf :: UV.Unbox a => Get a -> Get (UV.Vector a)
getVectorOf g = do
  n <- getWord32le
  enc <- getWord32le
  if enc == 0
    then do
      _ <- getWord32le
      fmap UV.fromList (replicateM (fromIntegral n) g)
    else do
      l <- getWord32le
      bs <- getLazyByteString (fromIntegral l)
      return $ runGet (fmap UV.fromList (replicateM (fromIntegral n) g)) (Zlib.decompress bs)

scalar :: (x ∈ '[Int16, Int32, Int64, Float, Double, Bool]) => x -> Property
scalar = Scalar . embed . Identity

replicateUntil :: Monad m => (a -> Bool) -> m a -> m [a]
replicateUntil p m = go where
  go = do
    a <- m
    if p a
      then return []
      else liftM (a:) go

loopWhile :: Monad m => m Bool -> m a -> m [a]
loopWhile c m = go where
  go = c >>= \r -> if r
    then (:) <$> m <*> go
    else return []
