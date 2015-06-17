import BurningPrelude
import qualified Player
import Geometry
import Assets
import Voxel
import TPQueue
import Control.Bool
import Control.Concurrent
import Control.Concurrent.STM
import Control.Object
import Data.BoundingBox (Box(..))
import Data.Reflection
import Data.Witherable
import Debug.Trace
import Entity
import Graphics.Holz
import qualified Block
import qualified Data.HashMap.Strict as HM
import qualified Data.Heap as Heap
import qualified Data.Set as Set
import qualified Data.Vector.Storable as V
import Text.Printf
import Control.Concurrent
import qualified Data.Ix as Ix
import qualified Data.Array as A

type Chunks a = HM.HashMap (V3 Int) a

emptyChunks :: Chunks a
emptyChunks = HM.empty

drawChunks :: Given System => M44 Float -> Texture -> Chunks VertexBuffer -> IO ()
drawChunks v tex = itraverse_ (\i buf -> drawVertex (v !*! tr i) tex buf) where
  tr i = identity & translation .~ fmap fromIntegral (i ^* chunkSize)

chunkSize :: Int
chunkSize = 8

chunkRange :: (V3 Int, V3 Int)
chunkRange = (0, pure (chunkSize-1))

worldSeed :: Int
worldSeed = 0

readChunk :: V3 Int -> IO (A.Array (V3 Int) (Maybe Block.Appearance))
readChunk ch = do
  let origin@(V3 x0 y0 z0) = ch ^* chunkSize
  let hm = A.listArray (0, pure (chunkSize - 1)) $ do
        p <- Ix.range (0, pure (chunkSize - 1))
        let pos = fmap fromIntegral (V2 x0 z0 + p) :: V2 Float
        let h = perlin worldSeed (pos / 30) + perlin worldSeed (pos / 71) + perlin worldSeed (pos / 130)
        return $ floor $ h * 32 + 16
  return $ A.listArray chunkRange $ do
    p@(V3 x y z) <- Ix.range chunkRange
    let h = hm ^?! ix (V2 x z)
    if
      | y0 + y < h -> return $ Just $ Block.dirt
      | y0 + y == h -> return $ Just $ Block.gdirt
      | otherwise -> return Nothing

main = withHolz Windowed (Box (V2 0 0) (V2 1024 768)) $ do

  disableCursor
  clearColor (V4 0 0 0 1)

  pl <- new $ variable Player.initial
  prevCursor <- new $ variable zero

  linkMouseButton $ \case
    Down 0 -> pl .- Player.attack
    Down 1 -> pl .- Player.act
    _ -> return ()
  linkMouseCursor $ \pos -> do
    pos' <- prevCursor .- get
    pl .- Player.turn ((pos - pos') / 300)
    prevCursor .- put pos

  buffers <- newMVar emptyChunks

  texBlocks <- registerTexture _terrain_png
  texSkybox <- registerTexture _skybox_png

  chunkUpdate <- atomically newTPQueue :: IO (TPQueue Float (V3 Int))
  chunkReady <- atomically newEmptyTMVar :: IO (TMVar (V3 Int, V.Vector Vertex))

  for (Ix.range (V3 (-16) (-1) (-16), V3 16 2 16)) $ \p -> atomically
    $ writeTPQueue chunkUpdate (norm (fmap fromIntegral p :: V3 Float)) p

  skybox <- registerVertex Triangles $ V.fromList $ fold $ Block.cubeMesh (Cube
    [V2 (2/3) 0.5, V2 (1/3) 0.5, V2 (2/3) 0, V2 (1/3) 0]
    [V2 0 0, V2 (1/3) 0, V2 0 0.5, V2 (1/3) 0.5]
    [V2 0 0.5, V2 (1/3) 0.5, V2 0 1, V2 (1/3) 1]
    [V2 (2/3) 0.5, V2 1 0.5, V2 (2/3) 1, V2 1 1]
    [V2 (2/3) 0, V2 1 0, V2 (2/3) 0.5, V2 1 0.5]
    [V2 (1/3) 0.5, V2 (2/3) 0.5, V2 (1/3) 1, V2 (2/3) 1]) zero

  let worker = forever $ do
        (_, ch) <- atomically (readTPQueue chunkUpdate)
        m <- readChunk ch
        let !v = V.fromList $ do
              k <- Ix.range chunkRange
              (_, f) <- m ^.. ix k . _Just
              let cb = tabulate $ \s -> maybe Block.Transparent fst $ m ^? ix (k + fromSurface s) . _Just
              f cb (fmap fromIntegral k)
        atomically $ putTMVar chunkReady (ch, v)

  replicateM_ 3 (forkIO worker)

  -- Handle the input and draws the world periodically.
  forever $ withFrame $ do

    atomically (tryTakeTMVar chunkReady) >>= \case
      Just (ch, v) -> do
        buf <- registerVertex Triangles v
        rm <- takeMVar buffers
        case rm ^? ix ch of
          Nothing -> return ()
          Just a -> releaseVertex a
        putMVar buffers $ rm & at ch ?~ buf
      Nothing -> return ()

    let dt = 1/60
    pl .- Player.update dt

    dir <- new $ variable zero

    whenM (keyPress KeyW) $ dir .- id += V2 0 1
    whenM (keyPress KeyS) $ dir .- id -= V2 0 1
    whenM (keyPress KeyA) $ dir .- id -= V2 1 0
    whenM (keyPress KeyD) $ dir .- id += V2 1 0
    whenM (keyPress KeySpace) $ pl .- Player.position' += V3 0 0.1 0
    whenM (keyPress KeyLeftShift) $ pl .- Player.position' -= V3 0 0.1 0

    v <- dir .- get

    unless (nearZero v) $ pl .- Player.move (v ^* dt * 3)

    psp <- pl .- Player.getPerspective

    setProjection $ perspective (pi / 4) (1024/768) 1 360

    drawVertex (psp !*! scaled (V4 256 256 (-256) 1)) texSkybox skybox
    drawChunks psp texBlocks =<< readMVar buffers
    threadDelay $ floor $ dt * 1000 * 1000

newtype Min a = Min { getMin :: Maybe a }

instance Ord a => Monoid (Min a) where
  mempty = Min Nothing
  mappend (Min Nothing) a = a
  mappend a (Min Nothing) = a
  mappend (Min (Just a)) (Min (Just b)) = Min (Just (min a b))

penetrationEntry :: V3 Float -> V3 Float -> V3 Int -> Min (Heap.Entry Float (V3 Int, Surface))
penetrationEntry pos ray i = flip foldMap allSurfaces $ \s -> do
  let n = fromSurface s
  case penetration ray (fmap fromIntegral i + n ^* 0.5 - pos) n of
    Just k -> Min $ Just $ Heap.Entry k (i, s)
    Nothing -> mempty