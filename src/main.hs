import Assets
import Control.Bool
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Control.Object
import Data.Semigroup
import Data.BoundingBox (Box(..))
import Data.Functor.Rep
import Data.Reflection
import Data.Witherable
import Data.Traversable
import Debug.Trace
import Entity
import Geometry
import Graphics.Holz
import Graphics.Holz.Shader
import Lib.Cube
import Lib.TPQueue
import Linear
import Vertex
import qualified Block
import qualified Data.Array as A
import qualified Data.HashMap.Strict as HM
import qualified Data.Heap as Heap
import qualified Data.Ix as Ix
import qualified Data.Set as Set
import qualified Data.Vector.Storable as V
import qualified Player
import Text.Printf
import UnliftIO.Concurrent
import UnliftIO.STM

data WindowAndShader = WindowAndShader
  { wsWindow :: !Window
  , wsShader :: !(Shader ModelProj Vertex)
  }
instance HasWindow WindowAndShader where getWindow = wsWindow
instance HasShader WindowAndShader where
  type ShaderUniform WindowAndShader = ModelProj
  type ShaderVertex WindowAndShader = Vertex
  getShader = wsShader

type Chunks a = HM.HashMap (V3 Int) a

emptyChunks :: Chunks a
emptyChunks = HM.empty

drawChunks :: M44 Float
  -> Texture
  -> Chunks (VertexBuffer Vertex)
  -> ReaderT WindowAndShader IO ()
drawChunks v tex = itraverse_ (\i buf -> drawVertex (v !*! tr i) tex buf) where
  tr i = identity & translation .~ fmap fromIntegral (i ^* chunkSize)

chunkSize :: Int
chunkSize = 8

chunkRange :: (V3 Int, V3 Int)
chunkRange = (0, pure (chunkSize-1))

worldSeed :: Int
worldSeed = 0

generateChunk :: V3 Int -> IO (A.Array (V3 Int) (Maybe Block.Appearance))
generateChunk ch = do
  let origin@(V3 x0 y0 z0) = ch ^* chunkSize
  let hm = A.listArray (0, pure (chunkSize - 1)) $ do
        p <- Ix.range (0, pure (chunkSize - 1))
        let pos = fmap fromIntegral (V2 x0 z0 + p) :: V2 Float
        let h = perlin worldSeed (pos / 30) + perlin worldSeed (pos / 71) + perlin worldSeed (pos / 130)
        return $ round $ h * 16 + 16
  return $ A.listArray chunkRange $ do
    p@(V3 x y z) <- Ix.range chunkRange
    let h = hm ^?! ix (V2 x z)
    if
      | y0 + y < h -> return $ Just $ Block.dirt
      | y0 + y == h -> return $ Just $ Block.gdirt
      | otherwise -> return Nothing

readChunk :: V3 Int -> IO (A.Array (V3 Int) (Maybe Block.Appearance))
readChunk = generateChunk

main = withHolz $ do
  win <- openWindow Windowed (Box (V2 0 0) (V2 1024 768))
  sh <- makeShader vertexShaderSource fragmentShaderSource
  runReaderT holzMain (WindowAndShader win sh)

holzMain :: ReaderT WindowAndShader IO ()
holzMain = do
  win <- asks wsWindow

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

  buffers <- liftIO $ newMVar emptyChunks

  texBlocks <- registerTexture _terrain_png
  texSkybox <- registerTexture _skybox_png

  chunkUpdate :: TPQueue Float (V3 Int) <- atomically newTPQueue
  chunkReady :: TMVar (V3 Int, V.Vector Vertex) <- atomically newEmptyTMVar

  for (Ix.range (V3 (-16) (-1) (-16), V3 16 2 16)) $ \p -> liftIO $ atomically
    $ writeTPQueue chunkUpdate (norm (fmap fromIntegral p :: V3 Float)) p

  skybox <- registerVertex Triangles $ V.fromList $ foldMap id $ Block.cubeMesh (Cube
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

  replicateM_ 3 $ forkIO $ liftIO worker

  -- Handle the input and draws the world periodically.
  forever $ withFrame win $ do

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
    liftIO $ pl .- Player.update dt

    dir <- liftIO $ new $ variable zero

    whenM (keyPress KeyW) $ liftIO $ dir .- id += V2 0 1
    whenM (keyPress KeyS) $ liftIO $ dir .- id -= V2 0 1
    whenM (keyPress KeyA) $ liftIO $ dir .- id -= V2 1 0
    whenM (keyPress KeyD) $ liftIO $ dir .- id += V2 1 0
    whenM (keyPress KeySpace) $ liftIO $ pl .- Player.position' += V3 0 0.1 0
    whenM (keyPress KeyLeftShift) $ liftIO $ pl .- Player.position' -= V3 0 0.1 0

    v <- liftIO $ dir .- get

    liftIO $ unless (nearZero v) $ pl .- Player.move (v ^* dt * 3)

    psp <- liftIO $ pl .- Player.getPerspective

    setUniform mpProjection $ perspective (pi / 4) (1024/768) 1 360

    drawVertex (psp !*! scaled (V4 256 256 (-256) 1)) texSkybox skybox
    readMVar buffers >>= drawChunks psp texBlocks
    threadDelay $ floor $ dt * 1000 * 1000

penetrationEntry :: V3 Float -> V3 Float -> V3 Int -> Maybe (Min (Heap.Entry Float (V3 Int, Surface)))
penetrationEntry pos ray i = flip foldMap allSurfaces $ \s -> do
  let n = fromSurface s
  case penetration ray (fmap fromIntegral i + n ^* 0.5 - pos) n of
    Just k -> Just $ Min $ Heap.Entry k (i, s)
    Nothing -> mempty
