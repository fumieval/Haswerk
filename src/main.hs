import BurningPrelude
import qualified Player
import World
import Geometry
import Util
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

type Chunks = HM.HashMap (V3 Int) VertexBuffer

emptyChunks :: HM.HashMap (V3 Int) VertexBuffer
emptyChunks = HM.empty

drawChunks :: Given System => M44 Float -> Texture -> Chunks -> IO ()
drawChunks v tex = itraverse_ (\i buf -> drawVertex (tr i !*! v) tex buf) where
  tr i = identity & translation .~ fmap fromIntegral (i ^* chunkSize)

chunkSize :: Int
chunkSize = 16

main = withHolz Windowed (Box (V2 0 0) (V2 1024 768)) $ do

  disableCursor
  -- clearColor (V4 0 0 0 0)

  pl <- new Player.object
  prevCursor <- new $ variable zero

  linkMouseButton $ \case
    Down 0 -> pl .^ Player.Attack
    Down 1 -> pl .^ Player.Act
    _ -> return ()
  linkMouseCursor $ \pos -> do
    pos' <- prevCursor .- get
    pl .^ Player.Turn ((pos - pos') / 300)
    prevCursor .- put pos

  world <- newMVar HM.empty
  cache <- newMVar HM.empty
  rendered <- newMVar emptyChunks

  texBlocks <- registerTexture _terrain_png

  chunkUpdate <- atomically newTPQueue :: IO (TPQueue Float (V3 Int))

  forkOS $ forever $ do
    (_, ch) <- atomically $ readTPQueue chunkUpdate
    ca <- readMVar cache
    buf <- registerVertex Triangles
      $ V.fromList
      $ flip appEndo []
      $ foldVoxel (\i cube (ty, f) -> Endo $ (++) $ f $ fmap (maybe Block.Transparent fst) cube)
      $ do
        k <- sequence (pure [0..chunkSize-1])
        let i = ch ^* chunkSize + k
        a <- ca ^.. ix i
        return a
    rm <- takeMVar rendered
    case rm ^? ix ch of
      Nothing -> return ()
      Just a -> releaseVertex a
    putMVar rendered $ rm & at ch ?~ buf

  -- Handle the input and draws the world periodically.
  forever $ do
    let dt = 1/60
    pl .^ Player.Update dt

    dir <- new $ variable zero

    whenM (keyPress KeyW) $ dir .- id += V2 0 1
    whenM (keyPress KeyS) $ dir .- id -= V2 0 1
    whenM (keyPress KeyA) $ dir .- id -= V2 1 0
    whenM (keyPress KeyD) $ dir .- id += V2 1 0
    whenM (keyPress KeySpace) $ pl .& Player.position' += V3 0 1 0
    whenM (keyPress KeyLeftShift) $ pl .& Player.position' -= V3 0 1 0

    v <- dir .- get

    pl .^ Player.Move (v ^* dt)

    pos <- pl .& use Player.position

    -- |ray| == 1
    ray <- pl .& uses Player.angleP spherical'

    w <- readMVar world

    case getMin $ foldMap (penetrationEntry pos ray)
      $ Set.fromList [t
          | k <- [0, sqrt 3..8]
          , x <- [-1..1]
          , y <- [-1..1]
          , z <- [-1..1]
          , let t = fmap floor (pos + ray ^* k) + V3 x y z
          , has (ix t) w] of
        Just (Heap.Entry _ (i, s)) -> pl .& Player.currentTarget .= TBlock i s
        Nothing -> pl .& Player.currentTarget .= TNone

    psp <- pl .^ Player.GetPerspective

    drawChunks psp texBlocks =<< readMVar rendered

penetrationEntry :: V3 Float -> V3 Float -> V3 Int -> Min (Heap.Entry Float (V3 Int, Surface))
penetrationEntry pos ray i = flip foldMap allSurfaces $ \s -> do
  let n = fromSurface s
  case penetration ray (fmap fromIntegral i + n ^* 0.5 - pos) n of
    Just k -> Min $ Just $ Heap.Entry k (i, s)
    Nothing -> mempty