import BurningPrelude
import qualified Player
import World
import Render
import Geometry
import Util
import Assets
import Voxel
import Control.Lens
import Control.Elevator
import qualified Block
import qualified Data.Heap as Heap
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import Debug.Trace
import Entity
import qualified Audiovisual.Text as Text
import Text.Printf
import Control.Concurrent
import Data.Witherable
import Data.BoundingBox (Box(..))
import Criterion.Measurement as Criterion
import Holz

main = runHolz Windowed (Box (V2 0 0) (V2 1024 768)) $ do
  setFPS 30

  disableCursor
  -- clearColor (V4 0 0 0 0)

  pl <- new $ Player.object @>>^ (world.-)
  prevCursor <- new $ variable zero

  linkMouse $ \case
    Button (Down 0) -> pl .^ Player.Attack
    Button (Down 1) -> pl .^ Player.Act
    Cursor pos -> do
      pos' <- prevCursor .- get
      pl .^ Player.Turn ((pos - pos') / 300)
      prevCursor .- put pos
    _ -> return ()

  text <- Text.simple defaultFont 24

  rendered <- newMVar emptyChunks

  blockUpdate <- newTPQueue

  -- Maintains vertex buffers
  forkIO $ forever $ do
    v <- readTPQueue blockUpdate
    world *-& apprisesOf (blocks . at v . wither)
      (Block.Render (1/60))
      Alive
      (const Dead)
      >>= \case
        Impossible -> return ()
        Dead -> do
          cache *-& at v .= Nothing
          for (map (v +) neumann) (writeChan blockUpdate)
        Alive a -> do
          cache *-& at v ?= a

  forkOS $ forever $ do
    ch <- readTPQueue chunkUpdate
    ca <- readMVar cache
    buf <- registerVertex Triangles
      $ V.fromList
      $ flip appEndo []
      $ foldVoxel (\i cube (ty, f) -> Endo $ (++) $ f $ fmap (maybe Transparent id . fst) cube)
      $ do
        k <- sequence (pure [0..chunkSize-1])
        let i = ch ^* chunkSize + k
        a <- ca ^.. ix i
        return (i, a)
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

    w <- world .- use blocks

    case getMin $ foldMap (penCandidate pos ray) candidates
      $ Set.fromList [t
          | k <- [0, sqrt 3..8]
          , x <- [-1..1]
          , y <- [-1..1]
          , z <- [-1..1]
          , let t = fmap floor (pos + ray ^* k) + V3 x y z
          , w `has` ix t] of
        Just (Heap.Entry _ (i, s)) -> pl .& Player.currentTarget .= TBlock i s
        Nothing -> pl .& Player.currentTarget .= TNone

    psp <- pl .^ Player.GetPerspective

    let texBlocks = _dirt_png

    rendered *-& (get >>= drawChunks psp texBlocks)

penetrationEntry :: V3 Float -> V3 Float -> V3 Int -> Min (Heap.Entry Double (V3 Int, Surface))
penetrationEntry pos ray i = flip foldMap allSurfaces $ \s -> do
  let n = fromSurface s
  case penetration ray (fmap fromIntegral i + n ^* 0.5 - pos) n of
    Just k -> Min $ Just $ Heap.Entry k (i, s)
    Nothing -> mempty