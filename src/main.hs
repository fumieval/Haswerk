import BurningPrelude
import Call
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
import qualified Data.HashMap.Strict as Map
import qualified Data.Set as Set
import Debug.Trace
import Entity
import qualified Audiovisual.Text as Text
import Text.Printf
import Control.Concurrent
import Data.Witherable
import Data.BoundingBox (Box(..))

main = runCall Windowed (Box (V2 0 0) (V2 1024 768)) $ do
  setFPS 30

  disableCursor
  -- clearColor (V4 0 0 0 0)
  w <- newWorld
  world <- new $ variable w
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

  linkPicture $ \_ -> return $ translate (V2 320 240) $ bitmap _crosshair_png

  linkPicture $ \_ -> do
    t <- getFPS

    return $ mconcat [translate (V2 40 40) $ text $ printf "%.1f" t]

  rendered <- new $ variable Map.empty

  linkGraphic $ \dt -> do
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

    let mk i s = let n = fromSurface s in
          case penetration ray (fmap fromIntegral i + n ^* 0.5 - pos) n of
            Just k -> Min $ Just $ Heap.Entry k (i, s)
            Nothing -> mempty

    w <- world .- use blocks

    case getMin $ foldMap (\i -> foldMap (mk i) (unfoldSurfaces $ surfaces i w))
          $ Set.fromList [fmap floor (pos + ray ^* k) + V3 x y z
            | k <- [0, sqrt 3..8], x <- [-1..1], y <- [-1..1], z <- [-1..1]] of
      Just (Heap.Entry _ (i, s)) -> pl .& Player.currentTarget .= TBlock i s
      Nothing -> pl .& Player.currentTarget .= TNone

    s <- rendered .- do
      gets $ \sm -> Scene $ withSurfaces
        $ \cube -> ifoldMap (\v (bmp, ss) -> translate (fmap fromIntegral v)
          $ foldSurfaces (liftA2 drawPrimitive bmp cube) ss) sm

    psp <- pl .^ Player.GetPerspective
    return $ psp (translate pos skybox <> s <> line [V3 0 0 0, V3 0 0 1])

  forkIO $ forever $ world .- uses blockUpdate Heap.uncons >>= \case
    Nothing -> wait 0.01
    Just (Heap.Entry _ v, bu) -> do
      world .- blockUpdate .= bu
      world .- apprisesOf (blocks . at v . wither)
        (Block.Render (1/60))
        Alive
        (const Dead)
        >>= \case
          Impossible -> rendered .- at v .= Nothing
          Dead -> do
            rendered .- at v .= Nothing
            world .- forM_ neumann (causeBlockUpdate . (+v))
          Alive bmp -> do
            !ss <- world .- use (blocks . to (surfaces v))
            rendered .- at v ?= (bmp, ss)
