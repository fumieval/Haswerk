import BurningPrelude
import Call
import qualified Player
import World
import Render
import Util
import Assets
import Voxel
import Control.Lens
import Control.Elevator
import qualified Block
import qualified Data.Heap as Heap
import qualified Data.Map as Map
import Debug.Trace
import Entity
import qualified Audiovisual.Text as Text
import Text.Printf
import Control.Concurrent
import Data.Witherable

main = runCallDefault $ do
  setFPS 30

  disableCursor
  -- clearColor (V4 0 0 0 0)
  world <- new $ variable newWorld
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

    v <- dir .- get

    pl .^ Player.Move (v ^* dt)

    psp <- pl .^ Player.GetPerspective
    pos <- pl .& use Player.position

    ray <- pl .& uses Player.angleP spherical'

    let mk i s = let n = fromSurface s in
          case penetration ray (fmap fromIntegral i + n ^* 0.5 - pos) n of
            Just k -> Heap.singleton $! Heap.Entry k (i, s)
            Nothing -> Heap.empty

    focusB <- world .- do
      iuses (blocks . _VoxelWorld . ifolded <. _2)
        $ \i ss -> foldMap (mk i) (unfoldSurfaces ss)

    case fmap (Heap.payload . fst) $ Heap.uncons focusB of
      Just (i, s) -> pl .& Player.currentTarget .= TBlock i s
      Nothing -> pl .& Player.currentTarget .= TNone

    sceneB <- rendered .- use folded

    return $ psp (translate pos skybox <> sceneB)

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
          Alive f -> do
            s <- world .- uses (blocks . to (surfaces v)) (foldMap f)
            rendered .- at v ?= translate (fmap fromIntegral v) s

  stand

data ForOne a b = Alive a | Dead | Impossible

instance Monoid (ForOne a b) where
  mempty = Impossible
  mappend Impossible a = a
  mappend a Impossible = a
  mappend _ _ = Impossible

-- check whether the given ray passes through a 1*1 square
penetration :: V3 Float -> V3 Float -> V3 Float -> Maybe Float
penetration v p n
  | c < 0
  , quadrance p < 8^2
  , all (<=0.50001) $ abs $ p - k *^ v = Just k
  | otherwise = Nothing
  where
    c = dot v n
    ob = dot p n
    k = ob / c
