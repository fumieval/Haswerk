import Call
import qualified Player
import World
import Render
import Control.Monad.Trans
import Util
import Data.Monoid
import Assets
import Util
import Control.Lens
import Control.Monad.State.Strict
import Control.Elevator

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

  -- linkPicture $ \_ -> return $ translate (V2 320 240) $ bitmap _crosshair_png

  linkGraphic $ \dt -> do
    pl .^ Player.Update dt

    dir <- new $ variable zero

    whenM (keyPress KeyW) $ dir .- id += V2 0 1
    whenM (keyPress KeyS) $ dir .- id -= V2 0 1

    whenM (keyPress KeyA) $ dir .- id -= V2 1 0
    whenM (keyPress KeyD) $ dir .- id += V2 1 0

    v <- dir .- get

    pl .^ Player.Move (v / 16)

    w <- world .- get
    psp <- pl .^ Player.GetPerspective
    pos <- pl .& use Player.position
    return $ mconcat [psp $ mconcat [renderWorld pos w], crosshair]

  stand
