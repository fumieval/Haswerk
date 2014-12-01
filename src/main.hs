{-# LANGUAGE TemplateHaskell, LambdaCase, Rank2Types, ConstraintKinds, FlexibleContexts #-}
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

pipeline :: Monad m => (forall x. State w x -> m x) -> Object e (StateT w m) -> Object e m
pipeline f obj = Object $ \e -> do
  s <- f get
  ((a, o'), s') <- runStateT (runObject obj e) s
  f (put s')
  return (a, pipeline f o')

main = runSystemDefault $ do
  setFPS 30
  miku <- liftIO $ fromPMD "C:/Users/Fumiaki/Documents/Lat_miku/Lat式ミクVer2.31_Normalエッジ無し専用.pmd"
  world <- new $ variable newWorld
  pl <- new $ pipeline (world.-) Player.object
  
  linkGamepad $ \case
    PadButton _ (Down 7) -> pl .^ Player.Attack
    PadButton _ (Down 5) -> pl .^ Player.Act
    _ -> return ()
  
  linkGraphic $ \dt -> do
    pl .^ Player.Update dt

    getGamepads >>= \case
      [] -> return ()
      (p:_) -> do
        (mx : mz : px : py : _) <- gamepadAxes p
        bts <- gamepadButtons p

        when (bts !! 4) $ pl .& Player.position' += V3 0 (3*dt) 0
        when (bts !! 6) $ pl .& Player.position' -= V3 0 (3*dt) 0

        pl .^ Player.Move (play 0.0004 $ accel (V2 mx mz) ^* dt)
        pl .^ Player.Turn (play 0.0004 $ accel (V2 px py) ^* dt)
        
    w <- world .- get
    psp <- pl .^ Player.GetPerspective
    pos <- pl .& use Player.position
    return $ mconcat [psp $ mconcat [renderWorld pos w, miku], crosshair]

  stand