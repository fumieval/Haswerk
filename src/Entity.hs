module Entity where
import BurningPrelude
import Lib.Cube

data Target = TBlock (V3 Int) Surface
  | TNone
