module Entity where
import Prelude.Kai
import Lib.Cube
import Linear

data Target = TBlock (V3 Int) Surface
  | TNone
