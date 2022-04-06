module WAGS.Tumult.Tumult.Make where

import Data.Set (empty)
import Data.Tuple.Nested (type (/\))
import Prim.RowList (class RowToList)
import WAGS.Tumult.Control.Types (WAG(..))
import WAGS.Tumult.Create (class Create, create)
import WAGS.Tumult.Graph.AudioUnit as CTOR
import WAGS.Tumult.Tumult (Tumultuous, unsafeTumult)
import WAGS.Tumult.Validation (class SubgraphIsRenderable)

tumultuously
  :: forall inputs scene graph graphRL
   . Create (output :: CTOR.Gain /\ { | scene }) () graph
  => RowToList graph graphRL
  => SubgraphIsRenderable graph "output" inputs
  => { output :: CTOR.Gain /\ { | scene } }
  -> Tumultuous "output" inputs
tumultuously scene = unsafeTumult instructions
  where
  WAG { instructions } = create scene (WAG { instructions: empty } :: WAG ())