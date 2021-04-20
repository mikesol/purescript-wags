module WAGS.Example.KitchenSink.TLP.PeriodicOsc where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Math ((%))
import WAGS.Change (change)
import WAGS.Connect (connect)
import WAGS.Control.Functions (branch, env, inSitu, proof, withProof)
import WAGS.Control.Qualified as WAGS
import WAGS.Control.Types (Frame, Scene)
import WAGS.Create (create)
import WAGS.Cursor (cursor)
import WAGS.Destroy (destroy)
import WAGS.Disconnect (disconnect)
import WAGS.Example.KitchenSink.TLP.LoopSig (LoopSig)
import WAGS.Example.KitchenSink.TLP.SawtoothOsc (doSawtoothOsc)
import WAGS.Example.KitchenSink.Timing (pieceTime, ksPeriodicOscIntegral)
import WAGS.Example.KitchenSink.Types.Empty (reset)
import WAGS.Example.KitchenSink.Types.PeriodicOsc (PeriodicOscUniverse, deltaKsPeriodicOsc, ksPeriodicOscGain, ksPeriodicOscPeriodicOsc)
import WAGS.Graph.Constructors (OnOff(..), SawtoothOsc(..))
import WAGS.Interpret (FFIAudio)
import WAGS.Run (SceneI)

doPeriodicOsc ::
  forall proofA iu cb.
  Frame (SceneI Unit Unit) FFIAudio (Effect Unit) proofA iu (PeriodicOscUniverse cb) LoopSig ->
  Scene (SceneI Unit Unit) FFIAudio (Effect Unit) proofA
doPeriodicOsc =
  branch \lsig -> WAGS.do
    { time } <- env
    toRemove <- cursor ksPeriodicOscPeriodicOsc
    gn <- cursor ksPeriodicOscGain
    pr <- proof
    withProof pr
      $ if time % pieceTime < ksPeriodicOscIntegral then
          Right (change (deltaKsPeriodicOsc time) $> lsig)
        else
          Left
            $ inSitu doSawtoothOsc WAGS.do
                disconnect toRemove gn
                destroy toRemove
                reset
                toAdd <- create (SawtoothOsc On 440.0)
                connect toAdd gn
                withProof pr lsig
