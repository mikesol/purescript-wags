module Ocarina.Example.Docs.AudioUnits.SquareOsc where

import Prelude


import Deku.Core (Nut)
import Deku.Pursx ((~~))
import Effect (Effect)
import FRP.Poll (Poll)
import Ocarina.Control (gain_, squareOsc)
import Ocarina.Core (bangOn)
import Ocarina.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent)
import Ocarina.Example.Docs.Util (audioWrapper)
import Ocarina.Run (run2)
import Type.Proxy (Proxy(..))

px =
  Proxy :: Proxy """<section>
  <h2 id="sawtooth">Square wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sawtooth wave oscillator</a> plays back a sawtooth wave at a given frequency.</p>


  <pre><code>\buf -> run2_
  [ gain_ 0.2 [ squareOsc 448.0 bangOn ] ]</code></pre>

  ~periodic~
  </section>
"""

square
  :: CancelCurrentAudio
  -> (Page -> Effect Unit)
  -> Poll SingleSubgraphEvent
  -> Nut
square ccb _ ev = px ~~
  { periodic:
      ( audioWrapper ev ccb (\_ -> pure unit)
          \ctx _ -> run2 ctx
            [gain_ 0.2 [squareOsc 448.0 bangOn]]
      )
  }