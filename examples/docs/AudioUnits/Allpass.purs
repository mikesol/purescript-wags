module WAGS.Example.Docs.AudioUnits.Allpass where

import Prelude

import Control.Plus (class Plus)
import Deku.Core (Element)
import Deku.Pursx (makePursx', nut, (~~))
import Effect (Effect)
import FRP.Event (Event, class IsEvent)
import Type.Proxy (Proxy(..))
import WAGS.Control (gain_, allpass_, (~), loopBuf)
import WAGS.Core (fan, input)
import WAGS.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent)
import WAGS.Example.Docs.Util (audioWrapper)
import WAGS.Interpret (ctxAff, decodeAudioDataFromUri)
import WAGS.Parameter (bangOn)
import WAGS.Run (run2_)

px =
  Proxy :: Proxy """<section>
  <h2 id="allpass">Allpass filter</h2>
  <p>An <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">all-pass filter</a> <a href="https://en.wikipedia.org/wiki/All-pass_filter">passes through all frequencies of a source at equal volume but changes their phase</a>. Its use by itself is imperceptible, as the human ear (mostly) does not pick up on phase shifts by themselves. However, when an all-pass filter's output is mixed with several chained all-pass filters plus the original source, you hear a neat phaser effect.</p>

  <p>The <code>bangOn</code> is an event that turns the loop buffer on. We'll learn more about turning things on and off in the "Events" section.</p>

  <pre><code>\buf -> run2_
  $ fan (loopBuf buf bangOn)
      \b -> gain_ 0.2
        (input b
        ~ allpass_ 700.0
            (allpass_ { frequency: 990.0, q: 20.0 } (input b)
            ~ allpass_ 1110.0
                ( input b
                ~ allpass_ { frequency: 2010.0, q: 30.0 } (input b)
                )
            )
        )
</code></pre>

  @allpass@
  </section>
"""

allpass
  :: forall payload. CancelCurrentAudio -> (Page -> Effect Unit) -> Event SingleSubgraphEvent -> Element Event payload
allpass ccb _ ev = makePursx' (Proxy :: _ "@") px
  { allpass: nut
      ( audioWrapper ev ccb (ctxAff \ctx -> decodeAudioDataFromUri ctx "https://freesound.org/data/previews/320/320873_527080-hq.mp3")
          \buf -> run2_
            $ fan (loopBuf buf bangOn)
                \b -> gain_ 0.2
                  (input b
                  ~ allpass_ 700.0
                      (allpass_ { frequency: 990.0, q: 20.0 } (input b)
                      ~ allpass_ 1110.0
                          ( input b
                          ~ allpass_ { frequency: 2010.0, q: 30.0 } (input b)
                          )
                      )
                  )
      )
  }