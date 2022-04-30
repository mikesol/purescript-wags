module WAGS.Example.Docs.Events.Ex1 where

import Prelude

import Control.Alt ((<|>))
import Data.Foldable (oneOf, oneOfMap, traverse_)
import Deku.Attribute (attr, cb, (:=))
import Deku.Control (blank, plant, text, text_)
import Deku.Core (Element)
import Deku.DOM as D
import Deku.Pursx (makePursx', nut)
import Effect (Effect)
import Effect.Aff (launchAff, launchAff_)
import Effect.Class (liftEffect)
import FRP.Event (Event)
import FRP.Event.Class (bang, biSampleOn)
import FRP.Event.VBus (V, vbus)
import Type.Proxy (Proxy(..))
import WAGS.Control (loopBuf)
import WAGS.Core (Node)
import WAGS.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent(..))
import WAGS.Example.Docs.Util (raceSelf)
import WAGS.Interpret (close, constant0Hack, context, decodeAudioDataFromUri)
import WAGS.Math (calcSlope)
import WAGS.Parameter (bangOn)
import WAGS.Properties (loopEnd, loopStart, playbackRate)
import WAGS.Run (run2)
import Web.Event.Event (target)
import Web.HTML.HTMLInputElement (fromEventTarget, valueAsNumber)

px =
  Proxy    :: Proxy      """<section>
 <h2>Example 2: Three sliders</h2>

  <p>In this example, we'll use three sliders to control the playback rate, the start time, and the end time of a looping buffer.</p>

  <p>There is a fair bit of DOM-related code in this example, so before showing the whole thing, let's isolate the Wags bit.</p>

  <pre><code>@wagtxt@</code></pre>

  <p>Note that our loopBuf consumes four events: in addition to the three sliders, there is a <code>bangOn</code> event that turns it on. For the events belonging to range sliders, we use <code>calcSlope</code> to normalize the range to sensible values for these parameters.</p>

  <p>Because each slider event contains a number, we can compose it with a function from <code>WAGS.Properties</code>, like <code>playbackRate</code> or <code>loopStart</code>, to create an event that controls a Wags parameter. The <code>oneOf</code> directive indicates that the incoming event will be "one of" the events in the array. It's also possible to use the tie-fighter, aka <code>alt</code>, to separate each event, but I like the array syntax when possible as tie fighters do, after all, work for the Empire, and who likes the Empire?</p>

  <p>And below you'll find the full example. It also shows useful patterns like downloading audio files and filtering events.</p>

  <pre><code>@txt@</code></pre>

  @ex1@

</section>
"""

txt :: String
txt =
  """module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Foldable (oneOf, oneOfMap, traverse_)
import Data.Maybe (Maybe(..), maybe)
import Deku.Attribute (attr, cb, (:=))
import Deku.Control (blank, plant, switcher, text, text_)
import Deku.Core (Element)
import Deku.DOM as D
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import FRP.Event (create)
import FRP.Event.Class (bang, biSampleOn, keepLatest)
import FRP.Event.VBus (V, vbus)
import Type.Proxy (Proxy(..))
import WAGS.Control (loopBuf)
import WAGS.Interpret (bracketCtx, decodeAudioDataFromUri)
import WAGS.Math (calcSlope)
import WAGS.Parameter (bangOn)
import WAGS.Properties (loopEnd, loopStart, playbackRate)
import WAGS.Run (run2_)
import WAGS.WebAPI (BrowserAudioBuffer)
import Web.Event.Event (target)
import Web.HTML.HTMLInputElement (fromEventTarget, valueAsNumber)

type Slider = V (s0 :: Number, s1 :: Number, s2 :: Number)
type StartStop = V (start :: Unit, stop :: Effect Unit)
type UIEvents = V (startStop :: StartStop, slider :: Slider)

atari =
  "https://freesound.org/data/previews/100/100981_1234256-lq.mp3" :: String

main :: Effect Unit
main = do
  { push, event } <- create
  runInBody (switcher scene event)
  push Nothing
  launchAff_ $ bracketCtx
    \ctx -> decodeAudioDataFromUri ctx atari >>= liftEffect
      <<< push
      <<< Just
  where
  scene
    :: forall lock payload
     . Maybe BrowserAudioBuffer
    -> Element lock payload
  scene = maybe (D.div_ [ text_ "Loading..." ]) \buffer ->
    D.div_ $ keepLatest $ vbus (Proxy :: _ UIEvents) \push event -> do
      let
        sl0 = event.slider.s0
        sl1 = event.slider.s1
        sl2 = event.slider.s2
        start = event.startStop.start <|> bang unit
        music = run2_
          [ loopBuf
              { buffer: buffer
              , playbackRate: 2.6
              , loopStart: 0.6
              , loopEnd: 1.1
              }
              $ oneOf
                [ bangOn
                , map
                    (calcSlope 0.0 0.2 100.0 5.0 >>> playbackRate)
                    sl0
                , map
                    (calcSlope 0.0 0.0 100.0 1.2 >>> loopStart)
                    sl1
                , map
                    (calcSlope 0.0 0.05 100.0 1.0 >>> loopEnd)
                    (biSampleOn sl2 (add <$> (bang 0.0 <|> sl1)))
                ]
          ]
      plant $ D.div_
        $
          map
            ( \{ l, f } -> D.div_
                [ text_ l
                , D.input
                    ( oneOfMap bang
                        [ D.Xtype := "range"
                        , D.Min := "0"
                        , D.Max := "100"
                        , D.Step := "1"
                        , D.Value := "50"
                        , D.OnInput := cb
                            ( traverse_
                                (valueAsNumber >=> f)
                                <<< (=<<) fromEventTarget
                                <<< target
                            )
                        ]
                    )
                    blank
                ]
            )
            [ { l: "Playback rate", f: push.slider.s0 }
            , { l: "Loop start", f: push.slider.s1 }
            , { l: "Loop end", f: push.slider.s2 }
            ] <>
            [ D.button
                ( oneOfMap (map (attr D.OnClick <<< cb <<< const))
                    [ start $> (music >>= push.startStop.stop)
                    , event.startStop.stop <#>
                        (_ *> push.startStop.start unit)
                    ]
                )
                [ text $ oneOf
                    [ start $> "Turn on"
                    , event.startStop.stop $> "Turn off"
                    ]
                ]
            ]
"""
type Slider = V (s0 :: Number, s1 :: Number, s2 :: Number)
type StartStop = V (start :: Unit, stop :: Effect Unit, loading :: Unit)
type UIEvents = V (startStop :: StartStop, slider :: Slider)

atari :: String
atari =
  "https://freesound.org/data/previews/100/100981_1234256-lq.mp3"

ex1
  :: forall lock payload. CancelCurrentAudio -> (Page -> Effect Unit) -> Event SingleSubgraphEvent -> Element lock payload
ex1 ccb _ ev = makePursx' (Proxy :: _ "@") px
  { wagtxt: nut
      ( text_
          """run2_
  $ loopBuf
      { buffer: buffer
      , playbackRate: 2.6
      , loopStart: 0.6
      , loopEnd: 1.1
      }
  $ oneOf
      [ bangOn
      , (calcSlope 0.0 0.2 100.0 5.0 >>> playbackRate) <$> sl0
      , (calcSlope 0.0 0.0 100.0 1.2 >>> loopStart) <$> sl1
      , (calcSlope 0.0 0.05 100.0 1.0 >>> loopEnd) <$> biSampleOn sl2
          (add <$> (bang 0.0 <|> sl1))
      ]"""
      )
  , txt: nut (text_ txt)
  , ex1: nut
      ( vbus (Proxy :: _ UIEvents) \push event -> -- here
          do
            let
              sl0 = event.slider.s0
              sl1 = event.slider.s1
              sl2 = event.slider.s2
              start = event.startStop.start <|> bang unit
              music :: forall lock0. _ -> Node _ lock0 _
              music buffer =
                loopBuf
                  { buffer: buffer
                  , playbackRate: 2.6
                  , loopStart: 0.6
                  , loopEnd: 1.1
                  }
                  $ oneOf
                    [ bangOn
                    , (calcSlope 0.0 0.2 100.0 5.0 >>> playbackRate) <$> sl0
                    , (calcSlope 0.0 0.0 100.0 1.2 >>> loopStart) <$> sl1
                    , (calcSlope 0.0 0.05 100.0 1.0 >>> loopEnd) <$> biSampleOn sl2
                        (add <$> (bang 0.0 <|> sl1))
                    ]
            plant $ D.div_
              $
                map
                  ( \{ l, f } -> D.div_
                      [ text_ l
                      , D.input
                          ( oneOfMap bang
                              [ D.Xtype := "range"
                              , D.Min := "0"
                              , D.Max := "100"
                              , D.Step := "1"
                              , D.Value := "50"
                              , D.OnInput := cb
                                  ( traverse_
                                      (valueAsNumber >=> f)
                                      <<< (=<<) fromEventTarget
                                      <<< target
                                  )
                              ]
                          )
                          blank
                      ]
                  )
                  [ { l: "Playback rate", f: push.slider.s0 }
                  , { l: "Loop start", f: push.slider.s1 }
                  , { l: "Loop end", f: push.slider.s2 }
                  ] <>
                  [ D.button
                      ( oneOfMap (map (attr D.OnClick <<< cb <<< const))
                          [ event.startStop.loading $> pure unit
                          , event.startStop.stop <#>
                              (_ *> (ccb (pure unit) *> push.startStop.start unit))
                          , (biSampleOn (bang (pure unit) <|> (map (\(SetCancel x) -> x) ev))
                              (start $> identity)) <#> \cncl -> do
                              cncl
                              push.startStop.loading unit
                              fib <- launchAff do
                                ctx <- context
                                c0h <- constant0Hack ctx
                                buffer <- decodeAudioDataFromUri ctx atari
                                liftEffect do
                                  res' <- run2 ctx [ music buffer ]
                                  let res = res' *> c0h *> close ctx
                                  push.startStop.stop res
                                  pure res
                              ccb do
                                push.startStop.start unit
                                launchAff_ $ raceSelf fib
                              pure unit
                          ]
                      )

                      [ text $ oneOf [ map (const "Turn off") event.startStop.stop
                      , map (const "Turn on") start ]
                      ]
                  ]
      )
  }
