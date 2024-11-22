module WAGS.Example.MidiSequencer where

import Prelude

import Control.Applicative.Indexed (ipure)
import Control.Comonad.Cofree (Cofree, mkCofree)
import Control.Plus (empty)
import Control.Promise (toAffE)
import Data.Array (fromFoldable)
import Data.Foldable (foldl, for_)
import Data.Int (toNumber)
import Data.Lens (over)
import Data.Lens.Lens.Tuple (_1)
import Data.List (List(..), (:))
import Data.List as L
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Vec ((+>))
import Data.Vec as V
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import FRP.Event (subscribe)
import FRP.Event.MIDI (midiAccess)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Heterogeneous.Mapping (hmap)
import Math (pow)
import WAGS.Change (ichange)
import WAGS.Control.Functions.Graph (iloop, (@!>))
import WAGS.Control.Indexed (IxWAG)
import WAGS.Control.Types (Frame0, Scene)
import WAGS.Create (icreate)
import WAGS.Create.Optionals (CPeriodicOsc, CSpeaker, CGain, gain, periodicOsc, speaker)
import WAGS.Graph.AudioUnit (TGain, TPeriodicOsc, TSpeaker)
import WAGS.Graph.Parameter (ff)
import WAGS.Interpret (close, context, makeFFIAudioSnapshot, makePeriodicWave)
import WAGS.Math (calcSlope)
import WAGS.Run (RunAudio, RunEngine, SceneI(..), Run, run)
import WAGS.WebAPI (AudioContext, BrowserPeriodicWave)

type Note = Maybe Int

type Pattern = List Note

bpm = 96 :: Int

noteDuration = 60.0 / toNumber bpm :: Number

bd = pure 1
sn = pure 2
hh = pure 3
pp = empty

score :: List Pattern
score =
    ( bd : pp : pp : pp : Nil)
  : ( pp : pp : sn : pp : Nil)
  : ( hh : hh : hh : hh : Nil)
  : Nil


type EnrichedNote
  = { start :: Number, dur :: Number, end :: Number } /\ Note

type EnrichedPattern = Array EnrichedNote

rest0 = { start: 0.0, dur: 2.0, end: 2.0 } /\ empty :: EnrichedNote

score' :: Array EnrichedNote
score' =
  let
    folded =
      foldl
        ( \{ pos, acc } notes ->
            let
              mk note =
                { start: toNumber pos * noteDuration
                , dur: noteDuration
                , end: (toNumber $ pos + 1) * noteDuration
                }
                  /\ note
            in
              { pos: pos + 1, acc: acc <> fromFoldable (map mk notes) }
        )
        { pos: 0, acc: [] }
        score
  in
    folded.acc

type SceneTemplate
  = { test :: Unit }

type SceneType
  = Unit

scene :: (SceneI Unit World ()) -> EnrichedNote -> Number -> SceneTemplate
scene (SceneI { time, world: unit }) ({ start, dur } /\ pitch) to = { test: unit }

type World = Unit

createFrame :: SceneI Unit World () -> IxWAG RunAudio RunEngine Frame0 Unit () () (List EnrichedNote)
createFrame e = icreate (scene e rest0 0.0) $> L.fromFoldable score'

piece :: Scene (SceneI Unit World ()) RunAudio RunEngine Frame0 Unit
piece =
  createFrame
    @!> iloop \e@(SceneI { time }) l -> do
      liftEffect $ log $ show $ "Playing: " <> show l
      -- send midi event here?
      pure l

easingAlgorithm :: Cofree ((->) Int) Int
easingAlgorithm = let fOf initialTime = mkCofree initialTime \adj -> fOf $ max 10 (initialTime - adj) in fOf 20

main :: Effect Unit
main =
  runHalogenAff do
    body <- awaitBody
    runUI component unit body

type State
  =
  { unsubscribe :: Effect Unit
  }

data Action
  = StartMidi
  | StopMidi

component :: forall query input output m. MonadEffect m => MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall input. input -> State
initialState _ =
  { unsubscribe: pure unit
  }

buttonStyle :: forall r i. HP.IProp (style :: String | r) i
buttonStyle = HP.style "padding: 3px; margin: 5px"

render :: forall m. State -> H.ComponentHTML Action () m
render =
  const do
    HH.div_
      [ HH.h1_
          [ HH.text "Midi Output Test" ]
      , HH.button
          [ HE.onClick \_ -> StartMidi, buttonStyle ]
          [ HH.text "Play" ]
      , HH.button
          [ HE.onClick \_ -> StopMidi, buttonStyle ]
          [ HH.text "Stop" ]
      ]

handleAction :: forall output m. MonadEffect m => MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  StartMidi -> do
    handleAction StopMidi
    midAcc <- H.liftAff $ toAffE midiAccess
    { unsubscribe } <-
      H.liftEffect do
        audioCtx <- context
        ffiAudio <- makeFFIAudioSnapshot audioCtx
        unsubscribe <-
          subscribe
            (run (pure unit) (pure unit) { easingAlgorithm } ffiAudio piece)
            (\(_ :: Run Unit ()) -> pure unit)
        pure { unsubscribe }
    H.modify_ _ { unsubscribe = unsubscribe }
  StopMidi -> do
    { unsubscribe } <- H.get
    H.liftEffect unsubscribe
    H.modify_ _ { unsubscribe = pure unit }
