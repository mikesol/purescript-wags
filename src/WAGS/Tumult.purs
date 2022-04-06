module WAGS.Tumult where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Foldable (foldl)
import Data.Map (SemigroupMap(..), lookup)
import Data.Map as Map
import Data.Maybe (maybe')
import Data.Newtype (wrap)
import Data.Profunctor (lcmap)
import Data.Set as Set
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Data.Variant (match)
import FRP.Behavior (sample_)
import FRP.Event (class IsEvent, fold, keepLatest)
import Prim.RowList (class RowToList)
import Type.Proxy (Proxy(..))
import WAGS.Core (AudioWorkletNodeOptions_(..), Instruction(..))
import WAGS.Core as C
import WAGS.Parameter (AudioCancel(..), AudioEnvelope(..), AudioNumeric(..), AudioParameter(..), AudioSudden(..))
import WAGS.Subgraph (class MakeInputs, inputs)
import WAGS.Tumult.Tumult (Tumultuous, safeUntumult)
import WAGS.Tumult.Tumult.Reconciliation (reconcileTumult)

-- tumult

tumult
  :: forall outputChannels terminus inputs inputsRL event payload
   . IsEvent event
  => IsSymbol terminus
  => RowToList inputs inputsRL
  => MakeInputs inputsRL inputs
  => event ({ | inputs } -> Tumultuous terminus inputs)
  -> C.Node outputChannels () inputs event payload
tumult atts' = C.Node go
  where
  asNumber (AudioParameter v) = match
    { numeric: \(AudioNumeric { n }) -> n
    , cancel: \(AudioCancel _) -> 0.0
    , envelope: \(AudioEnvelope _) -> 0.0
    , sudden: \(AudioSudden { n }) -> n
    }
    v
  terminus = reflectSymbol (Proxy :: _ terminus)
  atts = fold (\a b -> reconcileTumult a (fst b))
    (map (\t -> safeUntumult (t (inputs (Proxy :: _ inputsRL)))) atts')
    (Set.empty /\ (wrap Map.empty))
  go prnt (C.AudioInterpret ai@{ ids, connectXToY }) =
    keepLatest
      ( (sample_ ids (pure unit)) <#> \msfx' ->
          let
            sfx i = i <> "_" <> msfx'

            isfx
              :: forall r a
               . ({ id :: String | r } -> a)
              -> ({ id :: String | r } -> a)
            isfx i = lcmap (\ii -> ii { id = sfx ii.id }) i
          in
            keepLatest
              ( map
                  ( \(instr /\ (SemigroupMap mp)) -> foldl
                      ( \b (Instruction i) -> b <|> match
                          { makeAllpass: \{ id, frequency, q, parent } -> pure $
                              ai.makeAllpass
                                { id: sfx id
                                , frequency: asNumber frequency
                                , q: asNumber q
                                , parent
                                }
                          , makeAnalyser:
                              \{ id
                               , channelCount
                               , channelCountMode
                               , channelInterpretation
                               , fftSize
                               , cb
                               , maxDecibels
                               , minDecibels
                               , parent
                               , smoothingTimeConstant
                               } -> pure $
                                ai.makeAnalyser
                                  { id: sfx id
                                  , channelCount
                                  , channelCountMode
                                  , channelInterpretation
                                  , fftSize
                                  , cb
                                  , maxDecibels
                                  , minDecibels
                                  , parent
                                  , smoothingTimeConstant
                                  }
                          , makeAudioWorkletNode:
                              \{ id
                               , options:
                                   ( AudioWorkletNodeOptions_
                                       { name
                                       , numberOfInputs
                                       , numberOfOutputs
                                       , outputChannelCount
                                       , parameterData
                                       , processorOptions
                                       }
                                   )
                               , parent
                               } ->
                                pure $ ai.makeAudioWorkletNode
                                  { id: sfx id
                                  , options:
                                      AudioWorkletNodeOptions_
                                        { name
                                        , numberOfInputs
                                        , numberOfOutputs
                                        , outputChannelCount
                                        , parameterData: map asNumber
                                            parameterData
                                        , processorOptions
                                        }
                                  , parent
                                  }
                          , makeBandpass: \{ id, frequency, q, parent } -> pure
                              $
                                ai.makeBandpass
                                  { id: sfx id
                                  , frequency: asNumber frequency
                                  , q: asNumber q
                                  , parent
                                  }
                          , makeConstant: \{ id, onOff, offset, parent } ->
                              ( pure
                                  $
                                    ai.makeConstant
                                      { id: sfx id
                                      , offset: asNumber offset
                                      , parent
                                      }
                              ) <|> (pure $ ai.setOnOff { id: sfx id, onOff })
                          , makeConvolver: \{ id, buffer, parent } -> pure $
                              ai.makeConvolver
                                { id: sfx id
                                , buffer
                                , parent
                                }
                          , makeDelay: \{ id, delayTime, parent } -> pure $
                              ai.makeDelay
                                { id: sfx id
                                , delayTime: asNumber delayTime
                                , parent
                                }
                          , makeDynamicsCompressor:
                              \{ id
                               , knee
                               , threshold
                               , ratio
                               , attack
                               , release
                               , parent
                               } -> pure $
                                ai.makeDynamicsCompressor
                                  { id: sfx id
                                  , knee: asNumber knee
                                  , threshold: asNumber threshold
                                  , ratio: asNumber ratio
                                  , attack: asNumber attack
                                  , release: asNumber release
                                  , parent
                                  }
                          , makeGain: \{ id, gain, parent } -> pure $
                              ai.makeGain
                                { id: sfx id
                                , gain: asNumber gain
                                , parent
                                }
                          , makeHighpass: \{ id, frequency, q, parent } -> pure
                              $
                                ai.makeHighpass
                                  { id: sfx id
                                  , frequency: asNumber frequency
                                  , q: asNumber q
                                  , parent
                                  }
                          , makeHighshelf: \{ id, frequency, gain, parent } ->
                              pure
                                $
                                  ai.makeHighshelf
                                    { id: sfx id
                                    , frequency: asNumber frequency
                                    , gain: asNumber gain
                                    , parent
                                    }
                          , makeLoopBuf:
                              \{ id
                               , loopStart
                               , loopEnd
                               , buffer
                               , onOff
                               , playbackRate
                               , duration
                               , parent
                               } ->
                                ( pure $
                                    ai.makeLoopBuf
                                      { id: sfx id
                                      , loopStart
                                      , loopEnd
                                      , buffer
                                      , playbackRate: asNumber playbackRate
                                      , parent
                                      , duration
                                      }
                                ) <|> (pure $ ai.setOnOff { id: sfx id, onOff })
                          , makeLowpass: \{ id, frequency, q, parent } -> pure $
                              ai.makeLowpass
                                { id: sfx id
                                , frequency: asNumber frequency
                                , q: asNumber q
                                , parent
                                }
                          , makeLowshelf: \{ id, frequency, gain, parent } ->
                              pure
                                $
                                  ai.makeLowshelf
                                    { id: sfx id
                                    , frequency: asNumber frequency
                                    , gain: asNumber gain
                                    , parent
                                    }
                          , makeMediaElement: \{ id, element, parent } -> pure
                              $
                                ai.makeMediaElement
                                  { id: sfx id
                                  , element
                                  , parent
                                  }
                          , makeMicrophone: \{ id, microphone, parent } -> pure
                              $
                                ai.makeMicrophone
                                  { id: sfx id
                                  , microphone
                                  , parent
                                  }
                          , makeNotch: \{ id, frequency, q, parent } -> pure $
                              ai.makeNotch
                                { id: sfx id
                                , frequency: asNumber frequency
                                , q: asNumber q
                                , parent
                                }
                          , makePeaking: \{ id, frequency, q, gain, parent } ->
                              pure $
                                ai.makePeaking
                                  { id: sfx id
                                  , frequency: asNumber frequency
                                  , gain: asNumber gain
                                  , q: asNumber q
                                  , parent
                                  }
                          , makePeriodicOsc:
                              \{ id, frequency, spec, onOff, parent } ->
                                ( pure $
                                    ai.makePeriodicOsc
                                      { id: sfx id
                                      , frequency: asNumber frequency
                                      , spec: spec
                                      , parent
                                      }
                                ) <|> (pure $ ai.setOnOff { id: sfx id, onOff })
                          , makePlayBuf:
                              \{ id
                               , playbackRate
                               , onOff
                               , duration
                               , bufferOffset
                               , buffer
                               , parent
                               } ->
                                ( pure $
                                    ai.makePlayBuf
                                      { id: sfx id
                                      , playbackRate: asNumber playbackRate
                                      , buffer
                                      , bufferOffset
                                      , duration
                                      , parent
                                      }
                                ) <|> (pure $ ai.setOnOff { id: sfx id, onOff })
                          , makeRecorder: \{ id, cb, parent } -> pure $
                              ai.makeRecorder { id: sfx id, cb, parent }
                          , makeSawtoothOsc:
                              \{ id, frequency, onOff, parent } ->
                                ( pure $
                                    ai.makeSawtoothOsc
                                      { id: sfx id
                                      , frequency: asNumber frequency
                                      , parent
                                      }
                                ) <|> (pure $ ai.setOnOff { id: sfx id, onOff })
                          , makeSinOsc: \{ id, frequency, onOff, parent } ->
                              ( pure $
                                  ai.makeSinOsc
                                    { id: sfx id
                                    , frequency: asNumber frequency
                                    , parent
                                    }
                              ) <|> (pure $ ai.setOnOff { id: sfx id, onOff })
                          , makeSquareOsc: \{ id, frequency, onOff, parent } ->
                              ( pure $
                                  ai.makeSquareOsc
                                    { id: sfx id
                                    , frequency: asNumber frequency
                                    , parent
                                    }
                              ) <|> (pure $ ai.setOnOff { id: sfx id, onOff })
                          , makeStereoPanner: \{ id, pan, parent } -> pure $
                              ai.makeStereoPanner
                                { id: sfx id
                                , pan: asNumber pan
                                , parent
                                }
                          , makeTriangleOsc:
                              \{ id, frequency, onOff, parent } ->
                                ( pure $
                                    ai.makeTriangleOsc
                                      { id: sfx id
                                      , frequency: asNumber frequency
                                      , parent
                                      }
                                ) <|> (pure $ ai.setOnOff { id: sfx id, onOff })
                          , makeWaveShaper:
                              \{ id, oversample, curve, parent } -> pure $
                                ai.makeWaveShaper
                                  { id: sfx id
                                  , oversample
                                  , curve
                                  , parent
                                  }
                          -- inputs come from the outside, so they do not need to be made
                          , makeInput: \_ -> empty
                          , connectXToY: \{ from, to } -> pure $
                              ai.connectXToY
                                { from: maybe' (\_ -> sfx from) identity
                                    (lookup from mp)
                                , to: maybe' (\_ -> sfx to) identity
                                    (lookup to mp)
                                }
                          , disconnectXFromY: \{ from, to } -> pure $
                              ai.disconnectXFromY
                                { from: maybe' (\_ -> sfx from) identity
                                    (lookup from mp)
                                , to: maybe' (\_ -> sfx to) identity
                                    (lookup to mp)
                                }
                          -- we never destroy inputs
                          , destroyUnit: \{ id } -> maybe'
                              (\_ -> pure $ ai.destroyUnit { id: sfx id })
                              (const empty)
                              (lookup id mp)
                          , setAnalyserNodeCb: isfx \ii -> pure $
                              ai.setAnalyserNodeCb ii
                          , setMediaRecorderCb: isfx \ii -> pure $
                              ai.setMediaRecorderCb ii
                          , setAudioWorkletParameter: isfx \ii -> pure $
                              ai.setAudioWorkletParameter ii
                          , setBuffer: isfx \ii -> pure $ ai.setBuffer ii
                          , setConvolverBuffer: isfx \ii -> pure $
                              ai.setConvolverBuffer ii
                          , setPeriodicOsc: isfx \ii -> pure $ ai.setPeriodicOsc
                              ii
                          , setOnOff: isfx \ii -> pure $ ai.setOnOff ii
                          , setBufferOffset: isfx \ii -> pure $
                              ai.setBufferOffset ii
                          , setLoopStart: isfx \ii -> pure $ ai.setLoopStart ii
                          , setLoopEnd: isfx \ii -> pure $ ai.setLoopEnd ii
                          , setRatio: isfx \ii -> pure $ ai.setRatio ii
                          , setOffset: isfx \ii -> pure $ ai.setOffset ii
                          , setAttack: isfx \ii -> pure $ ai.setAttack ii
                          , setGain: isfx \ii -> pure $ ai.setGain ii
                          , setQ: isfx \ii -> pure $ ai.setQ ii
                          , setPan: isfx \ii -> pure $ ai.setPan ii
                          , setThreshold: isfx \ii -> pure $ ai.setThreshold ii
                          , setRelease: isfx \ii -> pure $ ai.setRelease ii
                          , setKnee: isfx \ii -> pure $ ai.setKnee ii
                          , setDelay: isfx \ii -> pure $ ai.setDelay ii
                          , setPlaybackRate: isfx \ii -> pure $
                              ai.setPlaybackRate ii
                          , setFrequency: isfx \ii -> pure $ ai.setFrequency ii
                          , setWaveShaperCurve: isfx \ii -> pure $
                              ai.setWaveShaperCurve ii
                          }
                          i
                      )
                      empty
                      instr

                  )
                  atts
              )
              <|> pure (connectXToY { from: sfx terminus, to: prnt })
      )
