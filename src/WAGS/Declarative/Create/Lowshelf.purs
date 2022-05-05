module WAGS.Declarative.Create.Lowshelf where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Variant (match)
import Data.Variant.Maybe (just)
import FRP.Event (Event, bang, keepLatest, makeEvent, subscribe)
import WAGS.Common.Parameters.Lowshelf as Parameters
import WAGS.Core as Core
import WAGS.Declarative.Create.Gain (tmpResolveAU)

lowshelf
  :: forall i aud (outputChannels :: Type) lock payload
   . Parameters.InitialLowshelf i
  => Core.Mix aud (Core.Audible outputChannels lock payload)
  => i
  -> Event (Core.Lowshelf lock payload)
  -> aud
  -> Core.Node outputChannels lock payload
lowshelf i' atts elts = Core.Node go
  where
  Core.InitializeLowshelf i = Parameters.toInitializeLowshelf i'
  go parent di@(Core.AudioInterpret { ids, deleteFromCache, makeLowshelf, setFrequency, setGain }) = makeEvent \k -> do
    me <- ids
    parent.raiseId me
    map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
      bang
        ( makeLowshelf
            { id: me, parent: parent.parent, scope: parent.scope, frequency: i.frequency, gain: i.gain }
        )
        <|>
          ( keepLatest $ map
              ( \(Core.Lowshelf e) -> match
                  { frequency: tmpResolveAU parent.scope di (setFrequency <<< { id: me, frequency: _ })
                  , gain: tmpResolveAU parent.scope di (setGain <<< { id: me, gain: _ })
                  }
                  e
              )
              atts
          )
        <|> Core.__internalWagsFlatten (just me) parent.scope di (Core.mix elts)

lowshelf_
  :: forall i aud (outputChannels :: Type) lock payload
   . Parameters.InitialLowshelf i
  => Core.Mix aud (Core.Audible outputChannels lock payload)
  => i
  -> aud
  -> Core.Node outputChannels lock payload
lowshelf_ i a = lowshelf i empty a
