module Component where

import Prelude

import Data.Array (singleton)
import Data.Const (Const)
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Parser.Halogen (toHalogen)

data Action
  = ParseInput String

type State =
  { on :: Boolean
  , raw :: String
  }

testHtml :: String
testHtml = """<div class="main" id="zero">
  <div id="one">
    Text!
  </div>
  <br class="clearfix" />
  <div id="two" class="">
    Text!!
  </div>
  <form>
    <input type="text"/>
    <select>
      <option value="1">PureScript</option>
      <option value="2">GHCJS</option>
      <option value="3">ClojureScript</option>
    </select>
  </form>
</div>
"""

class' :: ∀ r a. String -> HH.IProp ("class" :: String | r) a
class' s = HP.class_ $ wrap s

classes' :: ∀ r a. Array String -> HH.IProp ("class" :: String | r) a
classes' ss = HP.classes $ map wrap ss

component :: forall m. H.Component HH.HTML (Const Void) Unit Void m
component =
  H.mkComponent
    { initialState: const initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        }
    }
  where

  initialState :: State
  initialState = { on: false, raw: testHtml }

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div [ class' "container" ]
      [ HH.h1 [ class' "title" ]
        [ selector [ "HTML"]
        , HH.text " → "
        , selector ["Halogen"]
        ]
      , HH.section []
        [ HH.label_ [ HH.text "HTML input:"]
        , HH.textarea [ HP.value state.raw, HE.onValueChange (Just <<< ParseInput) ]
        ]
      , HH.section []
        [ HH.label_ [ HH.text "Halogen output:"]
        , HH.pre_ [ HH.text parsed ]
        ]
      ]
    where
      parsed = either show identity $ toHalogen state.raw
      selector opts = HH.select [ class' "picker" ] $ (HH.option_ <<< singleton <<< HH.text) <$> opts

  handleAction :: Action -> H.HalogenM State Action () Void m Unit
  handleAction = case _ of
    ParseInput raw -> do
      H.modify_ \state -> state { raw = raw }
