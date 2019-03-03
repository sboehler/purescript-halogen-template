module Component where

import Prelude

import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Effect.Aff(Aff)
import Effect.Random(random)

data Query a = GetNumber a

type State = { n :: Number }

component :: H.Component HH.HTML Query Unit Void Aff
component =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { n: 10.0 }

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div_
      [ HH.h1_
          [ HH.text "Generate random numbers" ]
      , HH.p_
          [ HH.text "Why not toggle this button:" ]
      , HH.button
          [ HE.onClick (HE.input_ GetNumber) ]
          [ HH.text "Generate a new number"]
      , HH.div_
        [HH.text $ "The number is " <> show state]

      ]

  eval :: Query ~> H.ComponentDSL State Query Void Aff
  eval = case _ of
    GetNumber next -> do
      n' <- H.liftEffect random
      _ <- H.modify  \s -> s { n = n' }
      pure next
