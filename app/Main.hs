----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE CPP                        #-}
----------------------------------------------------------------------------
module Main where
----------------------------------------------------------------------------
import Miso hiding (model)
import Miso.String (MisoString, ms)
import Miso.Lens
----------------------------------------------------------------------------
-- | Component model state
data ParentModel
  = ParentModel
  { _counter :: Int
  , _childCounter :: Int
  } deriving (Show, Eq)
----------------------------------------------------------------------------
counter :: Lens ParentModel Int
counter = lens _counter $ \record field -> record { _counter = field }
----------------------------------------------------------------------------
childCounter :: Lens ParentModel Int
childCounter = lens _childCounter $ \record field -> record { _childCounter = field }
----------------------------------------------------------------------------
-- | Sum type for App events
data ParentAction
  = ParentAdd
  | ParentSubtract
  deriving (Show, Eq)
----------------------------------------------------------------------------
data ChildModel = ChildModel { _x :: Int }
  deriving (Eq, Show)
----------------------------------------------------------------------------
x :: Lens ChildModel Int
x = lens _x $ \record field -> record { _x = field }
----------------------------------------------------------------------------
-- | Sum type for App events
data ChildAction
  = ChildAdd
  | ChildSubtract
  deriving (Show, Eq)
----------------------------------------------------------------------------
-- | Entry point for a miso application
main :: IO ()
main = run (startApp Main.parent)
----------------------------------------------------------------------------
-- | WASM export, required when compiling w/ the WASM backend.
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
----------------------------------------------------------------------------
-- | `component` takes as arguments the initial model, update function, view function
parent :: App ParentModel ParentAction
parent = (component emptyModel updateModel viewModel)
#ifndef WASM
  { styles = [ Href "https://cdnjs.cloudflare.com/ajax/libs/milligram/1.4.1/milligram.css" ]
  }
#endif
----------------------------------------------------------------------------
-- | Empty application state
emptyModel :: ParentModel
emptyModel = ParentModel 0 0
----------------------------------------------------------------------------
-- | Updates model, optionally introduces side effects
updateModel :: ParentAction -> Transition ParentModel ParentAction
updateModel = \case
  ParentAdd ->
    counter += 1
  ParentSubtract ->
    counter -= 1
----------------------------------------------------------------------------
-- | Constructs a virtual DOM from a model
viewModel :: ParentModel -> View ParentModel ParentAction
viewModel (ParentModel parentState _) = div_ []
  [ h1_ [] [ "🍜 💥 miso-reactive" ]
  , h4_ [] [ "This example demonstrates sibling communication via reactivity" ]
  , h5_ [] [ "The child components are synchronized on any event raised via the parent, the parent also retains its own state" ]
  , h5_ [] [ "Child 1 <--> Parent <--> Child 2" ]
  , br_ []
  , h2_ [] [ "Parent Component" ]
  , button_
    [ onClick ParentAdd ]
    [ text "+" ]
  , text (ms parentState)
  , button_
    [ onClick ParentSubtract ]
    [ text "-" ]
  , br_ []
  , div_
    [ id_ "Child sibling components"
    ]
    [ div_
      [ key_ @MisoString "component-1"
      ] +> childComponent "one"
    , br_ []
    , div_
      [ key_ @MisoString "component-2"
      ] +> childComponent "two"
    ]
  ]
----------------------------------------------------------------------------
-- | Component used for distribution
childComponent :: MisoString -> Component ParentModel ChildModel ChildAction
childComponent name = (component (ChildModel 0) updateChildModel childView_)
  { bindings =
      [ childCounter <--> x
      ]
  } where
      childView_ :: ChildModel -> View ChildModel ChildAction
      childView_ (ChildModel x_) =
        div_
        []
        [ h3_ [] [ text ("Child Component " <> name) ]
        , button_ [ onClick ChildAdd ] [ "+" ]
        , text (ms x_)
        , button_ [ onClick ChildSubtract ] [ "-" ]
        ]
----------------------------------------------------------------------------
-- | Updates model, optionally introduces side effects
updateChildModel :: ChildAction -> Effect ParentModel ChildModel ChildAction
updateChildModel = \case
  ChildAdd ->
    x += 1
  ChildSubtract ->
    x -= 1
----------------------------------------------------------------------------
