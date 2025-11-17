----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE MultilineStrings           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE CPP                        #-}
----------------------------------------------------------------------------
module Main where
----------------------------------------------------------------------------
import           Miso hiding (model)
import qualified Miso.CSS as CSS
import qualified Miso.Html.Element as H
import qualified Miso.Html.Event as E
import qualified Miso.Html.Property as P
import           Miso.String (MisoString, ms)
import           Miso.Lens (Lens(..), lens, (-=), (+=), (^.))
import           Miso.Lens.TH (makeLenses)
----------------------------------------------------------------------------
-- | Component model state
data ParentModel
  = ParentModel
  { _parentCounter :: Int
  , _proxy :: Int
  } deriving (Show, Eq)
----------------------------------------------------------------------------
$(makeLenses ''ParentModel)
----------------------------------------------------------------------------
-- | Sum type for App events
data ParentAction
  = ParentAdd
  | ParentSubtract
  deriving (Show, Eq)
----------------------------------------------------------------------------
newtype ChildModel = ChildModel { _childCounter :: Int }
  deriving (Eq, Show)
----------------------------------------------------------------------------
$(makeLenses ''ChildModel)
----------------------------------------------------------------------------
-- | Sum type for App events
data ChildAction
  = ChildAdd
  | ChildSubtract
  deriving (Show, Eq)
----------------------------------------------------------------------------
-- | Entry point for a miso application
main :: IO ()
main = run (startApp Main.topLevel)
----------------------------------------------------------------------------
topLevel = (component () noop viewTop)
#ifndef WASM
  { styles = [ Href "assets/style.css" ]
  }
#endif
  where
    viewTop () =
      H.div_
      []
      [ githubStar
      , H.h1_
        [ CSS.style_ [ CSS.fontFamily "monospace" ] ]
        [ "🍜 💥 miso-reactive"
        ]
      , H.div_
        [ P.className "container"
        ]
        [ H.div_ [ P.className "box" ] +> box uniParent (parentComponent uniParent)
        , H.div_ [ P.className "box" ] +> box uniChild (parentComponent uniChild)
        , H.div_ [ P.className "box" ] +> box bidiParentChild (parentComponent bidiParentChild)
        , H.div_ [ P.className "box" ] +> box bidiSibling (parentComponent bidiSibling)
        ]
      ]
----------------------------------------------------------------------------
githubStar :: View parent action
githubStar = H.iframe_
    [ P.title_ "GitHub"
    , P.height_ "30"
    , P.width_ "170"
    , textProp "scrolling" "0"
    , textProp "frameborder" "0"
    , P.src_
      "https://ghbtns.com/github-btn.html?user=haskell-miso&repo=miso-reactive&type=star&count=true&size=large"
    ]
    []
----------------------------------------------------------------------------
data Example
  = Example
  { exampleBindings    :: [ Binding ParentModel ChildModel ]
  , exampleHeader      :: MisoString
  , exampleDescription :: MisoString
  , exampleSource      :: MisoString
  }
----------------------------------------------------------------------------
-- | Bidirectional binding between parent and child
bidiParentChild :: Example
bidiParentChild = Example
  { exampleBindings =
      [ parentCounter <--> childCounter
      ]
  , exampleHeader = "Bidirectional (parent to child, child to parent)"
  , exampleDescription = """
         In this example any changes to parent state are automatically
         propagated down to children. Simulataneously, any changes to children state
         are propagated to the parent and by extension, all siblings as well.
      """
  , exampleSource = """
         // Code Example
         data ParentModel
           = ParentModel
           { _parentCounter :: Int
           , _proxy :: Int
           } deriving (Show, Eq)

         $(makeLenses ''ParentModel)

          newtype ChildModel
            = ChildModel
            { _childCounter :: Int
            } deriving (Eq, Show)

         $(makeLenses ''ChildModel)

         child
           :: Component ParentModel ChildModel ChildAction
         child = childComponent
           { bindings =
             [ parentCounter <--> childCounter
             ]
           }
      """                                 
  }
----------------------------------------------------------------------------
-- | Unidirecational binding between parent and child
uniParent :: Example
uniParent = Example
  { exampleBindings = [ parentCounter --> childCounter ]
  , exampleHeader = "Unidirectional (parent-to-child)"
  , exampleDescription =
      """
         This example demonstrates unidirectional data flow where the
         parent field changes are synchronized to the child state. Children
         can still alter their state, but any received updates from the
         parent will immediately overwrite child state. Parent state remains unaffected
         by child state changes.
      """
  , exampleSource =
      """
         // Code Example
         data ParentModel
           = ParentModel
           { _parentCounter :: Int
           , _proxy :: Int
           } deriving (Show, Eq)

         $(makeLenses ''ParentModel)

          newtype ChildModel
            = ChildModel
            { _childCounter :: Int
            } deriving (Eq, Show)

         $(makeLenses ''ChildModel)

         child
           :: Component ParentModel ChildModel ChildAction
         child = childComponent
           { bindings =
               [ parentCounter --> childCounter
               ]
           }
     """
  }
----------------------------------------------------------------------------
-- | Unidirecational binding between child to parent
uniChild :: Example
uniChild = Example
  { exampleBindings = [ parentCounter <-- childCounter ]
  , exampleHeader = "Unidirectional (child-to-parent)"
  , exampleDescription =
      """
         This example demonstrates unidirectional data flow where the
         child state changes synchronize to the parent. The parent state
         is overwritten by whichever child changes its state first.
         Child states do not affect other sibling states. Parents can alter
         their own states, but will be immediately overwritten by any child
         state updates.
      """
  , exampleSource =
      """
         // Code Example
         data ParentModel
           = ParentModel
           { _parentCounter :: Int
           , _proxy :: Int
           } deriving (Show, Eq)

         $(makeLenses ''ParentModel)

          newtype ChildModel
            = ChildModel
            { _childCounter :: Int
            } deriving (Eq, Show)

         $(makeLenses ''ChildModel)

         child
           :: Component ParentModel ChildModel ChildAction
         child = childComponent
           { bindings =
             [ parentCounter <-- childCounter
             ]
           }
     """
  }
----------------------------------------------------------------------------
-- | Bidirectional binding between sibling (by way of parent)
bidiSibling :: Example
bidiSibling = Example
  { exampleBindings =
      [ proxy <--> childCounter
      ]
  , exampleHeader = "Bidirectional (sibling-to-sibling)"
  , exampleDescription =
      """
         This example demonstrates bidirectional sibling communication where the
         parent field is used as a proxy to relay state information between
         child siblings. The parent itself maintains its own state that is
         unaffected during the child sibling model synchronization.
      """
  , exampleSource =
      """
         // Code Example
         data ParentModel
           = ParentModel
           { _parentCounter :: Int
           , _proxy :: Int
           } deriving (Show, Eq)

         $(makeLenses ''ParentModel)

          newtype ChildModel
            = ChildModel
            { _childCounter :: Int
            } deriving (Eq, Show)

         $(makeLenses ''ChildModel)

         child
           :: Component ParentModel ChildModel ChildAction
         child = childComponent
           { bindings =
               [ proxy <--> childCounter
               ]
           }
      """
  }
----------------------------------------------------------------------------
-- | WASM export, required when compiling w/ the WASM backend.
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
----------------------------------------------------------------------------
-- | `Component` takes as arguments the initial model, update function, view function
parentComponent
  :: Example
  -> Component parent ParentModel ParentAction
parentComponent = component emptyModel updateModel . viewModel
  where
    updateModel = \case
      ParentAdd ->
        parentCounter += 1
      ParentSubtract ->
        parentCounter -= 1
----------------------------------------------------------------------------
-- | Empty application state
emptyModel :: ParentModel
emptyModel = ParentModel 0 0
----------------------------------------------------------------------------
-- | Constructs a virtual DOM from a model
-- viewModel :: ParentModel -> View ParentModel ParentAction
viewModel Example {..} m =
  H.div_
  [ P.className "counters-section"
  ]
  [ H.div_
    [ P.class_ "counter-example"
    ]
    [ H.h3_ [] [ "Parent" ]
    , H.div_
      [ P.class_ "counter"
      ]
      [ text $ ms (m ^. parentCounter)
      ]
    , H.div_
      []
      [ H.button_
        [ P.class_ "btn btn-increment"
        , E.onClick ParentAdd
        ]
        ["+"]
      , H.button_
        [ P.class_ "btn btn-decrement"
        , E.onClick ParentSubtract
        ]
        ["-"]
      ]
    ]
  , H.div_
    [ P.class_ "counter-example"
    ] +> (childComponent "Child 1") { bindings = exampleBindings }
  , H.div_
    [ P.class_ "counter-example"
    ] +> (childComponent "Child 2") { bindings = exampleBindings }
  ]
----------------------------------------------------------------------------
-- | Component used for distribution
childComponent :: MisoString -> Component ParentModel ChildModel ChildAction
childComponent name = (component (ChildModel 0) updateChildModel childView_)
  where
      childView_ :: ChildModel -> View ChildModel ChildAction
      childView_ m =
        H.div_
        [ P.className "counter-example"
        ]
        [ H.h3_ [] [ text name ]
        , H.div_
          [ P.class_ "counter"
          ]
          [ text $ ms (m ^. childCounter)
          ]
        , H.div_
          []
          [ H.button_
            [ P.class_ "btn btn-increment"
            , E.onClick ChildAdd
            ]
            [ "+"
            ]
          , H.button_
            [ P.class_ "btn btn-decrement"
            , E.onClick ChildSubtract
            ]
            [ "-"
            ]
          ]
        ]
----------------------------------------------------------------------------
-- | Updates model, optionally introduces side effects
updateChildModel :: ChildAction -> Effect ParentModel ChildModel ChildAction
updateChildModel = \case
  ChildAdd ->
    childCounter += 1
  ChildSubtract ->
    childCounter -= 1
----------------------------------------------------------------------------
box
  :: Eq model
  => Example 
  -> Component () model action1
  -> Component parent () action2
box Example {..} vcomp = component () noop $ \() ->
  H.div_
    [ P.class_ "box"
    ]
    [ H.div_
      [ P.class_ "box-header"
      ]
      [ text (ms exampleHeader)
      ]
    , H.div_
      [ P.class_ "box-content"
      ]
      [ H.div_
        [ P.class_ "counter-section"
        ] +> vcomp
      , H.div_
        [ P.class_ "code-section" ]
        [ H.div_
          [ P.class_ "description" ]
          [ H.h4_ []
            [ "Description"
            ]
          , H.p_  []
            [ text (ms exampleDescription)
            ]
          ]
        , H.div_
          [ P.class_ "code-block"
          ]
          [ H.pre_
            []
            [ text (ms exampleSource)
            ]
          ]
        ]
      ]
    ]
----------------------------------------------------------------------------
