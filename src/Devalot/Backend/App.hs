{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

--------------------------------------------------------------------------------
module Devalot.Backend.App (App, appInit) where

--------------------------------------------------------------------------------
import Control.Lens
import Data.ByteString (ByteString)
import Devalot.Backend.Snaplets.Feedback (Feedback, feedbackInit)
import Snap.Util.FileServe (serveDirectory)
import Snap.Snaplet

--------------------------------------------------------------------------------
-- | Data type to represent the base of the Devalot site.
data App = App
  { _feedback :: Snaplet Feedback
  }

--------------------------------------------------------------------------------
-- | Template Haskell to create lenses for the 'App' type.
makeLenses ''App

------------------------------------------------------------------------------
-- | The built in routes for this snaplet.
routes :: [(ByteString, Handler App App ())]
routes =
  [("", serveDirectory "www") -- Only used for testing.
  ]

--------------------------------------------------------------------------------
-- | Application initializer.
appInit :: SnapletInit App App
appInit = makeSnaplet "app" "Base Devalot application" Nothing $ do
    fb   <- nestSnaplet "json" feedback feedbackInit
    addRoutes routes
    return $! App fb
