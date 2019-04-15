{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

--------------------------------------------------------------------------------
module Devalot.Backend.App (app) where

--------------------------------------------------------------------------------
import Devalot.Backend.Feedback
import Network.Wai (Application)
import Servant.Server (serve)

--------------------------------------------------------------------------------
app :: Application
app = serve feedbackAPI feedbackServer
