{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
-- | Main entry point to the backend server for Devalot.com.
module Main (main) where

--------------------------------------------------------------------------------
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Devalot.Backend.App (appInit)
import Snap (MonadSnap)
import qualified Snap.Http.Server.Config as Config
import Snap.Snaplet (serveSnaplet)

--------------------------------------------------------------------------------
conf :: MonadSnap m => Config.Config m a
conf = Config.setVerbose   False $
       Config.setErrorLog  (Config.ConfigIoLog stdoutLog) $
       Config.setAccessLog (Config.ConfigIoLog stdoutLog) $
       Config.setPort      8000 $
       Config.setHostname  "www.devalot.com" $
       Config.setBind      "0.0.0.0" $
       Config.defaultConfig
  where
    stdoutLog :: ByteString -> IO ()
    stdoutLog = ByteString.putStr . (<> "\n")

--------------------------------------------------------------------------------
main :: IO ()
main = serveSnaplet conf appInit
