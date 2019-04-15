{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
-- | Main entry point to the backend server for Devalot.com.
module Main (main) where

--------------------------------------------------------------------------------
import Network.Wai.Handler.Warp (run)
import Devalot.Backend.App (app)

--------------------------------------------------------------------------------
main :: IO ()
main = run 8000 app
