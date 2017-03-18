{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
module FeedbackSpec (spec) where

--------------------------------------------------------------------------------
import Data.ByteString (ByteString)
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T
import Devalot.Backend.Snaplets.Feedback
import Snap.Internal.Http.Types
import Snap.Snaplet.Test (runHandler)
import Snap.Test (postRaw)
import qualified Snap.Types.Headers as SH
import Test.HUnit (assertFailure, assertEqual)
import Test.Hspec

--------------------------------------------------------------------------------
-- | Main entry point.
spec :: Spec
spec = do
  describe "Sending in JSON" $ do
    it "With good JSON" $ do
      submitJSON goodJSON $ \res -> do
        assertEqual "rspStatus" 200 (rspStatus res)
        assertEqual "rspContentLength" (Just 0) (rspContentLength res)
        assertContentType "application/json" res

    it "With bad JSON" $ do
      submitJSON badJSON $ \res -> do
        assertEqual "rspStatus" 400 (rspStatus res)

--------------------------------------------------------------------------------
-- | All values present, but the secret key is wrong.
goodJSON :: ByteString
goodJSON = "{\"name\": \"x\", \"email\": \"x\", \"text\": \"x\", \"key\": \"bm9zcGFtCg\"}"

--------------------------------------------------------------------------------
-- | Missing the secret key.
badJSON :: ByteString
badJSON = "{\"name\": \"x\", \"email\": \"x\", \"text\": \"x\"}"

--------------------------------------------------------------------------------
-- | Send JSON through to the Feedback snaplet and run the IO action
-- giving it access to the response.
submitJSON :: ByteString -> (Response -> IO ()) -> IO ()
submitJSON json action = do
  answer <- runHandler (Just "test") post feedbackHandler feedbackInit

  case answer of
    Left err  -> assertFailure $ T.unpack err
    Right res -> action res

  where
    post = postRaw "feedback.json" "application/json" json

--------------------------------------------------------------------------------
-- | Ensure that the Content-Length header matches the given value.
assertContentType :: ByteString -> Response -> IO ()
assertContentType expect res =
  case SH.lookup (CI.mk "Content-Type") (rspHeaders res) of
    Just x -> assertEqual "Content-Type" expect x
    _      -> assertFailure "missing Content-Type"
