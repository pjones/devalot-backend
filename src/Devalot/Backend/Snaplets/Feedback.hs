{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
module Devalot.Backend.Snaplets.Feedback
       ( Feedback
       , feedbackInit
       , feedbackHandler -- FIXME: remove this after fixing Snap
       ) where

--------------------------------------------------------------------------------
import Control.Applicative (empty)
import Control.Monad.Reader.Class (ask)
import Control.Monad.Trans (liftIO)
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import Network.Mail.Mime
import Snap.Core
import Snap.Snaplet

--------------------------------------------------------------------------------
data Feedback = Feedback
  { feedbackKey  :: String -- ^ The secret key to expect.
  , feedbackLive :: Bool   -- ^ Actually send mail or not.
  } deriving (Show)

--------------------------------------------------------------------------------
data Message = Message
  { fbName  :: String -- ^ The name of the person sending the feedback.
  , fbEmail :: String -- ^ The email address of the sender.
  , fbText  :: String -- ^ The message body.
  , fbKey   :: String -- ^ A stupid static anti-spam key.
  } deriving (Show)

--------------------------------------------------------------------------------
instance FromJSON Message where
  parseJSON (Object v) =
    Message <$> v .: "name"
            <*> v .: "email"
            <*> v .: "text"
            <*> v .: "key"
  parseJSON _ = empty

--------------------------------------------------------------------------------
staticKey :: String
staticKey = "bm9zcGFtCg"

--------------------------------------------------------------------------------
-- | The built in routes for this snaplet.
routes :: [(ByteString, Handler b Feedback ())]
routes =
  [ -- Incoming from contact.html, sends email to me
    ("feedback.json", feedbackHandler)
  ]

--------------------------------------------------------------------------------
-- | Application initializer.
feedbackInit :: SnapletInit b Feedback
feedbackInit = makeSnaplet "feedback" "Form mailer" Nothing $ do
  env <- getEnvironment
  addRoutes routes

  return $! Feedback { feedbackKey  = staticKey
                     , feedbackLive = env == "production"
                     }

--------------------------------------------------------------------------------
feedbackHandler :: Handler b Feedback ()
feedbackHandler = do
  body <- readRequestBody 2048
  fb   <- ask

  let live = feedbackLive fb
      key  = feedbackKey  fb
      msg  = decode body
      bad  = modifyResponse (setResponseStatus 400 "Bad Request")

  -- Decide what to do based on three conditions.
  case (live, msg, (== key) . fbKey <$> msg) of
    (_,     Nothing, _         ) -> bad -- Bad decode.
    (_,     Just _,  Nothing   ) -> bad -- Bad decode or bad key.
    (_,     Just _,  Just False) -> bad -- Bad key.
    (True,  Just x,  Just  True) -> liftIO $ sendFeedbackMail x
    (False, Just x,  Just  True) -> liftIO $ putStrLn ("Message from: " ++ fbName x)

  -- Apache fails unless you set Content-Length to 0.
  modifyResponse $ setContentLength 0 . setContentType "application/json"

--------------------------------------------------------------------------------
sendFeedbackMail :: Message -> IO ()
sendFeedbackMail = renderSendMail . makeMail

--------------------------------------------------------------------------------
makeMail :: Message -> Mail
makeMail a = m { mailTo      = [to]
               , mailParts   = [[body]]
               , mailHeaders = hdrs
               }
  where m    = emptyMail from
        from = Address (Just . T.pack $ fbName a) (T.pack $ fbEmail a)
        to   = Address (Just "Peter Jones") "pjones@devalot.com"
        subj = "[Devalot] Feedback Message"
        hdrs = [("Subject", subj), ("X-Devalot", "Yes")]
        body = Part { partType = "text/plain; charset=utf-8"
                    , partEncoding = QuotedPrintableText
                    , partFilename = Nothing
                    , partHeaders  = []
                    , partContent  = LT.encodeUtf8 . LT.pack $ fbText a
                    }
