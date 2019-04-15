{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

--------------------------------------------------------------------------------
module Devalot.Backend.Feedback
       ( feedbackServer
       , feedbackAPI
       ) where

--------------------------------------------------------------------------------
import Control.Applicative (empty)
import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Control.Monad.Trans (liftIO)
import Data.Aeson
import Data.Proxy
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import Network.Mail.Mime
import Servant.API
import Servant.Server
import System.Directory (findExecutable)

--------------------------------------------------------------------------------
type FeedbackAPI = "feedback.json" :> ReqBody '[JSON] Message :> Post '[JSON] NoContent

--------------------------------------------------------------------------------
feedbackAPI :: Proxy FeedbackAPI
feedbackAPI = Proxy

--------------------------------------------------------------------------------
feedbackServer :: Server FeedbackAPI
feedbackServer = feedback

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
feedback :: Message -> Handler NoContent
feedback msg = do
  unless (fbKey msg == staticKey) (throwError err401)
  liftIO (sendFeedbackMail msg)
  return NoContent

--------------------------------------------------------------------------------
sendFeedbackMail :: Message -> IO ()
sendFeedbackMail msg = do
    exec <- findExecutable "sendmail"

    case exec of
      Nothing -> return ()
      Just f  -> renderMail' (makeMail msg) >>= sendmailCustom f ["-t"]

--------------------------------------------------------------------------------
makeMail :: Message -> Mail
makeMail a = m { mailTo      = [to]
               , mailParts   = [[body]]
               , mailHeaders = hdrs
               }
  where m = emptyMail from
        sender = Address (Just . T.pack $ fbName a) (T.pack $ fbEmail a)
        from = Address (Just "No Reply") "noreply@devalot.com"
        to = Address (Just "Peter Jones") "pjones@devalot.com"
        subj = "[Devalot] Feedback Message"
        hdrs = [ ("Subject", subj)
               , ("Reply-To", renderAddress sender)
               , ("X-Devalot", "Yes")
               ]
        body = Part { partType = "text/plain; charset=utf-8"
                    , partEncoding = QuotedPrintableText
                    , partFilename = Nothing
                    , partHeaders  = []
                    , partContent  = LT.encodeUtf8 . LT.pack $ fbText a
                    }
