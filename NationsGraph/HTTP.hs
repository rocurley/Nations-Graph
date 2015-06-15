{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module NationsGraph.HTTP (
    getWiki,
    httpGetInfobox,
) where

import NationsGraph.Types
import NationsGraph.Wiki

import Data.Aeson.Lens
import Control.Lens

import qualified Data.Text as T

import qualified Data.Attoparsec.Text as AP

import Control.Monad.Trans.Either
import Control.Error.Util
import Control.Monad.IO.Class
import Control.Exception

import Network.Wreq
import qualified Network.Wreq.Session as Sess


wikiAPI = "http://en.wikipedia.org/w/api.php" :: String

getWiki :: Sess.Session -> String -> ErrorHandlingT IO (String,T.Text)
getWiki sess article = do
    let underscoresToSpaces '_' = ' '
    let underscoresToSpaces c = c
    let tweakedName = map underscoresToSpaces $ takeWhile (/= '#') article
    let opts = param "action" .~ ["query"] $
               param "prop" .~ ["revisions"] $
               param "rvprop" .~ ["content"] $
               param "format" .~ ["json"] $
               param "titles" .~ [T.pack tweakedName] $ defaults
    rawResp <- liftIO $ try (Sess.getWith opts sess wikiAPI)
    resp <- raiseError rawResp
    source <- raiseError $ failWith JsonParseError $
      (resp^?responseBody
        .key "query"
        .key "pages"
        .members
        .key "revisions"
        .nth 0
        .key "*"
        ._String  :: Maybe T.Text)
    case AP.parseOnly redirectParser source of
        Right link -> getWiki sess $ T.unpack link
        Left _ -> return (tweakedName,source)

httpGetInfobox :: Sess.Session -> String -> ErrorHandlingT IO (String,Infobox)
httpGetInfobox sess target =  do
  (cannonicalName,wiki) <- raiseError <$> getWiki sess target
  --The endOfInput won't work unless the wiki parser is improved.
  --See the result for French Thrid Republic for a hint.
  --parse <- EitherT $ return $ (_Left%~WikiParseError) $ parseOnly (wikiParser<*endOfInput) wiki
  parse <- raiseError $  (_Left%~WikiParseError) $ AP.parseOnly wikiParser wiki
  infobox <- getInfobox parse
  return (cannonicalName,infobox)
