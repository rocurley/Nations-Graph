{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module NationsGraph.HTTP (
    getWiki,
    httpGetInfobox,
    httpGetImage,
) where

import NationsGraph.Types
import NationsGraph.Wiki

import Data.Aeson.Lens
import Control.Lens

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS

import qualified Data.Attoparsec.Text as AP

import Safe
import Data.Maybe

import Control.Monad.Trans.Either
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class

import Control.Error.Util
import Control.Exception

import Network.Wreq
import qualified Network.Wreq.Session as Sess

import qualified Data.Aeson.Types as ATypes

getWiki :: Sess.Session -> Wiki -> String -> ErrorHandlingT IO (String,T.Text,Maybe String)
getWiki sess wiki article = do
    let api = apiEndpoint wiki
    let opts = param "action" .~ ["query"] $
               param "prop" .~ ["revisions"] $
               param "rvprop" .~ ["content"] $
               param "format" .~ ["json"] $
               param "redirects" .~ [""] $
               param "titles" .~ [T.pack article] $ defaults
    resp <- raiseError $ EitherT $ mapped._Left%~HTTPError $
      try (Sess.getWith opts sess api)
    let pages :: Traversal' (Response LBS.ByteString) ATypes.Value
        pages = responseBody.key "query".key "pages".members
    title <- raiseError $ failWith JsonParseError $
      (resp^?pages.key "title"._String.to T.unpack :: Maybe String)
    source <- raiseError $ failWith JsonParseError $
      resp^?pages.key "revisions".nth 0.key "*"._String
    let imageUrl = resp^?pages.key "imageinfo".key "url"._String.to T.unpack
    return (title,source,imageUrl)

httpGetInfobox :: Sess.Session -> String -> EitherT HistoryError (WriterT ErrorLog IO) (String,Infobox)
httpGetInfobox sess target =  mapEitherT (mapWriterT (`runReaderT` target)) $ do
  (cannonicalName,wiki,_) <- getWiki sess Wikipedia target
  --The endOfInput won't work unless the wiki parser is improved.
  --See the result for French Thrid Republic for a hint.
  --parse <- EitherT $ return $ (_Left%~WikiParseError) $ parseOnly (wikiParser<*endOfInput) wiki
  parse <- rebaseErrorHandling $ raiseError $  (_Left%~WikiParseError) $ AP.parseOnly wikiParser wiki
  infobox <- rebaseErrorHandling $ getInfobox parse
  return (cannonicalName,infobox)

httpGetImage :: Sess.Session -> String -> EitherT HistoryError (WriterT ErrorLog IO)  (String, Licence)
httpGetImage sess imageName = mapEitherT (mapWriterT (`runReaderT` imageName)) $ do
  (_,wiki,imageUrlMay) <- getWiki sess WikiCommons imageName
  imageUrl <- rebaseErrorHandling $ raiseError $ note MissingImage imageUrlMay
  parse <- rebaseErrorHandling $ raiseError $  (_Left%~WikiParseError) $ AP.parseOnly wikiParser wiki
  licence <- rebaseErrorHandling $ raiseError $ note UnknownLicence $
    headMay $ filter (\ licence -> isJust $ findTemplate (licenceTemplateName licence) parse) [PdSelf]
  return (imageUrl, licence)

