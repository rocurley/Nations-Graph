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

import Control.DeepSeq

import Network.Wreq
import qualified Network.Wreq.Session as Sess

import qualified Data.Aeson.Types as ATypes

getWiki :: Sess.Session -> Wiki -> String -> ErrorHandlingT IO (String,T.Text,Maybe (Image, String))
getWiki sess wiki article = do
    let api = apiEndpoint wiki
    let opts = param "action" .~ ["query"] $
               param "prop" .~ ["imageinfo|revisions"] $
               param "rvprop" .~ ["content"] $
               param "iiprop" .~ ["url|extmetadata"] $
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
    let imageUrl = resp^?pages.key "imageinfo".nth 0.key "url"._String.to T.unpack
    let imagedescriptionUrl = resp^?pages.key "imageinfo".nth 0.key "descriptionurl"._String.to T.unpack
    let licenceStr = resp^?pages.key "imageinfo".nth 0.key "extmetadata".key "License".key "value"._String.to T.unpack
    let image = Image <$> imageUrl <*> imagedescriptionUrl
    return (title, source, (,) <$> image <*> licenceStr)

httpGetInfobox :: Sess.Session -> String -> ErrorHandlingT IO (String,Infobox)
httpGetInfobox sess target = do
  (cannonicalName,wiki,_) <- getWiki sess Wikipedia target
  --The endOfInput won't work unless the wiki parser is improved.
  --See the result for French Thrid Republic for a hint.
  --parse <- EitherT $ return $ (_Left%~WikiParseError) $ parseOnly (wikiParser<*endOfInput) wiki
  parse <- rebaseErrorHandling $ raiseError $  (_Left%~WikiParseError) $ AP.parseOnly wikiParser wiki
  infobox <- rebaseErrorHandling $ getInfobox (T.pack cannonicalName) parse
  return (cannonicalName,infobox)

httpGetImage :: Sess.Session -> String -> ErrorHandlingT IO  (Image, License)
httpGetImage sess imageName = do
  (_,wiki,imageAndLicenseMay) <- getWiki sess WikiCommons $ "file:" ++ imageName
  (image,licenseStr) <- rebaseErrorHandling $ raiseError $ note MissingImage imageAndLicenseMay
  parse <- rebaseErrorHandling $ raiseError $  (_Left%~WikiParseError) $ AP.parseOnly wikiParser wiki
  license <- rebaseErrorHandling $ raiseError $ note (UnknownLicense licenseStr) $ parseLicense licenseStr
  return (image, license)

