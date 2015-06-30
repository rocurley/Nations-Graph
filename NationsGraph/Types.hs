{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}

module NationsGraph.Types (
    Wiki(..),
    WikiMarkup(..),
    WikiNode(..),
    Infobox(..),
    AutoLinkedFlag(..),
    HistoryError(..),
    ErrorContext(..),
    ErrorHandling(..),
    ErrorHandlingT(..),
    NationKey(..),
    NationNode(..),
    NationValue(..),
    SubdivisionNode(..),
    BuildingNationGraph(..),
    MonadEither(..),
    HttpException,
    ErrorLog(..),
    License(..),
    Image(..),
    apiEndpoint,
    rebaseErrorHandling,
    discardError,
    raiseError,
    nationname,
    nationStartYear,
    nationEndYear,
    position,
    infoboxToNode,
    parseLicense,
) where

import Safe

import Data.Maybe
import Data.List
import Data.List.NonEmpty
import qualified Data.Map.Strict as SMap
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)

import qualified Data.Text as T

import Test.QuickCheck

import Data.Aeson hiding ((.=))

import Control.Lens

import Network.HTTP.Client (HttpException)

import Control.Monad.Writer
import Control.Monad.Trans.Either
import Control.Monad.Trans.Reader

import Control.Error.Util

import Control.Applicative

import GHC.Generics (Generic)
import Control.DeepSeq

data Wiki = Wikipedia | WikiCommons

apiEndpoint :: Wiki -> String
apiEndpoint Wikipedia = "http://en.wikipedia.org/w/api.php"
apiEndpoint WikiCommons = "http://commons.wikimedia.org/w/api.php"

newtype WikiMarkup = WikiMarkup{wikiList ::NonEmpty WikiNode} deriving (Show)

data WikiNode = WikiText T.Text |
            WikiTemplate T.Text [WikiMarkup] (Map T.Text WikiMarkup) |
            WikiLink T.Text [WikiMarkup]|
            WikiHTMLTag T.Text (Map T.Text T.Text)|
            WikiComment T.Text
            deriving (Show)

data HistoryError = HTTPError HttpException |
                    JsonParseError |
                    WikiParseError String |
                    MissingInfobox |
                    DoubleInfobox |
                    InfoboxInterpretationError |
                    PropInterpretationError T.Text |
                    MissingInfoboxFieldError T.Text |
                    MissingImage |
                    UnknownLicense String deriving Show

instance NFData HistoryError where
    rnf (HTTPError httpError) = seq httpError ()
    rnf JsonParseError = ()
    rnf (WikiParseError err) = deepseq err ()
    rnf MissingInfobox = ()
    rnf InfoboxInterpretationError = ()
    rnf (PropInterpretationError text) = deepseq text ()
    rnf (MissingInfoboxFieldError text) = deepseq text ()
    rnf MissingImage = ()
    rnf (UnknownLicense str) = deepseq str ()

data AutoLinkedFlag = AutoLinked | NotAutoLinked

type ErrorContext = String

newtype ErrorLog = ErrorLog (SMap.Map ErrorContext [HistoryError]) deriving (Show,Generic)

instance NFData ErrorLog

instance Monoid ErrorLog where
  mempty = ErrorLog (SMap.empty)
  mappend (ErrorLog a) (ErrorLog b) = ErrorLog $ SMap.unionWith (++) a b

type ErrorHandling = ErrorHandlingT Identity

type ErrorHandlingT m = EitherT HistoryError (ReaderT ErrorContext (WriterT ErrorLog m))

liftErrorHandlingT :: Monad m => m a -> ErrorHandlingT m a
liftErrorHandlingT = lift . lift . lift

class (Monad e, Monad m) => MonadEither l m e | e -> l m where
  runMonadEitherT :: e a -> m (Either l a)

instance Monad m => MonadEither l m (EitherT l m) where
  runMonadEitherT = runEitherT

instance MonadEither l Identity (Either l) where
  runMonadEitherT = return

rebaseErrorHandling :: Monad m => ErrorHandling a -> ErrorHandlingT m a
rebaseErrorHandling = mapEitherT $ mapReaderT $ mapWriterT $ return . runIdentity

makeOptional :: Monad m => ErrorHandlingT m a -> ErrorHandlingT m (Maybe a)
makeOptional thing = do
  thing2 <- EitherT $ return <$> runEitherT thing
  rebaseErrorHandling $ discardError thing2

discardError :: MonadEither HistoryError m e=> e a -> ErrorHandlingT m  (Maybe a)
discardError eitherT = do
  either <- liftErrorHandlingT $ runMonadEitherT eitherT
  case either of
    Right a -> return (Just a)
    Left err -> do
      context <- lift $ ask
      lift $ tell $ ErrorLog $ SMap.singleton context [err]
      return Nothing

raiseError :: MonadEither HistoryError m e => e a -> ErrorHandlingT m a
raiseError eitherT = do
  either <- liftErrorHandlingT $ runMonadEitherT eitherT
  case either of
    Right a -> return a
    Left err -> left err -- Errors will be logged higher up.

type NationKey = String

type URL = String

data NationNode = NationNode
    {
        _nationValue :: NationValue,
        _nationPrecursors :: Set NationKey,
        _nationSuccessors :: Set NationKey
    } deriving (Show, Generic)

instance NFData NationNode

instance Arbitrary NationNode where
    arbitrary = NationNode <$> arbitrary <*>
        fmap Set.fromList arbitrary <*>
        fmap Set.fromList arbitrary

data NationValue = NationValue
    {
        _nationname :: String,
        _nationStartYear :: Maybe Int,
        _nationEndYear :: Maybe Int,
        _position :: Maybe (Float, Float),
        _wikiArticle :: String,
        _flag :: Maybe Image
    } deriving (Show, Ord, Eq, Generic)

instance NFData NationValue

data Image = Image
  {
    _imageDirectURL  :: URL,
    _imageLandingURL :: URL
  } deriving (Show, Ord, Eq, Generic)

instance Arbitrary Image where
  arbitrary = Image <$> arbitrary <*> arbitrary

instance ToJSON Image where
    toJSON (Image direct landing) = object [("directURL", toJSON direct), ("landingURL", toJSON landing)]

instance FromJSON Image where
    parseJSON (Object json) = do
        direct <- json.:"directURL"
        landing <- json.: "landingURL"
        return $ Image direct landing

instance NFData Image

makeLenses ''NationValue
instance Arbitrary NationValue where
    arbitrary = NationValue <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

data SubdivisionNode = SubdivisionNode
    {
        _subdivisionValue :: NationValue,
        _subdivisionPossibleParents :: [NationKey],
        _subdivisionPrecursors :: Set NationKey,
        _subdivisionSuccessors :: Set NationKey
    } deriving (Show, Generic)

instance NFData SubdivisionNode

data BuildingNationGraph = BuildingNationGraph {
    _nations :: (SMap.Map NationKey NationNode),
    _subdivisions :: (SMap.Map NationKey SubdivisionNode),
    _synonyms :: (SMap.Map String NationKey),
    _todo :: [String]} deriving (Show,Generic)

instance NFData BuildingNationGraph

data Infobox = Infobox{
  _infoboxName :: String,
  _infoboxCommonName :: Maybe String,
  _infoboxStartYear :: Maybe Int,
  _infoboxEndYear :: Maybe Int,
  _infoboxFlagName :: Maybe String,
  _infoboxPrecursors :: [String],
  _infoboxSuccessors :: [String],
  _infoboxSubdivision :: Maybe Subdivision
} deriving (Show, Generic)

instance NFData Infobox

infoboxToNode :: Monad m => (String -> ErrorHandlingT m Image) -> Infobox -> String ->
  ErrorHandlingT m (Either NationNode SubdivisionNode)
infoboxToNode getFlag (Infobox name commonName sYear eYear maybeFlagName precursors successors subdivision) articleName = do
  let
    flagNameError :: Monad m => ErrorHandlingT m String --Note that it is not bound in the do block.
    flagNameError = raiseError $ EitherT $ return $
      note (MissingInfoboxFieldError "Nothing providing a flag name available") $
      maybeFlagName <|> fmap (\name -> "Flag of " ++ name ++ ".svg") commonName
  flag <- makeOptional $ do
    flagName <- flagNameError
    getFlag flagName
  let
    nationValue = NationValue name sYear eYear Nothing articleName flag
    node = case subdivision of
      Nothing -> Left $ NationNode nationValue (Set.fromList precursors) (Set.fromList successors)
      Just possibleParents -> Right $ SubdivisionNode nationValue possibleParents (Set.fromList precursors) (Set.fromList successors)
  return node

type Subdivision = [String]

data License = PD|CC0|CC_BY Float|CC_BY_SA Float deriving (Show,Generic)

instance NFData License

parseLicense :: String -> Maybe License
parseLicense "pd" = Just PD
parseLicense "cc0" = Just CC0
parseLicense licenseString = headMay [license $ read versionString |
                               (license,prefix)<-[(CC_BY_SA,"cc-by-sa-"),(CC_BY,"cc-by-")],
                               versionString <- maybeToList $ stripPrefix prefix licenseString]
