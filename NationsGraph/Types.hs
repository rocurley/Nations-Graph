{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}

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
    Licence(..),
    apiEndpoint,
    rebaseErrorHandling,
    discardError,
    raiseError,
    nationname,
    nationStartYear,
    nationEndYear,
    position,
    licenceTemplateName,
) where

import Data.List.NonEmpty
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)

import qualified Data.Text as T

import Test.QuickCheck

import Control.Lens

import Network.HTTP.Client (HttpException)

import Control.Monad.Writer
import Control.Monad.Trans.Either
import Control.Monad.Trans.Reader

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
                    UnknownLicence deriving Show

data AutoLinkedFlag = AutoLinked | NotAutoLinked

type ErrorContext = String

newtype ErrorLog = ErrorLog (Map ErrorContext [HistoryError]) deriving Show

instance Monoid ErrorLog where
  mempty = ErrorLog (Map.empty)
  mappend (ErrorLog a) (ErrorLog b) = ErrorLog $ Map.unionWith (++) a b

type ErrorHandling = ErrorHandlingT Identity

type ErrorHandlingT m = EitherT HistoryError (WriterT ErrorLog (ReaderT ErrorContext m))

liftErrorHandlingT :: Monad m => m a -> ErrorHandlingT m a
liftErrorHandlingT = lift . lift . lift

class (Monad e, Monad m) => MonadEither l m e | e -> l m where
  runMonadEitherT :: e a -> m (Either l a)

instance Monad m => MonadEither l m (EitherT l m) where
  runMonadEitherT = runEitherT

instance MonadEither l Identity (Either l) where
  runMonadEitherT = return

rebaseErrorHandling :: Monad m => ErrorHandling a -> ErrorHandlingT m a
rebaseErrorHandling = mapEitherT $ mapWriterT $ mapReaderT $ return . runIdentity

discardError :: MonadEither HistoryError m e=> e a -> ErrorHandlingT m  (Maybe a)
discardError eitherT = do
  either <- liftErrorHandlingT $ runMonadEitherT eitherT
  case either of
    Right a -> return (Just a)
    Left err -> do
      context <- lift $ lift $ ask
      lift $ tell $ ErrorLog $ Map.singleton context [err]
      return Nothing

raiseError :: MonadEither HistoryError m e => e a -> ErrorHandlingT m a
raiseError eitherT = do
  either <- liftErrorHandlingT $ runMonadEitherT eitherT
  case either of
    Right a -> return a
    Left err -> left err -- Errors will be logged higher up.

type NationKey = String

data NationNode = NationNode
    {
        _nationValue :: NationValue,
        _nationPrecursors :: Set NationKey,
        _nationSuccessors :: Set NationKey
    }

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
        _wikiArticle :: String
    } deriving (Show, Ord, Eq)
makeLenses ''NationValue
instance Arbitrary NationValue where
    arbitrary = NationValue <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

data SubdivisionNode = SubdivisionNode
    {
        _subdivisionValue :: NationValue,
        _subdivisionPossibleParents :: [NationKey],
        _subdivisionPrecursors :: Set NationKey,
        _subdivisionSuccessors :: Set NationKey
    }

data BuildingNationGraph = BuildingNationGraph {
    _nations :: (Map NationKey NationNode),
    _subdivisions :: (Map NationKey SubdivisionNode),
    _synonyms :: (Map String NationKey),
    _todo :: [String]}

data Infobox = NationInfobox{
                    _name :: String,
                    _start_year :: Maybe Int,
                    _end_year :: Maybe Int,
                    _precursors :: [String],
                    _successors :: [String]} |

                SubdivisionInfobox{
                    _name :: String,
                    _start_year :: Maybe Int,
                    _end_year :: Maybe Int,
                    _precursors :: [String],
                    _successors :: [String],
                    _parentCandidates :: [String]} deriving Show

data Licence = PdSelf

licenceTemplateName :: Licence -> T.Text
licenceTemplateName PdSelf = "pd-self"
