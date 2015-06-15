{-# LANGUAGE TemplateHaskell #-}

module NationsGraph.Types (
    Wiki(..),
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
    HttpException,
    discardError,
    raiseError,
    nationname,
    nationStartYear,
    nationEndYear,
    position,
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

newtype Wiki = Wiki{wikiList ::NonEmpty WikiNode} deriving (Show)

data WikiNode = WikiText T.Text |
            WikiTemplate T.Text [Wiki] (Map T.Text Wiki) |
            WikiLink T.Text [Wiki]|
            WikiHTMLTag T.Text (Map T.Text T.Text)|
            WikiComment T.Text
            deriving (Show)

data HistoryError = HTTPError HttpException |
                    JsonParseError |
                    WikiParseError String |
                    MissingInfobox |
                    DoubleInfobox |
                    InfoboxInterpretationError|
                    PropInterpretationError T.Text|
                    MissingInfoboxFieldError T.Text deriving Show

data AutoLinkedFlag = AutoLinked | NotAutoLinked

type ErrorContext = String

newtype ErrorLog = ErrorLog (Map ErrorContext [HistoryError]) deriving Show

instance Monoid ErrorLog where
  mempty = ErrorLog (Map.empty)
  mappend (ErrorLog a) (ErrorLog b) = ErrorLog $ Map.unionWith (++) a b

type ErrorHandling = ErrorHandlingT Identity

type ErrorHandlingT m = ReaderT ErrorContext (EitherT HistoryError (WriterT ErrorLog m))

discardError ::  Monad m => EitherT HistoryError m a -> ErrorHandlingT m  (Maybe a)
discardError (Left err) = do
  context <- ask
  lift $ lift $ tell $ ErrorLog $ Map.singleton context [err]
  return Nothing
discardError (Right a) = return (Just a)

raiseError :: Monad m => EitherT HistoryError m a -> ErrorHandlingT m a
raiseError (Left err) = do
  context <- ask
  lift $ lift $ tell $ ErrorLog $ Map.singleton context [err]
  lift $ left err
raiseError (Right a) = return a

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
    _todo :: [String],
    _errors :: Map String HistoryError}

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
