{-# LANGUAGE TemplateHaskell #-}

module NationsGraph.Types (
    Wiki(..),
    Infobox(..),
    HistoryError(..),
    NationKey(..),
    NationNode(..),
    NationValue(..),
    SubdivisionNode(..),
    BuildingNationGraph(..),
    HttpException,
    nationname,
    nationStartYear,
    nationEndYear,
    position,
) where

import qualified Data.Map as M
import qualified Data.Set as S

import qualified Data.Text as T

import Test.QuickCheck

import Control.Lens

import Network.HTTP.Client (HttpException)

data Wiki = WikiText T.Text |
            WikiTemplate T.Text [Wiki] (M.Map T.Text Wiki) |
            WikiLink T.Text [Wiki]|
            WikiHTMLTag T.Text (M.Map T.Text T.Text)|
            Wiki :> Wiki
            deriving (Show)
infixr :>

data HistoryError = HTTPError HttpException |
                    JsonParseError |
                    WikiParseError String |
                    MissingInfobox |
                    DoubleInfobox |
                    InfoboxInterpretationError deriving Show

type NationKey = String

data NationNode = NationNode
    {
        _nationValue :: NationValue,
        _nationPrecursors :: S.Set NationKey,
        _nationSuccessors :: S.Set NationKey
    }

instance Arbitrary NationNode where
    arbitrary = NationNode <$> arbitrary <*>
        fmap S.fromList arbitrary <*>
        fmap S.fromList arbitrary

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
        _subdivisionPrecursors :: S.Set NationKey,
        _subdivisionSuccessors :: S.Set NationKey
    }

data BuildingNationGraph = BuildingNationGraph {
    _nations :: (M.Map NationKey NationNode),
    _subdivisions :: (M.Map NationKey SubdivisionNode),
    _synonyms :: (M.Map String NationKey),
    _todo :: [String],
    _errors :: M.Map String HistoryError}

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