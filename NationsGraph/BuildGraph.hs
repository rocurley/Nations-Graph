{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module NationsGraph.BuildGraph (
    getNext,
) where

import NationsGraph.Types
import NationsGraph.HTTP

import Control.Monad.Trans.Writer
import Control.Monad.Trans.Either
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Maybe

import qualified Network.Wreq.Session as Sess

getNext :: Sess.Session -> BuildingNationGraph -> WriterT ErrorLog IO BuildingNationGraph
getNext _ ng@(BuildingNationGraph _ _ _ []) = return ng
getNext sess (BuildingNationGraph nationsGraph subdivisionsGraph synonyms (next:stack)) =
    if examined
    then getNext sess $ BuildingNationGraph nationsGraph subdivisionsGraph synonyms stack
    else do
        result <- (`runReaderT` bestName) $ runEitherT $ do
          (realArticleName, infobox) <- httpGetInfobox sess bestName
          node <- infoboxToNode getFlag infobox realArticleName
          return (realArticleName, node)
        case result of
            Left err -> do
                lift $ putStrLn $ show err ++ " for " ++ next
                tell $ ErrorLog $ Map.singleton next [err]
                getNext sess $ BuildingNationGraph nationsGraph subdivisionsGraph synonyms stack
            Right (realArticleName, node)-> do
                if Map.member realArticleName nationsGraph || Map.member realArticleName subdivisionsGraph
                then getNext sess $ BuildingNationGraph nationsGraph subdivisionsGraph newSynonyms stack
                else insert node
                where
                  newSynonyms =
                    if next == realArticleName
                    then synonyms
                    else Map.insert next realArticleName synonyms
                  insert :: Either NationNode SubdivisionNode -> WriterT ErrorLog IO BuildingNationGraph
                  insert nodeEither = return $ case nodeEither of
                      Left node@(NationNode nationValue precursors successors) ->
                          BuildingNationGraph
                            (Map.insert realArticleName node nationsGraph)
                            subdivisionsGraph
                            synonyms
                            (Set.toList precursors ++ Set.toList successors ++ stack)
                      Right node@(SubdivisionNode nationValue possibleParents precursors successors) ->
                          BuildingNationGraph
                            nationsGraph
                            (Map.insert realArticleName node subdivisionsGraph)
                            synonyms
                            (Set.toList precursors ++ Set.toList successors ++ stack)
    where
        bestName = fromMaybe next (Map.lookup next synonyms)
        examined =
            bestName `Map.member` nationsGraph ||
            bestName `Map.member` subdivisionsGraph
        getFlag :: String -> ErrorHandlingT IO Flag
        getFlag flagName = do
          (flagUrl,licence) <- httpGetImage sess flagName
          return $ Flag flagUrl
