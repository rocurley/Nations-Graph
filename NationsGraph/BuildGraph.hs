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
    then getNext sess (BuildingNationGraph nationsGraph subdivisionsGraph synonyms stack)
    else do
        result <- runEitherT $ httpGetInfobox sess bestName
        case result of
            Left err -> do
                lift $ putStrLn $ show err ++ " for " ++ next
                tell $ ErrorLog $ Map.singleton next [err]
                getNext sess $ BuildingNationGraph nationsGraph subdivisionsGraph synonyms stack
            Right (name, infobox)-> do
                if Map.member name nationsGraph || Map.member name subdivisionsGraph
                then getNext sess $ BuildingNationGraph nationsGraph subdivisionsGraph  newSynonyms stack
                else insert infobox
                where newSynonyms =
                        if next == name
                        then synonyms
                        else Map.insert next name synonyms
                      getFlag :: Maybe String -> WriterT ErrorLog IO (Maybe Flag)
                      getFlag flagName = do
                          flagEither <- maybe
                            (return $ Left $ MissingInfoboxFieldError "image_flag")
                            (runEitherT . httpGetImage sess)
                            flagName
                          case flagEither of
                            Left err -> do
                              tell $ ErrorLog $ Map.singleton name [err]
                              return Nothing
                            Right (url,_) -> return $ Just $ Flag url
                      insert :: Infobox -> WriterT ErrorLog IO BuildingNationGraph
                      insert (NationInfobox n sy ey flagName p s) = do
                          flag <- getFlag flagName
                          return $ BuildingNationGraph
                            (Map.insert
                                name
                                (NationNode
                                    (NationValue n sy ey Nothing name flag)
                                    (Set.fromList p)
                                    (Set.fromList s)
                                )
                                nationsGraph
                            )
                            subdivisionsGraph
                            synonyms
                            (p++s++stack)
                      insert (SubdivisionInfobox n sy ey flagName p s pc) = do
                          flag <- getFlag flagName
                          return $ BuildingNationGraph
                            nationsGraph
                            (Map.insert
                                name
                                (SubdivisionNode
                                    (NationValue n sy ey Nothing name flag)
                                    pc
                                    (Set.fromList p)
                                    (Set.fromList s)
                                )
                                subdivisionsGraph
                            )
                            synonyms (p++s++stack)
    where
        bestName = fromMaybe next (Map.lookup next synonyms)
        examined =
            bestName `Map.member` nationsGraph ||
            bestName `Map.member` subdivisionsGraph
