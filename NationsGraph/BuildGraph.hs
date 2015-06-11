{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module NationsGraph.BuildGraph (
    getNext,
) where

import NationsGraph.Types
import NationsGraph.HTTP

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe

import qualified Network.Wreq.Session as Sess

getNext :: Sess.Session -> BuildingNationGraph -> IO BuildingNationGraph
getNext _ ng@(BuildingNationGraph _ _ _ [] _) = return ng
getNext sess (BuildingNationGraph nationsGraph subdivisionsGraph synonyms (next:stack) errors) =
    if examined
    then getNext sess (BuildingNationGraph nationsGraph subdivisionsGraph synonyms stack errors)
    else do
        result <- httpGetInfobox sess bestName
        case result of
            Left err -> do
                putStrLn $ show err ++ " for " ++ next
                let newErrors = M.insert next err errors
                getNext sess $ BuildingNationGraph nationsGraph subdivisionsGraph synonyms stack newErrors
            Right (name, infobox)-> do
                if M.member name nationsGraph || M.member name subdivisionsGraph
                then getNext sess $ BuildingNationGraph nationsGraph subdivisionsGraph  newSynonyms stack errors
                else return $ insert infobox
                where newSynonyms =
                        if next == name
                        then synonyms
                        else M.insert next name synonyms
                      insert :: Infobox -> BuildingNationGraph
                      insert (,
FormerCountryInfobox n sy ey p s) = BuildingNationGraph
                        (M.insert
                            name 
                            (NationNode
                                (NationValue n sy ey Nothing name)
                                (S.fromList p)
                                (S.fromList s)
                            )
                            nationsGraph
                        )
                        subdivisionsGraph
                        synonyms
                        (p++s++stack)
                        errors
                      insert (,
FormerSubdivisionInfobox n sy ey p s pc) = BuildingNationGraph 
                        nationsGraph
                        (M.insert
                            name
                            (SubdivisionNode
                                (NationValue n sy ey Nothing name)
                                pc
                                (S.fromList p)
                                (S.fromList s)
                            )
                            subdivisionsGraph
                        ) 
                        synonyms (p++s++stack)
                        errors
    where
        bestName = fromMaybe next (M.lookup next synonyms)
        examined = 
            bestName `M.member` nationsGraph ||
            bestName `M.member` subdivisionsGraph ||
            bestName `M.member` errors