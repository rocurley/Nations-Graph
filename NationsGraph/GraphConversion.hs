{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}

module NationsGraph.GraphConversion (
    BuildingNationGraph(..),
	toAdjacency,
    toFGL,
    toUnlabeledTGF,
    toTGF,
    toGV,
) where
import NationsGraph.Types

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Monoid
import Data.List
import Data.Maybe
import Data.Char
import Data.Foldable (foldMap)
import qualified Data.Vector as V

import Control.Monad.Trans.Maybe
import Control.Monad.Trans
import Control.Monad
import Control.Applicative as A

import Control.Arrow
import Data.Tuple

import Data.Aeson
import Data.Aeson.Lens
import Control.Lens
import Data.Aeson.Encode
import Data.Aeson.Encode.Pretty

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy.IO as LTIO
import Data.Text.Encoding
import qualified Data.ByteString.Lazy.Char8 as BSC

import qualified Data.Attoparsec.Text as AP

import Control.Monad.Trans.Either
import Control.Error.Util
import Control.Exception

import Network.HTTP.Client (HttpException)
import Network.Wreq
import qualified Network.Wreq.Session as Sess

import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive (Gr)
import Data.GraphViz
import Data.GraphViz.Printing hiding ((<>))
import Data.GraphViz.Attributes.Complete

--The one on the right is considered cannonical, we're just grabbing
--p and s from the left one.
addEdges :: NationNode -> NationNode -> NationNode
addEdges (NationNode _ p1 s1)  (NationNode val p2 s2) =
        NationNode val (p1<>p2) (s1<>s2)

toAdjacency :: BuildingNationGraph -> M.Map NationKey NationNode
toAdjacency (BuildingNationGraph nations subdivisions synonyms _ _) = let
    strictCannonicalNationName :: String -> Maybe NationKey
    strictCannonicalNationName name =
        if name `M.member` nations
        then Just name
        else M.lookup name synonyms

    subdivisionKeyToNationKey :: M.Map String NationKey
    subdivisionKeyToNationKey = M.mapWithKey  (\ name (SubdivisionNode _ possibleParents _ _) ->
            fromMaybe name $ getFirst $ -- Default to the subdivision name
                foldMap (First . strictCannonicalNationName) possibleParents
        ) subdivisions

    cannonicalNationName :: String -> NationKey
    cannonicalNationName name = let
        deSynonymized = fromMaybe name $ M.lookup name synonyms
        in fromMaybe deSynonymized $ M.lookup deSynonymized subdivisionKeyToNationKey

    cannonicalNationsGraph = 
        (\ (NationNode val p s) -> NationNode val
            (S.map cannonicalNationName p) (S.map cannonicalNationName s)) <$>
        nations
    in M.foldrWithKey (\ subdivisionName (SubdivisionNode nationValue _ p s) acc ->
            M.insertWith addEdges
                (cannonicalNationName subdivisionName)
                (NationNode nationValue p s)
                acc
        ) cannonicalNationsGraph subdivisions

showLifetime :: Maybe Int -> Maybe Int -> String
showLifetime sy ey = "(" ++ maybe "?" show sy ++ " to " ++ maybe "?" show ey ++ ")"

instance Show BuildingNationGraph where
    show ng@(BuildingNationGraph _ _ synonyms todo errors) =
        M.foldWithKey (\ k (NationNode (NationValue n sy ey) p s) rest -> k ++
                "\n\tname:\n\t\t" ++ n ++
                "\n\tlifetime:\n\t\t" ++ showLifetime sy ey ++
                "\n\tprecursors:" ++
                foldMap ("\n\t\t"++) p ++
                "\n\tsuccessors:" ++
                foldMap ("\n\t\t"++) s ++
                "\n" ++ rest
            ) ("remaining:"++show filteredTodo++
               "\nsynonyms:"++ show synonyms++
               "\nerrors:" ++ show errors) graph
        where
            graph = toAdjacency ng 
            filteredTodo = [bestName|name <- todo,
                                let bestName = fromMaybe name $ M.lookup name synonyms,
                                not $ bestName `M.member` graph]

toFGL :: BuildingNationGraph -> Gr NationValue ()
toFGL nationGraph = let
    graph = toAdjacency nationGraph
    nodes = M.foldWithKey (\ name (NationNode val precursors successors) acc -> S.unions
        [S.singleton name,
         precursors,
         successors,
         acc]) S.empty graph
    edges = ifoldMap (\ name (NationNode val precursors successors) ->
        S.map (,name) precursors <>
        S.map (name,) successors) graph
    numberedNodesAscList = zip (S.toAscList nodes) [1..]
    keyToValue :: NationKey -> NationValue
    keyToValue k = case M.lookup k graph of
        Nothing -> NationValue k Nothing Nothing
        Just (NationNode val _ _) -> val
    fglNodes = map (\ (k,i) -> (i, keyToValue k)) numberedNodesAscList
    numberedNodes = M.fromAscList numberedNodesAscList
    indexOf = (numberedNodes M.!)
    fglEdges = map (\ (p,s) -> (indexOf p,indexOf s,())) $ S.toList edges
    in G.mkGraph fglNodes fglEdges

toUnlabeledTGF :: Gr a b -> String
toUnlabeledTGF graph = let
    nodesStr = foldMap (\(i, _) -> Endo $ (++) $ show i ++ " " ++ show i ++ "\n") $ G.labNodes graph
    edgesStr = foldMap (\(i, j, _) -> Endo $ (++) $ show i ++ " " ++ show j ++ "\n") $ G.labEdges graph
    in appEndo nodesStr $ ("#\n"++) $ appEndo edgesStr []

toTGF :: Gr NationValue () -> String
toTGF graph = let
    nodesStr = foldMap (\(i, NationValue name sy ey) ->
        show i ++ " " ++ name ++ "\\n" ++ 
        showLifetime sy ey ++ "\n") $ G.labNodes graph
    edgesStr = foldMap (\(i, j, _) -> show i ++ " " ++ show j ++ "\n") $ G.labEdges graph
    in nodesStr ++ "#\n" ++ edgesStr

toGV :: Gr NationValue () -> DotGraph G.Node
toGV graph = let
    gvParams = nonClusteredParams{globalAttributes =
        [
            GraphAttrs [Splines SplineEdges],
            EdgeAttrs [HeadPort $ CompassPoint North,
                    TailPort $ CompassPoint South],
            NodeAttrs [Shape $ BoxShape]
        ]}
    in graphToDot gvParams graph

instance ToJSON (Gr NationValue ()) where
    toJSON graph =  let
        jsonNodes = toJSON $ map (\ (i,NationValue name sy ey) -> 
                object $ catMaybes [
                    Just ("id", toJSON i),
                    Just ("label", toJSON name),
                    ("startYear",) <$> toJSON <$> sy,
                    ("endYear",) <$> toJSON <$> ey
                ]
            ) $ G.labNodes graph
        jsonEdges = toJSON $ map (\ (p,s,_) -> 
                object [
                    ("source", toJSON p),
                    ("target", toJSON s)
                ]
            ) $ G.labEdges graph
        in object [("nodes", jsonNodes),("edges", jsonEdges)]
instance FromJSON (Gr NationValue ()) where
    parseJSON (Object json) = do
        Array jsonNodes <- json .: "nodes"
        Array jsonEdges <- json .: "edges"
        nodes <- traverse (\ (Object node) -> do
                id <- node.:"id"
                label <- node.:"label"
                startYear <- node.:?"startYear"
                endYear <- node.:?"endYear"
                return (id,NationValue label startYear endYear)
            ) $ V.toList jsonNodes
        edges <- traverse (\ (Object edge) ->
                (,,()) <$> edge.:"source" <*> edge.:"target"
            ) $ V.toList jsonEdges
        return $ G.mkGraph nodes edges