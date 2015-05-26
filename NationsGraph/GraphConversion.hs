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
    loadPositionsFromGraphml,
    addPositionsToGraph,
    svgToHTML,
    svgToXHTML,
    mergeSvgXHTML,
    fillInSvg,
) where
import NationsGraph.Types

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import Data.Char
import Data.Ord
import qualified Data.Text as T
import Data.Monoid
import Data.Maybe
import qualified Data.Vector as V

import Control.Lens

import Data.Aeson hiding ((.=))

import Text.XML
import Text.XML.Lens

import Control.Monad.State

import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive (Gr)
import Data.GraphViz
import Data.GraphViz.Attributes.Complete

import Debug.Trace

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
        M.foldWithKey (\ k (NationNode (NationValue n sy ey _ key) p s) rest -> k ++
                "\n\tname:\n\t\t" ++ n ++
                "\n\tlifetime:\n\t\t" ++ showLifetime sy ey ++
                "\n\tprecursors:" ++
                foldMap ("\n\t\t"++) p ++
                "\n\tsuccessors:" ++
                foldMap ("\n\t\t"++) s ++
                "\n\twikipedia article:\n\t\t" ++ key ++
                "\n" ++ rest
            ) ("remaining:"++show filteredTodo++
               "\nsynonyms:"++ show synonyms++
               "\nerrors:" ++ show errors) graph
        where
            graph = toAdjacency ng 
            filteredTodo = [bestName | name <- todo,
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
        Nothing -> NationValue k Nothing Nothing Nothing k
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
    nodesStr = foldMap (\(i, NationValue name sy ey _ _) ->
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
        jsonNodes = toJSON $ map (\ (i,NationValue name sy ey pos key) -> 
                object $ catMaybes [
                    Just ("id", toJSON i),
                    Just ("label", toJSON name),
                    ("startYear",) <$> toJSON <$> sy,
                    ("endYear",) <$> toJSON <$> ey,
                    ("x",) <$> toJSON <$> fst <$> pos,
                    ("y",) <$> toJSON <$> snd <$> pos,
                    Just ("wikiArticle", toJSON key)
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
                x <- node.:?"x"
                y <- node.:?"y"
                key <- node.:"wikiArticle"
                let pos = (,) <$> x <*> y
                return (id,NationValue label startYear endYear pos key)
            ) $ V.toList jsonNodes
        edges <- traverse (\ (Object edge) ->
                (,,()) <$> edge.:"source" <*> edge.:"target"
            ) $ V.toList jsonEdges
        return $ G.mkGraph nodes edges

shapeNodes = 
    root.
    named "graphml"./
    named "graph"./
    named "node"./
    named "data"./
    named "ShapeNode"

asRead :: (Read a, Show a) => Lens' T.Text a
asRead f txt= T.pack <$> show <$> f (read $ T.unpack txt)

nodeInfo :: Traversal' Element (Int,(Float,Float))
nodeInfo f node = let
    labelLens :: Traversal' Element Int
    labelLens = id./named "NodeLabel".text.
            filtered (not . T.all isSpace).
            asRead
    xLens :: Traversal' Element Float
    xLens = id./
            named "Geometry".
            attr "x".
            asRead
    yLens :: Traversal' Element Float
    yLens = id./
            named "Geometry".
            attr "y".
            asRead
    maybeTrio = (\ label x y -> (label,(x,y))) <$>
        node^? labelLens <*> node^? xLens <*> node^? yLens
    in case maybeTrio of
        Nothing -> pure node
        Just trio -> fmap (\ (label,(x,y)) -> 
            labelLens.~label $ xLens.~x $ yLens.~y $ node) $ f trio

loadPositionsFromGraphml :: Document -> [(Int,(Float,Float))]
loadPositionsFromGraphml doc = doc^..shapeNodes.nodeInfo

addPositionsToGraph :: M.Map Int (Float,Float) -> Gr NationValue () -> Gr NationValue ()
addPositionsToGraph positions graph =
    G.gmap (\ (p,i,val,s) ->
        (p,i,position.~M.lookup i positions $ val,s)
    ) graph

svgToHTML :: Document -> Document
svgToHTML doc = let
    svg = doc^..root.named "svg"
    html = Element "html" M.empty
        [
            NodeElement $ Element "head" M.empty [],
            NodeElement $ Element "body" M.empty $ NodeElement <$> svg
        ]
    in Document (Prologue [] Nothing []) html []

svgToXHTML :: Document -> Document
svgToXHTML doc = let
    svg = doc^..root.named "svg"
    xhtmlName :: T.Text -> Name
    xhtmlName name = Name name (Just "http://www.w3.org/1999/xhtml") Nothing
    html = Element (xhtmlName "html") M.empty
        [
            NodeElement $ Element (xhtmlName "head") M.empty
                [NodeElement $ Element (xhtmlName "title") M.empty
                    [NodeContent "Nations Graph"]],
            NodeElement $ Element (xhtmlName "body") M.empty $ NodeElement <$> svg
        ]
    in Document (Prologue [] Nothing []) html []

mergeSvgXHTML :: Document -> Document -> Document
mergeSvgXHTML svgDoc xhtmlDoc = let
    svgNodeList = shrinkSvg svgDoc^..root.named "svg".to NodeElement 
    in root.named "html"./named "body".nodes %~ (++svgNodeList) $ xhtmlDoc

shrinkSvg :: Document -> Document
shrinkSvg = execState $ do
    root.named "svg".attribute "height".= Just "600"
    root.named "svg".attribute "width".= Just "600"
{-
<svg>
  <g>
    <g fill="white">  #keep unmodified
      <rect/>
    </g>
    <g fill="rgb(255,204,0)"> #kill
      <rect/>
    </g>
    <g>
      <text>1</text> #kill and replace
      <rect fill="none"/> #set fill
    </g>
    ....
    <g>
      <text>44</text> #kill and replace
      <rect fill="none"/> #set fill
      <path/> #keep unmodified
      ....
    </g>
  </g>
<svg/>
-}


fillInSvg :: Gr NationValue () -> Document -> Document
fillInSvg gr = execState $ do
    prologue.= Prologue
        [MiscInstruction $ Instruction
            "xml-stylesheet"
            "type=\"text/css\" href=\"style.css\""
        ]
        Nothing
        []
    --let cssElement = Element
    --        "style"
    --        (M.singleton "type" "text/css")
    --        [NodeContent css]
    --root.named "svg".nodes %= (NodeElement cssElement :)
    root.named "svg"./named "g".nodes %=
        filter (\ g ->
            (g^?_Element.attr "fill") /= Just "rgb(255,204,0)"
        )
    root.named "svg"./named "g"./named "g".filtered (\ g ->
            g^?attr "fill" /= Just "white")%= tweakElement gr

tweakElement :: Gr NationValue () -> Element -> Element
tweakElement gr = execState $ do
    id./named "rect".attr "fill".="rgb(255,204,0)"
    x <- use (id./named "rect".attr "x")
    y <- use (id./named "rect".attr "y")
    i <- read <$> T.unpack <$> use (id./named "text".text)
    let Just (NationValue name sy ey _ wikiLink) = G.lab gr i
    width <- use (id./named "rect".attr "width")
    height <- use (id./named "rect".attr "height")
    nodes%=filter (\ node -> node^?_Element.localName /= Just "text")
    let miniBody = Element
            "{http://www.w3.org/1999/xhtml}body"
            M.empty
            [
                NodeElement $ Element
                    "{http://www.w3.org/1999/xhtml}div"
                    (M.singleton "class" "node")
                    [
                        NodeElement $ Element
                            "{http://www.w3.org/1999/xhtml}a"
                            (M.fromList [
                                ("href", "http://en.wikipedia.org/wiki/"<> T.pack wikiLink),
                                ("target","_blank")
                            ])
                            [NodeContent $ T.pack name],
                        NodeElement $ Element
                            "{http://www.w3.org/1999/xhtml}br"
                            M.empty
                            [],
                        NodeContent $ T.pack $ showLifetime sy ey
                    ]
            ]
    let foreignObject = Element
            "{http://www.w3.org/2000/svg}foreignObject"
            (M.fromList [("x",x),("y",y),("width",width),("height",height)])
            [NodeElement miniBody]
    nodes%=(NodeElement foreignObject :)