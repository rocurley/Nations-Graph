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
    iTreeFromList,
    intervalIntersect,
    rectangleIntersect,
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
        M.foldWithKey (\ k (NationNode (NationValue n sy ey _) p s) rest -> k ++
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
        Nothing -> NationValue k Nothing Nothing Nothing
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
    nodesStr = foldMap (\(i, NationValue name sy ey _) ->
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
        jsonNodes = toJSON $ map (\ (i,NationValue name sy ey pos) -> 
                object $ catMaybes [
                    Just ("id", toJSON i),
                    Just ("label", toJSON name),
                    ("startYear",) <$> toJSON <$> sy,
                    ("endYear",) <$> toJSON <$> ey,
                    ("x",) <$> toJSON <$> fst <$> pos,
                    ("y",) <$> toJSON <$> snd <$> pos
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
                let pos = (,) <$> x <*> y
                return (id,NationValue label startYear endYear pos)
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
    svgNodeList = svgDoc^..root.named "svg".to NodeElement 
    in root.named "html"./named "body".nodes %~ (++svgNodeList) $ xhtmlDoc

--fillInNode :: Gr NationValue () -> [Node] -> [Node]
--fillInNode gr [NodeContent indexString] = let
--    i = read $ T.unpack indexString
--    (Just (_,_,NationValue name sy ey _,_),_) = G.match i gr
--    in T.pack $ name ++ "\n" ++ showLifetime sy ey
--fillInNode _ x = x

fillInSvg :: Gr NationValue () -> Document -> Document
fillInSvg gr doc = let
    iTree = iTreeFromList $ svgRectangles doc
    in root.named "svg"./named "g"./named "g"./named "text"%~tweakElement gr iTree $ doc

tweakElement :: Gr NationValue () -> IntervalTree Float (Float,Float)-> Element -> Element
tweakElement gr iTree = execState $ do
    i <- read <$> T.unpack <$> use text
    x0 <- read <$> T.unpack <$> use (attr "x")
    y0 <- read <$> T.unpack <$> use (attr "y")
    let ((lR,rR),(uR,bR)) = head $ rectangleIntersect iTree (x0,y0)
    let x = T.pack $ show $ (lR + rR)/2
    Text.XML.Lens.attrs %= M.delete "{http://www.w3.org/XML/1998/namespace}space"
    let (Just (_,_,NationValue name sy ey _,_),_) = G.match i gr
    let l1 = Element "{http://www.w3.org/2000/svg}tspan"
            (M.fromList [("x",x),("dy","-0.7em"),
                ("text-anchor","middle")])
            [NodeContent $ T.pack name]
    let l2 = Element "{http://www.w3.org/2000/svg}tspan"
            (M.fromList [("x",x),("dy","1.4em"),
                ("text-anchor","middle")])
            [NodeContent $ T.pack $ showLifetime sy ey]
    nodes .= fmap NodeElement [l1,l2]

iTreeFromList :: Ord a => [((a,a),b)] -> IntervalTree a b
iTreeFromList [] = EmptyNode
iTreeFromList xs = let
    mid = foldr (\ ((l,r),_) acc -> (l:r:acc)) [] xs !! length xs
    leftIntervals = filter ((<mid) . snd . fst) xs
    centerIntervals = filter (\ ((l,r),_) -> l <= mid && mid <= r) xs
    rightIntervals = filter ((mid<) . fst . fst) xs
    in Node mid
        (iTreeFromList leftIntervals)
        (sortOn (fst.fst) centerIntervals,
            sortOn (Down . snd.fst) centerIntervals)
        (iTreeFromList rightIntervals)

intervalIntersect :: Ord a => IntervalTree a b -> a -> [((a,a),b)]
intervalIntersect EmptyNode _ = []
intervalIntersect (Node mid left (asc,desc) right) x = case compare x mid of
    LT -> intervalIntersect left x ++ takeWhile ((<=x) . fst . fst) asc
    EQ -> asc
    GT -> intervalIntersect right x ++ takeWhile ((x<=) . snd . fst) desc

rectangleIntersect :: Ord a => IntervalTree a (a,a) -> (a,a) -> [((a,a),(a,a))]
rectangleIntersect iTree (x,y) =
    [rect | rect@(_,(y1,y2))<-intervalIntersect iTree x, y1 <= y, y <= y2]

rectFromElem :: (Read a, Num a) => Element -> ((a,a),(a,a))
rectFromElem elem = let
     x = read $ T.unpack $ elem^.attr "x"
     w = read $ T.unpack $ elem^.attr "width"
     y = read $ T.unpack $ elem^.attr "y"
     h = read $ T.unpack $ elem^.attr "height"
     in ((x,x+w),(y,y+h))

svgRectangles :: (Read a, Num a) => Document -> [((a,a),(a,a))]
svgRectangles doc = doc^..root.
    named "svg"./named "g"./named "g".
    filtered (\ elem -> elem^?attr "fill" /= Just "white")./
    named "rect".to rectFromElem