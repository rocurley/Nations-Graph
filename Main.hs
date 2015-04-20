{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}

import Wiki

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Monoid
import Data.List
import Data.Maybe
import Data.Char
import Data.Foldable (foldMap)

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

--TODO:
--Underscores vs spaces
--Non-ascii characters
--Include Dates

wikiAPI = "http://en.wikipedia.org/w/api.php" :: String

getWiki :: Sess.Session -> String -> EitherT HistoryError IO (String,T.Text)
getWiki sess article = do
    let articleRedirectStripped = takeWhile (/= '#') article
    let opts = param "action" .~ ["query"] $
               param "prop" .~ ["revisions"] $
               param "rvprop" .~ ["content"] $
               param "format" .~ ["json"] $
               param "titles" .~ [T.pack articleRedirectStripped] $ defaults
    resp <- EitherT $ (_Left%~HTTPError) <$> try (Sess.getWith opts sess wikiAPI)
    source <- failWith JsonParseError $ (resp^?responseBody
                                            .key "query"
                                            .key "pages"
                                            .members
                                            .key "revisions"
                                            .nth 0
                                            .key "*"
                                            ._String  :: Maybe T.Text)
    case AP.parseOnly redirectParser source of
        Right link -> getWiki sess $ T.unpack link
        Left _ -> return (articleRedirectStripped,source)

getConnected :: Sess.Session -> String -> IO (Either HistoryError (String,Infobox))
getConnected sess target = runEitherT $ do
    (cannonicalName,wiki) <- getWiki sess target
    --The endOfInput won't work unless the wiki parser is improved.
    --See the result for French Thrid Republic for a hint.
    --parse <- EitherT $ return $ (_Left%~WikiParseError) $ parseOnly (wikiParser<*endOfInput) wiki
    parse <- EitherT $ return $ (_Left%~WikiParseError) $ AP.parseOnly wikiParser wiki
    --lift $ print parse
    infobox <- EitherT $ return $ getInfobox parse
    return (cannonicalName,infobox)

test :: String -> IO ()
test str = void $ Sess.withSession $ \sess -> runEitherT $ do
    (cannonicalName,wiki) <- getWiki sess str
    parseResult <- AP.parseWith (return "") (wikiParser<*AP.endOfInput) wiki
    case parseResult of
        AP.Done unused parsed -> do
            lift $ print $ last $ wikiToList parsed
            lift $ print unused
        AP.Fail unused contexts err -> do
            lift $ print err
            lift $ print unused

type Nation = String --Denotes the (non-redirect) name of a nation.

data NationGraph = NationGraph {
    _nations :: (M.Map Nation ([String],[String])),
    _subdivisions :: (M.Map String ([String],[String],[String])),
    _synonyms :: (M.Map String Nation),
    _todo :: [String]}

toGraph :: NationGraph -> M.Map Nation (S.Set Nation,S.Set Nation)
toGraph (NationGraph nationsGraph subdivisionsGraph synonyms _) = let
    strictCannonicalNationName :: String -> Maybe Nation
    strictCannonicalNationName name =
        if name `M.member` nationsGraph
        then Just name
        else M.lookup name synonyms

    subdivisionNations = M.mapWithKey  (\ name (_,_,possibleParent) ->
            fromMaybe name $ getFirst $ -- Default to the subdivision name
                foldMap (First . strictCannonicalNationName) possibleParent
        ) subdivisionsGraph

    cannonicalNationName :: String -> Nation
    cannonicalNationName name = let
        deSynonymized = fromMaybe name $ M.lookup name synonyms
        in fromMaybe deSynonymized $ M.lookup deSynonymized subdivisionNations

    cannonicalNationsGraph = traverse.both%~S.fromList $
        traverse.both.traverse%~cannonicalNationName $ nationsGraph
    cannonicalSubdivisionsGraph =  traverse.both%~S.fromList $
        M.mapKeys cannonicalNationName $
        traverse%~(\ (p,s,_) -> both.traverse%~cannonicalNationName $ (p,s)) $
        subdivisionsGraph
    in M.unionWith mappend cannonicalNationsGraph cannonicalSubdivisionsGraph


instance Show NationGraph where
    show ng@(NationGraph _ _ synonyms todo) =
        M.foldWithKey (\ k (p,s) rest -> k ++
                "\n\tprecursors:" ++
                foldMap ("\n\t\t"++) p ++
                "\n\tsuccessors:" ++
                foldMap ("\n\t\t"++) s ++
                "\n" ++ rest
            ) ("remaining:"++show filteredTodo++"\nsynonyms:"++ show synonyms) graph
        where
            graph = toGraph ng 
            filteredTodo = [bestName|name <- todo,
                                let bestName = fromMaybe name $ M.lookup name synonyms,
                                not $ bestName `M.member` graph]

getNext :: Sess.Session -> NationGraph -> IO NationGraph
getNext _ ng@(NationGraph _ _ _ []) = return ng
getNext sess (NationGraph nationsGraph subdivisionsGraph synonyms (next:stack)) =
    if bestName `M.member` nationsGraph || bestName `M.member` subdivisionsGraph 
    then getNext sess (NationGraph nationsGraph subdivisionsGraph synonyms stack)
    else do
        result <- getConnected sess bestName
        case result of
            Left err -> do
                putStrLn $ show err ++ " for " ++ next
                getNext sess $ NationGraph nationsGraph subdivisionsGraph synonyms stack
            Right (name, infobox)-> do
                if M.member name nationsGraph || M.member name subdivisionsGraph
                then getNext sess $ NationGraph nationsGraph subdivisionsGraph  newSynonyms stack
                else return $ insert infobox
                where newSynonyms =
                        if next == name
                        then synonyms
                        else M.insert next name synonyms
                      insert :: Infobox -> NationGraph
                      insert (NationInfobox p s) = NationGraph
                        (M.insert name (p,s) nationsGraph)
                            subdivisionsGraph synonyms (p++s++stack)
                      insert (SubdivisionInfobox p s pc) = NationGraph 
                        nationsGraph (M.insert name (p,s,pc) subdivisionsGraph) 
                        synonyms (p++s++stack)
    where
        bestName = fromMaybe next (M.lookup next synonyms)

initialGraph = NationGraph M.empty M.empty M.empty ["Roman Empire"] 

--All of this is pretty terrible. What is needed is to convert the graph into some
--standard representation (probably that of FGL) and have everything come from there.
--In particular, this needs to support in the future more information for a nation
--than just the name.


doIt :: Int -> NationGraph -> IO NationGraph
doIt n graph= Sess.withSession (\ sess -> doIt' sess n graph) where
    doIt' :: Sess.Session -> Int -> NationGraph -> IO NationGraph
    doIt' _ 0 graph = return $ graph
    doIt' sess n graph = do
        next <- getNext sess graph
        doIt' sess (n-1) next

nodesAndEdgesSets :: NationGraph -> (S.Set Nation,S.Set (Nation,Nation))
nodesAndEdgesSets nationsGraph= let
    graph = toGraph nationsGraph
    nodes = M.foldWithKey (\ name (precursors,successors) acc -> S.unions
        [S.singleton name,
         precursors,
         successors,
         acc]) S.empty graph
    edges = ifoldMap (\ name (precursors,successors) ->
        S.map (,name) precursors <>
        S.map (name,) successors) graph
    in (nodes,edges)

toUnlabeledTGF :: NationGraph -> (String,(M.Map Int Nation,S.Set (Int,Int))
toTGF nationGraph = let
    (nodes,edges) = nodesAndEdgesSets nationGraph
    numberedNodes = M.fromAscList $ zip (S.toAscList nodes) [1..]
    indexOf = (`M.lookup` numberedNodes)
    numberedEdges = S.map (indexOf***indexOf) edges
    nodesStr = ifoldMap (\name i -> show i ++ "\n") numberedNodes
    edgesStr = foldMap (\(Just i,Just j) -> show i ++ " " ++ show j ++ "\n") numberedEdges
    tgf = nodesStr ++ "#\n" ++ edgesStr
    swappedNumberedNodes = M.fromList [(i,nation) |(nation,i)  <- M.toList] 
    in (tgf,(swappedNumberedNodes,numberedEdges))

toTGF :: NationGraph -> String
toTGF nationGraph = let
    (nodes,edges) = nodesAndEdgesSets nationGraph
    numberedNodes = M.fromAscList $ zip (S.toAscList nodes) [1..]
    indexOf = (`M.lookup` numberedNodes)
    numberedEdges = S.map (indexOf***indexOf) edges
    nodesStr = ifoldMap (\name i -> show i ++ " " ++ name ++ "\n") numberedNodes
    edgesStr = foldMap (\(Just i,Just j) -> show i ++ " " ++ show j ++ "\n") numberedEdges
    tgf = nodesStr ++ "#\n" ++ edgesStr

toGV :: NationGraph -> DotGraph String
toGV nationGraph = let
    (nodes,edges) = nodesAndEdgesSets nationGraph
    gvNodes = map (\ n -> (n,n)) $ S.toList nodes
    gvEdges = map (\ (p,s) -> (p,s,())) $ S.toList edges
    gvParams = nonClusteredParams{globalAttributes =
        [
            GraphAttrs [Splines SplineEdges],
            EdgeAttrs [HeadPort $ CompassPoint North,
                    TailPort $ CompassPoint South],
            NodeAttrs [Shape $ BoxShape]
        ]}
    in graphElemsToDot gvParams gvNodes gvEdges

toFGL :: NationGraph -> Gr Nation ()
toFGL nationGraph = let
    (nodes,edges) = nodesAndEdgesSets nationGraph
    numberedNodesAscList = zip (S.toAscList nodes) [1..]
    fglNodes = map swap numberedNodesAscList
    numberedNodes = M.fromAscList numberedNodesAscList
    indexOf = (numberedNodes M.!)
    fglEdges = map (\ (p,s) -> (indexOf p,indexOf s,())) $ S.toList edges
    in G.mkGraph fglNodes fglEdges

instance ToJSON NationGraph where
    toJSON nationGraph =  let
        (nodes,edges) = nodesAndEdgesSets nationGraph
        jsonNodes = toJSON $ map (\ name -> 
                object [
                    ("id",toJSON name),
                    ("label", toJSON name)
                ]
            )$ S.toList nodes
        jsonEdges = toJSON $ map (\ (p,s) -> 
                object [
                    ("source",toJSON p),
                    ("target",toJSON s)
                ]
            )$ S.toList edges
        in object [("nodes", jsonNodes),("edges", jsonEdges)]
main = do
    result <- doIt 10 initialGraph
    print result
    --runGraphviz (toGV result) Svg "./out.svg"
    LTIO.writeFile "out.dot" $ renderDot $ toDot $ toGV result
    --BSC.writeFile "./out.json" $ encodePretty result
