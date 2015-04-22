{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

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
    parseResult <- AP.parseWith (return "") wikiParser wiki
    case parseResult of
        AP.Done unused parsed -> do
            lift $ print $ last $ wikiToList parsed
            lift $ print unused
        AP.Fail unused contexts err -> do
            lift $ print err
            lift $ print unused

type NationKey = String
data NationNode = NationNode
    {
        _nationValue :: NationValue,
        _nationPrecursors :: S.Set NationKey,
        _nationSuccessors :: S.Set NationKey
    }
data NationValue = NationValue
    {
        _name :: String,
        _nationStartYear :: Maybe Int,
        _nationEndYear :: Maybe Int
    }

--The one on the right is considered cannonical, we're just grabbing
--p and s from the left one.
addEdges :: NationNode -> NationNode -> NationNode
addEdges (NationNode _ p1 s1)  (NationNode val p2 s2) =
        NationNode val (p1<>p2) (s1<>s2)

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
    _todo :: [String]}

toAdjacency :: BuildingNationGraph -> M.Map NationKey NationNode
toAdjacency (BuildingNationGraph nations subdivisions synonyms _) = let
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

instance Show BuildingNationGraph where
    show ng@(BuildingNationGraph _ _ synonyms todo) =
        M.foldWithKey (\ k (NationNode (NationValue n sy ey) p s) rest -> k ++
                "\n\tname:\n\t\t" ++ n ++
                "\n\tlifetime:\n\t\t(" ++ maybe "?" show sy ++ " to " ++ maybe "?" show ey ++
                ")\n\tprecursors:" ++
                foldMap ("\n\t\t"++) p ++
                "\n\tsuccessors:" ++
                foldMap ("\n\t\t"++) s ++
                "\n" ++ rest
            ) ("remaining:"++show filteredTodo++"\nsynonyms:"++ show synonyms) graph
        where
            graph = toAdjacency ng 
            filteredTodo = [bestName|name <- todo,
                                let bestName = fromMaybe name $ M.lookup name synonyms,
                                not $ bestName `M.member` graph]

getNext :: Sess.Session -> BuildingNationGraph -> IO BuildingNationGraph
getNext _ ng@(BuildingNationGraph _ _ _ []) = return ng
getNext sess (BuildingNationGraph nationsGraph subdivisionsGraph synonyms (next:stack)) =
    if bestName `M.member` nationsGraph || bestName `M.member` subdivisionsGraph 
    then getNext sess (BuildingNationGraph nationsGraph subdivisionsGraph synonyms stack)
    else do
        result <- getConnected sess bestName
        case result of
            Left err -> do
                putStrLn $ show err ++ " for " ++ next
                getNext sess $ BuildingNationGraph nationsGraph subdivisionsGraph synonyms stack
            Right (name, infobox)-> do
                if M.member name nationsGraph || M.member name subdivisionsGraph
                then getNext sess $ BuildingNationGraph nationsGraph subdivisionsGraph  newSynonyms stack
                else return $ insert infobox
                where newSynonyms =
                        if next == name
                        then synonyms
                        else M.insert next name synonyms
                      insert :: Infobox -> BuildingNationGraph
                      insert (NationInfobox n sy ey p s) = BuildingNationGraph
                        (M.insert
                            name 
                            (NationNode
                                (NationValue n sy ey)
                                (S.fromList p)
                                (S.fromList s)
                            )
                            nationsGraph
                        )
                        subdivisionsGraph
                        synonyms
                        (p++s++stack)
                      insert (SubdivisionInfobox n sy ey p s pc) = BuildingNationGraph 
                        nationsGraph
                        (M.insert
                            name
                            (SubdivisionNode
                                (NationValue n sy ey)
                                pc
                                (S.fromList p)
                                (S.fromList s)
                            )
                            subdivisionsGraph
                        ) 
                        synonyms (p++s++stack)
    where
        bestName = fromMaybe next (M.lookup next synonyms)

initialGraph = BuildingNationGraph M.empty M.empty M.empty ["Roman Empire"] 

doIt :: Int -> BuildingNationGraph -> IO BuildingNationGraph
doIt n graph= Sess.withSession (\ sess -> doIt' sess n graph) where
    doIt' :: Sess.Session -> Int -> BuildingNationGraph -> IO BuildingNationGraph
    doIt' _ 0 graph = return $ graph
    doIt' sess n graph = do
        next <- getNext sess graph
        doIt' sess (n-1) next

toFGL :: BuildingNationGraph -> Gr NationKey ()
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
    fglNodes = map swap numberedNodesAscList
    numberedNodes = M.fromAscList numberedNodesAscList
    indexOf = (numberedNodes M.!)
    fglEdges = map (\ (p,s) -> (indexOf p,indexOf s,())) $ S.toList edges
    in G.mkGraph fglNodes fglEdges

toUnlabeledTGF :: Gr NationKey () -> String
toUnlabeledTGF graph = let
    nodesStr = foldMap (\(i, name) -> (++) $ show i ++"\n") $ G.labNodes graph
    edgesStr = foldMap (\(i, j, _) -> (++) $ show i ++ " " ++ show j ++ "\n") $ G.labEdges graph
    in nodesStr $ ("#\n"++) $ edgesStr []

toTGF :: Gr NationKey () -> String
toTGF graph = let
    nodesStr = foldMap (\(i, name) -> show i ++ " " ++ name ++ "\n") $ G.labNodes graph
    edgesStr = foldMap (\(i, j, _) -> show i ++ " " ++ show j ++ "\n") $ G.labEdges graph
    in nodesStr ++ "#\n" ++ edgesStr

toGV :: Gr NationKey () -> DotGraph G.Node
toGV graph = let
    gvParams = nonClusteredParams{globalAttributes =
        [
            GraphAttrs [Splines SplineEdges],
            EdgeAttrs [HeadPort $ CompassPoint North,
                    TailPort $ CompassPoint South],
            NodeAttrs [Shape $ BoxShape]
        ]}
    in graphToDot gvParams graph

instance ToJSON (Gr NationKey ()) where
    toJSON graph =  let
        jsonNodes = toJSON $ map (\ (i,name) -> 
                object [
                    ("id", toJSON i),
                    ("label", toJSON name)
                ]
            ) $ G.labNodes graph
        jsonEdges = toJSON $ map (\ (p,s,_) -> 
                object [
                    ("source", toJSON p),
                    ("target", toJSON s)
                ]
            ) $ G.labEdges graph
        in object [("nodes", jsonNodes),("edges", jsonEdges)]

--main = void $ Sess.withSession $ \sess -> runEitherT $ do
--    (cannonicalName,wiki) <- getWiki sess "Etruscan civilization"
--    parse <- EitherT $ return $ (_Left%~WikiParseError) $ AP.parseOnly wikiParser wiki
--    let Just (WikiTemplate title _ props) = findTemplate "infobox former country" parse
--    lift $ print props
main = do
    result <- doIt 10 initialGraph
    print result
    let fglResult = toFGL result
    runGraphviz (toGV fglResult) Svg "./out.svg"
    --LTIO.writeFile "out.dot" $ renderDot $ toDot $ toGV result
    writeFile "./out.tgf" $ toTGF fglResult
    BSC.writeFile "./out.json" $ encodePretty fglResult
