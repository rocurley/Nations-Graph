{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}

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

import Data.Aeson
import Data.Aeson.Lens
import Control.Lens
import Data.Aeson.Encode
import Data.Aeson.Encode.Pretty

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Encoding
import qualified Data.ByteString.Lazy.Char8 as BSC

import qualified Data.Attoparsec.Text as AP

import Control.Monad.Trans.Either
import Control.Error.Util
import Control.Exception

import Network.HTTP.Client (HttpException)
import Network.Wreq
import qualified Network.Wreq.Session as Sess 

--TODO:
--Underscores vs spaces
--Non-ascii characters
--Include Dates

wikiAPI = "http://en.wikipedia.org/w/api.php" :: String

data Wiki = WikiText T.Text |
            WikiTemplate T.Text [Wiki] (M.Map T.Text Wiki) |
            WikiLink T.Text [Wiki]|
            WikiHTMLTag T.Text (M.Map T.Text T.Text)|
            Wiki :> Wiki
            deriving (Show)
infixr :>
wikiHead :: Wiki -> Wiki
wikiHead (a :> b) = a
wikiHead x = x

wikiEmpty :: Wiki -> Bool
wikiEmpty (WikiText t) = T.null t
wikiEmpty _ = False

wikiToList :: Wiki -> [Wiki]
wikiToList (a :> b) = a : wikiToList b
wikiToList x = [x]

wikiFlatten :: Wiki -> Wiki
wikiFlatten (WikiText a :> WikiText b :> rest) = wikiFlatten $ WikiText (a<>b) :> rest
wikiFlatten (WikiText a :> WikiText b) = WikiText (a<>b)
wikiFlatten (a :> b) = a :> wikiFlatten b
wikiFlatten a = a

nonDouble :: Char -> AP.Parser Char
nonDouble c = do
    AP.char c
    peek <- AP.peekChar
    case peek of
        Just c' -> if c == c' then empty else return c
        Nothing -> return c

xmlName :: AP.Parser T.Text
xmlName = do
    c1 <- AP.letter <|> AP.char '_' <|> AP.char ':'
    rest <- AP.takeWhile (\ c -> isLetter c || isDigit c || (c `elem` ".-_:"))
    return $ T.cons c1 rest

xmlAttribute :: AP.Parser (T.Text,T.Text)
xmlAttribute = do
    name <- xmlName
    "=\""
    value <- AP.takeWhile(\ c -> c /= '>' && not (isSpace c))
    return (name,value)

xmlTag :: AP.Parser (T.Text,M.Map T.Text T.Text)
xmlTag = do
    "<" <|> "</"
    name <- xmlName
    attributes <- A.many $ AP.takeWhile isSpace *> xmlAttribute
    AP.takeWhile isSpace
    ">"<|> "/>"
    return (name, M.fromList attributes)

xmlSpecificTag :: T.Text -> AP.Parser (T.Text,M.Map T.Text T.Text)
xmlSpecificTag name = do
    "<" <|> "</"
    AP.string name
    attributes <- A.many $ AP.takeWhile isSpace *> xmlAttribute
    AP.takeWhile isSpace
    ">"<|> "/>"
    return (name, M.fromList attributes)

wikiParser :: AP.Parser Wiki
wikiParser = do
    begin <- wikiHTMLTagParser <|>
            wikiLinkParser <|>
            wikiTemplateParser <|>
            WikiText <$> ("<"<|>">") <|>
            (WikiText <$> T.singleton <$> foldr ((<|>) . nonDouble) empty "{}[]") <|>
            (WikiText <$> AP.takeWhile (AP.notInClass "{}[]<>|"))
    if wikiEmpty begin
    then return begin
    else (AP.endOfInput >> return begin) <|> do
        next <- wikiParser
        return $ if wikiEmpty next
        then begin
        else begin :> next

wikiHTMLTagParser :: AP.Parser Wiki
wikiHTMLTagParser = do
    (name, attributes) <- xmlTag
    return $ WikiHTMLTag name attributes
    
wikiLinkParser :: AP.Parser Wiki
wikiLinkParser = do
    "[["
    first <- AP.takeWhile (\ c -> c /= '|' && c /= ']')
    peek <- AP.peekChar
    rest <- many $ "|" *> wikiParser
    "]]"
    return $ WikiLink first rest

wikiTemplateNamedParameter :: AP.Parser (T.Text,Wiki)
wikiTemplateNamedParameter = do
    "|"
    key <- AP.takeWhile (\ c -> c /='|' && c /= '=' && c /= '}')
    "="
    value <- wikiParser
    return (T.strip key,value)

wikiTemplateUnNamedParameter :: AP.Parser Wiki
wikiTemplateUnNamedParameter = do
    "|"
    wikiParser

wikiTemplateParser :: AP.Parser Wiki
wikiTemplateParser = do
    "{{"
    title <- AP.takeWhile (\ c -> c /= '|' && c /= '}')
    parameters <- A.many $ fmap Right wikiTemplateNamedParameter <|> fmap Left wikiTemplateUnNamedParameter
    "}}"
    return $ WikiTemplate (T.strip title) [x|Left x <- parameters] $ M.fromList [x|Right x <- parameters] 

redirectParser :: AP.Parser T.Text
redirectParser = do
    "#REDIRECT"
    A.many AP.space
    WikiLink link _ <- wikiLinkParser
    return link

data HistoryError = HTTPError HttpException |
                    JsonParseError |
                    WikiParseError String |
                    MissingInfobox |
                    DoubleInfobox |
                    InfoboxInterpretationError deriving Show

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

topLevelTemplates :: Wiki -> M.Map T.Text Wiki
topLevelTemplates wiki = M.fromList [(T.toLower title,WikiTemplate title uParams oParams)|WikiTemplate title uParams oParams <- wikiToList wiki ]

findTemplate :: T.Text -> Wiki -> Maybe Wiki
findTemplate target = getFirst . foldMap (First . 
    \case 
        t@(WikiTemplate title _ _) -> if T.toLower title == target then Just t else Nothing
        _ -> Nothing
    ) . wikiToList

data Infobox = NationInfobox{
                    _precursors :: [String],
                    _successors :: [String]} |
                SubdivisionInfobox{
                    _precursors :: [String],
                    _successors :: [String],
                    _parentCandidates :: [String]} deriving Show

precursors :: Lens Infobox Infobox [String] [String]
precursors f (NationInfobox p s) = (\ p' -> NationInfobox p' s) <$> f p
precursors f (SubdivisionInfobox p s pc) = (\ p' -> SubdivisionInfobox p' s pc) <$> f p
successors :: Lens Infobox Infobox [String] [String]
successors f (NationInfobox p s) = (NationInfobox p) <$> f s
successors f (SubdivisionInfobox p s pc) = (\ s' -> SubdivisionInfobox p s' pc) <$> f s
parentCandidates f (NationInfobox p s) =  pure (NationInfobox p s)
parentCandidates f (SubdivisionInfobox p s pc) = (SubdivisionInfobox p s) <$> f pc

getInfobox :: Wiki -> Either HistoryError Infobox
getInfobox wiki = case (findTemplate "infobox former country" wiki,
                        findTemplate "infobox former subdivision" wiki) of
        (Just _, Just _) -> Left DoubleInfobox
        (Just (WikiTemplate title _ props), Nothing) -> note InfoboxInterpretationError $
            NationInfobox <$> conn props 'p' <*> conn props 's'
        (Nothing, Just (WikiTemplate title _ props)) -> note InfoboxInterpretationError $
            SubdivisionInfobox <$> conn props 'p' <*> conn props 's'<*> pure (parents props)
        (Nothing,Nothing) -> Left MissingInfobox
        where
            conn :: M.Map T.Text Wiki -> Char -> Maybe [String]
            conn props ty = let
                raw = mapMaybe (\ i -> M.lookup (T.pack $ ty:show i) props) [1..15]
                in filter (not . null) <$> map (T.unpack . T.strip) <$> raw `forM` (\case
                        WikiText text -> Just text
                        WikiText text :> (WikiTemplate t _ _ :> _) -> if t == "!"
                            then Just text
                            else Nothing
                        _ -> Nothing)
            parents :: M.Map T.Text Wiki -> [String]
            parents props = [T.unpack nationName | WikiLink nationName _<- props^.ix "nation".to wikiToList]

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

doIt :: Int -> NationGraph -> IO NationGraph
doIt n graph= Sess.withSession (\ sess -> doIt' sess n graph) where
    doIt' :: Sess.Session -> Int -> NationGraph -> IO NationGraph
    doIt' _ 0 graph = return $ graph
    doIt' sess n graph = do
        next <- getNext sess graph
        doIt' sess (n-1) next

nodesAndEdgesSets :: NationGraph -> (S.Set String,S.Set (String,String))
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

toTGF :: NationGraph -> String
toTGF nationGraph = let
    (nodes,edges) = nodesAndEdgesSets nationGraph
    numberedNodes = M.fromAscList $ zip (S.toAscList nodes) [1..]
    indexOf = (`M.lookup` numberedNodes)
    numberedEdges = S.map (indexOf***indexOf) edges
    nodesStr = ifoldMap (\name i -> show i ++ " " ++ name ++ "\n") numberedNodes
    edgesStr = foldMap (\(Just i,Just j) -> show i ++ " " ++ show j ++ "\n") numberedEdges
    in nodesStr ++ "#\n" ++ edgesStr

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
    BSC.writeFile "./out.json" $ encodePretty result
--main = do
--    print $ length nationsList
--    Sess.withSession $ \ sess ->
--        traverse (getConnected sess >=> print) $ Data.List.take 50 nationsList

