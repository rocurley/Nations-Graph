{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

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

import Data.Aeson
import Data.Aeson.Lens
import Control.Lens

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Encoding
import qualified Data.ByteString.Lazy.Char8 as BSC

import Data.Attoparsec.Text as AP hiding (try)

import Control.Monad.Trans.Either
import Control.Error.Util
import Control.Exception

import Network.HTTP.Client (HttpException)
import Network.Wreq
import qualified Network.Wreq.Session as Sess 

--Todo
--Underscores vs spaces
--Non-ascii characters
--Template:Infobox former subdivision


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

nonDouble :: Char -> Parser Char
nonDouble c = do
    char c
    peek <- peekChar
    case peek of
        Just c' -> if c == c' then empty else return c
        Nothing -> return c

xmlName :: Parser T.Text
xmlName = do
    c1 <- letter <|> char '_' <|> char ':'
    rest <- AP.takeWhile (\ c -> isLetter c || isDigit c || (c `elem` ".-_:"))
    return $ T.cons c1 rest

xmlAttribute :: Parser (T.Text,T.Text)
xmlAttribute = do
    name <- xmlName
    "=\""
    value <- AP.takeWhile(\ c -> c /= '>' && not (isSpace c))
    return (name,value)

xmlTag :: Parser (T.Text,M.Map T.Text T.Text)
xmlTag = do
    "<" <|> "</"
    name <- xmlName
    attributes <- A.many $ AP.takeWhile isSpace *> xmlAttribute
    AP.takeWhile isSpace
    ">"<|> "/>"
    return (name, M.fromList attributes)

xmlSpecificTag :: T.Text -> Parser (T.Text,M.Map T.Text T.Text)
xmlSpecificTag name = do
    "<" <|> "</"
    string name
    attributes <- A.many $ AP.takeWhile isSpace *> xmlAttribute
    AP.takeWhile isSpace
    ">"<|> "/>"
    return (name, M.fromList attributes)

wikiParser :: Parser Wiki
wikiParser = do
    begin <- wikiHTMLTagParser <|>
            wikiLinkParser <|>
            wikiTemplateParser <|>
            WikiText <$> ("<"<|>">") <|>
            (WikiText <$> T.singleton <$> foldr ((<|>) . nonDouble) empty "{}[]") <|>
            (WikiText <$> AP.takeWhile (notInClass "{}[]<>|"))
    if wikiEmpty begin
    then return begin
    else (endOfInput >> return begin) <|> do
        next <- wikiParser
        return $ if wikiEmpty next
        then begin
        else begin :> next

wikiHTMLTagParser :: Parser Wiki
wikiHTMLTagParser = do
    (name, attributes) <- xmlTag
    return $ WikiHTMLTag name attributes
    
wikiLinkParser :: Parser Wiki
wikiLinkParser = do
    "[["
    first <- AP.takeWhile (\ c -> c /= '|' && c /= ']')
    peek <- peekChar
    rest <- many $ "|" *> wikiParser
    "]]"
    return $ WikiLink first rest

wikiTemplateNamedParameter :: Parser (T.Text,Wiki)
wikiTemplateNamedParameter = do
    "|"
    key <- AP.takeWhile (\ c -> c /='|' && c /= '=' && c /= '}')
    "="
    value <- wikiParser
    return (T.strip key,value)

wikiTemplateUnNamedParameter :: Parser Wiki
wikiTemplateUnNamedParameter = do
    "|"
    wikiParser

wikiTemplateParser :: Parser Wiki
wikiTemplateParser = do
    "{{"
    title <- AP.takeWhile (\ c -> c /= '|' && c /= '}')
    parameters <- A.many $ fmap Right wikiTemplateNamedParameter <|> fmap Left wikiTemplateUnNamedParameter
    "}}"
    return $ WikiTemplate (T.strip title) [x|Left x <- parameters] $ M.fromList [x|Right x <- parameters] 

redirectParser :: Parser T.Text
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
    let opts = param "action" .~ ["query"] $
               param "prop" .~ ["revisions"] $
               param "rvprop" .~ ["content"] $
               param "format" .~ ["json"] $
               param "titles" .~ [T.pack article] $ defaults
    resp <- EitherT $ (_Left%~HTTPError) <$> try (Sess.getWith opts sess wikiAPI)
    source <- failWith JsonParseError $ (resp^?responseBody
                                            .key "query"
                                            .key "pages"
                                            .members
                                            .key "revisions"
                                            .nth 0
                                            .key "*"
                                            ._String  :: Maybe T.Text)
    case parseOnly redirectParser source of
        Right link -> getWiki sess $ T.unpack link
        Left _ -> return (article,source)

topLevelTemplates :: Wiki -> M.Map T.Text Wiki
topLevelTemplates wiki = M.fromList [(T.toLower title,WikiTemplate title uParams oParams)|WikiTemplate title uParams oParams <- wikiToList wiki ]

findTemplate :: T.Text -> Wiki -> Maybe Wiki
findTemplate target = getFirst . foldMap (First . 
    \case 
        t@(WikiTemplate title _ _) -> if T.toLower title == target then Just t else Nothing
        _ -> Nothing
    ) . wikiToList

data Infobox = Infobox [String] [String]

getInfobox :: Wiki -> Either HistoryError Infobox
getInfobox wiki = case (findTemplate "infobox former country" wiki,
                        findTemplate "infobox former subdivision" wiki) of
        (Just _, Just _) -> Left DoubleInfobox
        (Just (WikiTemplate title _ props), Nothing) -> note InfoboxInterpretationError $
            Infobox <$> conn props 'p' <*> conn props 's'
        (Nothing, Just (WikiTemplate title _ props)) -> note InfoboxInterpretationError $
            Infobox <$> conn props 'p' <*> conn props 's'
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

getConnected :: Sess.Session -> String -> IO (Either HistoryError (String,([String],[String])))
getConnected sess target = runEitherT $ do
    (cannonicalName,wiki) <- getWiki sess target
    --The endOfInput won't work unless the wiki parser is improved.
    --See the result for French Thrid Republic for a hint.
    --parse <- EitherT $ return $ (_Left%~WikiParseError) $ parseOnly (wikiParser<*endOfInput) wiki
    parse <- EitherT $ return $ (_Left%~WikiParseError) $ parseOnly wikiParser wiki
    --lift $ print parse
    Infobox p s <- EitherT $ return $ getInfobox parse
    return (cannonicalName,(p,s))

test :: String -> IO ()
test str = void $ Sess.withSession $ \sess -> runEitherT $ do
    (cannonicalName,wiki) <- getWiki sess str
    parseResult <- parseWith (return "") (wikiParser<*endOfInput) wiki
    case parseResult of
        AP.Done unused parsed -> do
            lift $ print $ last $ wikiToList parsed
            lift $ print unused
        AP.Fail unused contexts err -> do
            lift $ print err
            lift $ print unused

data NationGraph = NationGraph (M.Map String ([String],[String])) (M.Map String String) [String]
instance Show NationGraph where
    show (NationGraph graph synonyms todo) =
        M.foldWithKey (\ k (p,s) rest -> k ++
                "\n\tprecursors:" ++
                concatMap ("\n\t\t"++) p ++
                "\n\tsuccessors:" ++
                concatMap ("\n\t\t"++) s ++
                "\n" ++ rest
            ) ("remaining:"++show filteredTodo++"\nsynonyms:"++ show synonyms) graph
        where filteredTodo = [bestName|name <- todo,
                                let bestName = fromMaybe name $ M.lookup name synonyms,
                                not $ bestName `M.member` graph]

getNext :: Sess.Session -> NationGraph -> IO NationGraph
getNext _ sg@(NationGraph _ _ []) = return sg
getNext sess (NationGraph graph synonyms (next:stack)) =
    if bestName `M.member` graph
    then getNext sess (NationGraph graph synonyms stack)
    else do
        result <- getConnected sess bestName
        case result of
            Left err -> do
                putStrLn $ show err ++ " for " ++ next
                getNext sess $ NationGraph graph synonyms stack
            Right (name,(p,s)) -> do
                if M.member name graph
                then getNext sess $ NationGraph graph newSynonyms stack
                else return $ NationGraph (M.insert name (p,s) graph) newSynonyms $ p ++ s ++ stack
                where newSynonyms =
                        if next == name
                        then synonyms
                        else M.insert next name synonyms
    where
        bestName = fromMaybe next (M.lookup next synonyms)

initialGraph = NationGraph M.empty M.empty ["Roman Empire"] 

doIt :: Int -> NationGraph -> IO NationGraph
doIt n graph= Sess.withSession (\ sess -> doIt' sess n graph) where
    doIt' :: Sess.Session -> Int -> NationGraph -> IO NationGraph
    doIt' _ 0 graph = return $ graph
    doIt' sess n graph = do
        next <- getNext sess graph
        doIt' sess (n-1) next

nationsList = ["Turkic Khaganate","Second Turkic Khaganate","Turgesh","Oghuz Yabgu State",
               "Khazar Khaganate","Second Turkic Khaganate","Khazar Khaganate",
               "Seljuq Empire","Kimek Khanate","Ghaznavid Empire","Buyid dynasty",
               "Byzantine Empire","Kakuyids","Sultanate of R\251m","Anatolian beyliks",
               "Ghurid Dynasty","Khwarezmian Empire","Ayyubid dynasty",
               "Atabegs of Azerbaijan","Burid dynasty","Zengid dynasty","Danishmends",
               "Artuqid dynasty","Saltukids","Danishmends","Mengujekids","Saltukids",
               "Artukids","Anatolian Beyliks","Ottoman Empire","Ilkhanate",
               "Armenian Kingdom of Cilicia","Byzantine Empire","Karamanids",
               "Kingdom of Bosnia","Second Bulgarian Empire","Serbian Empire",
               "Kingdom of Hungary (1000\8211\&1538)","Kingdom of Croatia (1102\8211\&1526)",
               "Mamluk Sultanate (Cairo)","Hafsid dynasty",
               "History of Malta under the Order of Saint John","Zayyanid dynasty",
               "Empire of Trebizond","Despotate of the Morea",
               "Government of the Grand National Assembly","First Hellenic Republic",
               "Khedivate of Egypt","Condominium of Bosnia and Herzegovina",
               "Principality of Serbia","Provisional Government of Albania",
               "Kingdom of Romania","Principality of Bulgaria",
               "British Cyprus (1914\8211\&1960)","Mandatory Iraq","Emirate of Diriyah",
               "French Algeria","Mutawakkilite Kingdom of Yemen",
               "French protectorate of Tunisia","Sheikhdom of Kuwait"]

toTGF :: NationGraph -> String
toTGF (NationGraph graph synonyms _) = let
    cannonical :: String -> String
    cannonical name = fromMaybe name $ M.lookup name synonyms
    cannonicalGraph = traverse.both.traverse%~cannonical $ graph
    nodes = M.foldWithKey (\ name (precursors,successors) acc -> S.unions
        [S.singleton name,
         S.fromList precursors,
         S.fromList successors,
         acc]) S.empty cannonicalGraph
    numberedNodes = M.fromAscList $ zip (S.toAscList nodes) [1..]
    indexOf = (`M.lookup` numberedNodes)
    edges = ifoldMap (\ name (precursors,successors) ->
        S.fromList [(indexOf p, indexOf name)|p<-precursors] <>
        S.fromList [(indexOf name, indexOf s)|s<-successors]) cannonicalGraph
    nodesStr = ifoldMap (\name i -> show i ++ " " ++ name ++ "\n") numberedNodes
    edgesStr = foldMap (\(Just i,Just j) -> show i ++ " " ++ show j ++ "\n") edges
    in nodesStr ++ "#\n" ++ edgesStr

main = do
    result <- doIt 10 initialGraph
    print result
    writeFile "./out.tgf" $ toTGF result
--main = do
--    print $ length nationsList
--    Sess.withSession $ \ sess ->
--        traverse (getConnected sess >=> print) $ Data.List.take 50 nationsList

