:ext LambdaCase
:ext OverloadedStrings
:ext TupleSections
import Network.HTTP
import Data.Aeson
import Data.Aeson.Lens
import Control.Lens
import qualified Data.Text as T
import Control.Monad.Trans.Maybe
import Control.Monad.Trans
import Control.Monad
import qualified Data.Text.IO as TIO
import Data.Attoparsec.Text as AP
import Control.Applicative as A
import Data.Monoid
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Data.Char
import Control.Monad.Trans.Either
import Control.Error.Util
import Network.Stream hiding (failWith)
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.Foldable (foldMap)

baseUrl = "http://en.wikipedia.org/w/api.php?action=query&prop=revisions&rvprop=content&format=json&titles="

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

data HistoryError = HTTPError ConnError | JsonParseError | WikiParseError String | MissingInfobox | InfoboxInterpretationError deriving Show


getWiki :: String -> EitherT HistoryError IO (String,T.Text)
getWiki article = do
    resp <- (EitherT $ (_Left%~HTTPError) <$> simpleHTTP (getRequest $ baseUrl ++ urlEncode article):: EitherT HistoryError IO (Response String))
    let json = decode $ BSC.pack $ rspBody resp
    source <- failWith JsonParseError $ join $ json^?key "query".key "pages".traverseObject.key "revisions".nth 0.key "*"
    case parseOnly redirectParser source of
        Right link -> getWiki $ T.unpack link
        Left _ -> return (article,source)

topLevelTemplates :: Wiki -> M.Map T.Text Wiki
topLevelTemplates wiki = M.fromList [(T.toLower title,WikiTemplate title uParams oParams)|WikiTemplate title uParams oParams <- wikiToList wiki ]

findTemplate :: T.Text -> Wiki -> Maybe Wiki
findTemplate target = getFirst . foldMap (First . 
    \case 
        t@(WikiTemplate title _ _) -> if T.toLower title == target then Just t else Nothing
        _ -> Nothing
    ) . wikiToList

getConnected :: String -> IO (Either HistoryError (String,([String],[String])))
getConnected target = runEitherT $ do
    (cannonicalName,wiki) <- getWiki target
    --The endOfInput won't work unless the wiki parser is improved.
    --See the result for French Thrid Republic for a hint.
    --parse <- EitherT $ return $ (_Left%~WikiParseError) $ parseOnly (wikiParser<*endOfInput) wiki
    parse <- EitherT $ return $ (_Left%~WikiParseError) $ parseOnly wikiParser wiki
    --lift $ print parse
    infobox <- failWith MissingInfobox $ findTemplate "infobox former country" parse
    let WikiTemplate title _ props = infobox
    --lift $ traverse print $M.toList props
    let conn ty = let
            raw = mapMaybe (\ i -> M.lookup (T.pack $ ty:show i) props) [1..15]
            in filter (not . null) <$> map (T.unpack . T.strip) <$> raw `forM` (\case
                    WikiText text -> Just text
                    WikiText text :> (WikiTemplate t _ _ :> _) -> Just text
                    _ -> Nothing
                )
    p <- failWith InfoboxInterpretationError $ conn 'p' 
    s <- failWith InfoboxInterpretationError $ conn 's' 
    return (cannonicalName,(p,s))

runEitherT $ do
    (cannonicalName,wiki) <- getWiki "Occupied France"
    parseResult <- parseWith (return "") (wikiParser<*endOfInput) wiki
    case parseResult of
        AP.Done unused parsed -> do
            lift $ print $ last $ wikiToList parsed
            lift $ print unused
        AP.Fail unused contexts err -> do
            lift $ print err
            lift $ print unused

data StateGraph = StateGraph (M.Map String ([String],[String])) [String]
instance Show StateGraph where
    show (StateGraph graph todo) =
        M.foldWithKey (\ k (p,s) rest -> k ++
                "\n\tprecursors:" ++
                concatMap ("\n\t\t"++) p ++
                "\n\tsuccessors:" ++
                concatMap ("\n\t\t"++) s ++
                "\n" ++ rest
            ) ("remaining:"++show todo) graph

getNext :: StateGraph -> IO StateGraph
getNext sg@(StateGraph _ []) = return sg
getNext (StateGraph graph (next:stack)) =
    if M.member next graph
    then getNext (StateGraph graph stack)
    else do
        result <- getConnected next
        case result of
            Left err -> do
                print $ show err ++ " for " ++ next
                getNext $ StateGraph graph stack
            Right (name,(p,s)) -> do
                if M.member name graph
                then getNext $ StateGraph graph stack
                else return $ StateGraph (M.insert name (p,s) graph) $ p ++ s ++ stack

initialGraph = StateGraph M.empty ["Roman Empire"] 

doIt :: Int -> StateGraph -> IO StateGraph
doIt 0 graph = return $ graph
doIt n graph = do
    next <- getNext graph
    doIt (n-1) next

main = print $ doIt 10 initialGraph

