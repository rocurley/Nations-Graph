{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module NationsGraph.Wiki (
    redirectParser,
    wikiParser,
    getInfobox,
    wikiToList,
    findTemplate,
) where

import NationsGraph.Types

import qualified Data.Map as M
import Data.Monoid
import Data.Maybe
import Data.Char
import Data.Foldable (foldMap)

import Control.Monad
import Control.Applicative as A

import Control.Lens

import qualified Data.Text as T

import qualified Data.Attoparsec.Text as AP

import Control.Error.Util

import Safe

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
        Just c' -> if c == c' then A.empty else return c
        Nothing -> return c

xmlName :: AP.Parser T.Text
xmlName = do
    c1 <- AP.letter <|> AP.char '_' <|> AP.char ':'
    rest <- AP.takeWhile $ \ c ->
        isLetter c || isDigit c || (c `elem` (".-_:" :: String))
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
            (WikiText <$> T.singleton <$> foldr ((<|>) . nonDouble) A.empty ("{}[]"::String)) <|>
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

yearParser :: AP.Parser Int
yearParser = do
    digits <- many $ AP.satisfy isDigit
    absYear <- maybe (fail "Year did not read as Int") return $ readMay digits 
    (AP.endOfInput >> return absYear) <|> do
        many (AP.satisfy isSeparator)
        "BC"
        AP.endOfInput
        return $ -absYear

readYearMay :: T.Text -> Maybe Int
readYearMay = either (const Nothing) Just .
    AP.parseOnly yearParser . T.strip

findTemplate :: T.Text -> Wiki -> Maybe Wiki
findTemplate target = getFirst . foldMap (First . 
    \case 
        t@(WikiTemplate title _ _) -> if T.toLower title == target then Just t else Nothing
        _ -> Nothing
    ) . wikiToList

propLookup :: T.Text -> M.Map T.Text Wiki -> Maybe T.Text
propLookup prop props = case M.lookup prop props of
    Just (WikiText x)   -> Just $ x
    Just (WikiLink x _) -> Just $ x
    Just (WikiText x :> _) -> Just $ x
    Just (WikiLink x _ :> _) -> Just $ x
    _ -> Nothing

getInfobox :: Wiki -> Either HistoryError Infobox
getInfobox wiki = case (findTemplate "infobox former country" wiki,
                        findTemplate "infobox former subdivision" wiki) of
        (Just _, Just _) -> Left DoubleInfobox
        (Just (WikiTemplate title _ props), Nothing) ->
            note InfoboxInterpretationError $
                NationInfobox <$>
                name props <*>
                pure (startYear props) <*>
                pure (endYear props) <*>
                conn props 'p' <*>
                conn props 's'
        (Nothing, Just (WikiTemplate title _ props)) ->
            note InfoboxInterpretationError $
                SubdivisionInfobox <$>
                name props <*>
                pure (startYear props) <*>
                pure (endYear props) <*>
                conn props 'p' <*>
                conn props 's'<*>
                pure (parents props)
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
            name = fmap (T.unpack . T.strip) . propLookup "conventional_long_name"
            startYear :: M.Map T.Text Wiki -> Maybe Int
            startYear = readYearMay <=< propLookup "year_start"
            endYear :: M.Map T.Text Wiki -> Maybe Int
            endYear = readYearMay <=< propLookup "year_end"