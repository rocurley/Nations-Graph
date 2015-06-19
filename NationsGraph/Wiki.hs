{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module NationsGraph.Wiki (
    redirectParser,
    wikiParser,
    getInfobox,
    wikiList,
    findTemplate,
) where

import NationsGraph.Types

import Data.List
import Data.List.NonEmpty hiding (filter,map)
import qualified Data.List.NonEmpty
import qualified Data.Map as M
import Data.Monoid
import Data.Maybe
import Data.Char

import Control.Monad
import Control.Applicative as A

import Control.Lens

import qualified Data.Text as T

import qualified Data.Attoparsec.Text as AP

import Control.Error.Util

import Control.Monad.Trans.Maybe

import Safe

emptyNode :: WikiNode -> Bool
emptyNode (WikiText t) = T.null t
emptyNode _ = False

wikiFlatten :: WikiMarkup -> WikiMarkup
wikiFlatten (WikiMarkup (x:|xs)) = case (x,foldl' wikiFlattenFold [] xs) of
    (WikiText a, WikiText b:xs) -> WikiMarkup $ WikiText (a<>b) :| xs
    (_,ys) -> WikiMarkup $ x:|ys
wikiFlattenFold :: [WikiNode] -> WikiNode -> [WikiNode]
wikiFlattenFold (WikiText b:acc) (WikiText a) =
    WikiText (a <> b) : acc
wikiFlattenFold acc a = a:acc

nonDouble :: Char -> AP.Parser Char
nonDouble c = do
    AP.char c
    peek <- AP.peekChar
    case peek of
        Just c' -> if c == c' then A.empty else return c
        Nothing -> return c

xmlComment :: AP.Parser WikiNode
xmlComment = do
    "<!--"
    content <- many $ nonDash <|> singleDash
    "-->"
    return $ WikiComment $ T.pack $ concat content
    where
    nonDash = (:[]) <$> AP.notChar '-'
    singleDash = do
        AP.char '-'
        c <- AP.notChar '-'
        return ['-',c]

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
    "</" <|> "<"
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

wikiParser = do
    begin <-xmlComment <|>
            wikiHTMLTagParser <|>
            wikiLinkParser <|>
            wikiTemplateParser <|>
            WikiText <$> ("<"<|>">") <|>
            (WikiText <$> T.singleton <$> foldr ((<|>) . nonDouble) A.empty ("{}[]"::String)) <|>
            (WikiText <$> AP.takeWhile (AP.notInClass "{}[]<>|"))
    if emptyNode begin
    then return $ WikiMarkup $ begin:|[]
    else (AP.endOfInput >> (return $ WikiMarkup $ begin:|[])) <|> do
        WikiMarkup (n:|ns) <- wikiParser
        return $ if emptyNode n
            then WikiMarkup $ begin:|[]
            else WikiMarkup $ begin:|(n:ns)

wikiHTMLTagParser :: AP.Parser WikiNode
wikiHTMLTagParser = do
    (name, attributes) <- xmlTag
    return $ WikiHTMLTag name attributes

wikiLinkParser :: AP.Parser WikiNode
wikiLinkParser = do
    "[["
    first <- AP.takeWhile (\ c -> c /= '|' && c /= ']')
    peek <- AP.peekChar
    rest <- many $ "|" *> wikiParser
    "]]"
    return $ WikiLink first rest

wikiTemplateNamedParameter :: AP.Parser (T.Text,WikiMarkup)
wikiTemplateNamedParameter = do
    "|"
    key <- AP.takeWhile (\ c -> c /='|' && c /= '=' && c /= '}')
    "="
    value <- wikiParser
    return (T.strip key,value)

wikiTemplateUnNamedParameter :: AP.Parser WikiMarkup
wikiTemplateUnNamedParameter = do
    "|"
    wikiParser

wikiTemplateParser :: AP.Parser WikiNode
wikiTemplateParser = do
    "{{"
    title <- AP.takeWhile (\ c -> c /= '|' && c /= '}')
    parameters <- A.many $ fmap Right wikiTemplateNamedParameter <|> fmap Left wikiTemplateUnNamedParameter
    "}}"
    return $ WikiTemplate (T.strip title) [x | Left x <- parameters] $ M.fromList [x | Right x <- parameters]

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

readYear :: String -> Either HistoryError Int
readYear = either (const $ Left $ PropInterpretationError "year") Right .
    AP.parseOnly yearParser . T.strip . T.pack

findTemplate :: T.Text -> WikiMarkup -> Maybe WikiNode
findTemplate target = getFirst . foldMap (First .
    \case
        t@(WikiTemplate title _ _) -> if T.toLower title == target then Just t else Nothing
        _ -> Nothing
    ) . wikiList

getPropAsText :: T.Text -> AutoLinkedFlag -> M.Map T.Text WikiMarkup -> Either HistoryError (Maybe T.Text)
getPropAsText propName isAutoLinked propMap = sequence $ do
    propVal <- M.lookup propName propMap
    return $ case (wikiList <$> wikiFilterNonText propVal,isAutoLinked) of
        (Just (WikiText x:|_), _)   -> Right x
        (Just (WikiLink x _:|_), NotAutoLinked) -> Right x
        (Just (WikiText text :| WikiTemplate "!" _ _ : _), AutoLinked) -> Right text
        _ -> Left $ PropInterpretationError propName

wikiFilterNonText :: WikiMarkup -> Maybe WikiMarkup
wikiFilterNonText =
    fmap WikiMarkup . nonEmpty .
    Data.List.NonEmpty.filter (\case
        WikiComment _ -> False
        WikiHTMLTag name _ -> case map toLower $ T.unpack name of
            "br" -> False
            "small" -> False
            _ -> True
        _ -> True
    ) .
    wikiList

getInfobox :: WikiMarkup -> ErrorHandling Infobox
getInfobox wiki = case (findTemplate "infobox former country" wiki,
                        findTemplate "infobox former subdivision" wiki) of
        (Just _, Just _) -> raiseError $ Left DoubleInfobox
        (Just (WikiTemplate title _ props), Nothing) ->
              NationInfobox <$>
              name props <*>
              startYear props <*>
              endYear props <*>
              conn props 'p' <*>
              conn props 's'
        (Nothing, Just (WikiTemplate title _ props)) ->
              SubdivisionInfobox <$>
              name props <*>
              startYear props <*>
              endYear props <*>
              conn props 'p' <*>
              conn props 's'<*>
              pure (parents props)
        (Nothing,Nothing) -> raiseError $ Left MissingInfobox
        where
            conn :: M.Map T.Text WikiMarkup -> Char -> ErrorHandling [String]
            conn props ty = do
                rawPropValues <- (sequence [getOptionalField propName AutoLinked props|
                    i<-[1..15], let propName = T.pack $ ty:show i])
                return $ filter (not . null) $ catMaybes rawPropValues

            parents :: M.Map T.Text WikiMarkup -> [String]
            parents props = [T.unpack nationName | WikiLink nationName _<- props^.ix "nation".to (toList.wikiList)]

            getMandatoryField :: T.Text -> AutoLinkedFlag -> M.Map T.Text WikiMarkup -> ErrorHandling String
            getMandatoryField fieldName isAutoLinked props = do
                fieldMay <- raiseError $ getPropAsText fieldName isAutoLinked props
                field <- raiseError $ note (MissingInfoboxFieldError fieldName) fieldMay
                return $ T.unpack $ T.strip $ field

            getOptionalField :: T.Text -> AutoLinkedFlag -> M.Map T.Text WikiMarkup -> ErrorHandling (Maybe String)
            getOptionalField fieldName isAutoLinked props = do
                fieldMay <- fmap join $ discardError $ getPropAsText fieldName isAutoLinked props
                return $ T.unpack . T.strip <$> fieldMay

            name :: M.Map T.Text WikiMarkup -> ErrorHandling String
            name = getMandatoryField "conventional_long_name" NotAutoLinked

            startYear :: M.Map T.Text WikiMarkup -> ErrorHandling (Maybe Int)
            startYear props = runMaybeT $ do
                yearText <- MaybeT $ getOptionalField "year_start" NotAutoLinked props
                MaybeT $ discardError $ readYear yearText

            endYear :: M.Map T.Text WikiMarkup -> ErrorHandling (Maybe Int)
            endYear props = runMaybeT $ do
                yearText <- MaybeT $ getOptionalField "year_end" NotAutoLinked props
                MaybeT $ discardError $ readYear yearText
