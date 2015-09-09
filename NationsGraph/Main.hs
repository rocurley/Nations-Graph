{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import NationsGraph.Types
import NationsGraph.BuildGraph
import NationsGraph.GraphConversion

import Control.Monad.Trans.Writer

import Data.Monoid

import qualified Data.Map as M
import qualified Data.Set as S

import Data.Aeson.Encode.Pretty

import qualified Data.Text.Lazy.IO as LTIO
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.String.Conversions

import qualified Network.Wreq.Session as Sess

import System.Environment

import Data.GraphViz
import Data.GraphViz.Printing hiding ((<>))

import Data.Aeson
import qualified Text.XML
import Data.Graph.Inductive (Gr)

import Web.Scotty

import Criterion.Main
import Criterion.Types
import Criterion.Main.Options

--import Network.Wai.Middleware.Gzip

--TODO:
--Non-ascii characters

initialGraph = BuildingNationGraph M.empty M.empty M.empty ["Judea (Roman province)"]

doIt :: Int -> BuildingNationGraph -> IO (BuildingNationGraph, ErrorLog)
doIt n graph = Sess.withSession (\ wikipediaSession -> (Sess.withSession (\ wikimediaSession ->
  runWriterT $ doIt' wikipediaSession wikimediaSession n graph))) where
    doIt' :: Sess.Session -> Sess.Session -> Int -> BuildingNationGraph -> WriterT ErrorLog IO BuildingNationGraph
    doIt' _ _ 0 graph = return $ graph
    doIt' wikipediaSession wikimediaSession n graph = do
        next <- getNext wikipediaSession wikimediaSession graph
        doIt' wikipediaSession wikimediaSession (n-1) next

loadLayoutGraph :: IO (Gr NationValue ())
loadLayoutGraph = do
    json <- BSC.readFile "./out.json"
    let Just graph = decode json
    graphml <- Text.XML.readFile Text.XML.def "./out.graphml"
    return $ addPositionsToGraph
        (M.fromList $ loadPositionsFromGraphml graphml)
        graph

data WebOptions = WebOptions{svgOnly :: Bool}

runWebserver :: WebOptions -> IO ()
runWebserver opts= do
    json <- BSC.readFile "./out.json"
    --css <- cs <$> BSC.readFile "./style.css"
    let Just graph = decode json
    svg <- fillInSvg graph <$> Text.XML.readFile Text.XML.def "./out.svg"
    template <- if svgOnly opts
                then return undefined
                else Text.XML.readFile Text.XML.def "./base.xhtml"
    let page = mergeSvgXHTML svg template
    scotty 3000 $ do
        --middleware $ gzip def
        get "/" $
            if svgOnly opts
            then do
                html $
                    Text.XML.renderText Text.XML.def svg
                setHeader "Content-Type" "image/svg+xml"
            else do
                html $
                    --Text.XML.renderText (Text.XML.def{Text.XML.rsPretty = True})
                    Text.XML.renderText Text.XML.def page
                setHeader "Content-Type" "application/xhtml+xml"
        get "/style.css" $ do
            file "./style.css"
            setHeader "Content-Type" "text/css"
        get "/code.js" $ do
            file "./code.js"
            setHeader "Content-Type" "application/javascript"
        get "/d3.js" $ do
            file "./d3.js"
            setHeader "Content-Type" "application/javascript"

runDownload :: Int -> IO ()
runDownload n = do
    (result,errors) <- doIt n initialGraph
    putStrLn $ showBNG result errors
    let fglResult = toFGL result
    writeFile "./out.tgf" $ toUnlabeledTGF fglResult
    BSC.writeFile "./out.json" $ encodePretty fglResult

runJoin :: IO ()
runJoin = do
    fglResult <- loadLayoutGraph
    --runGraphviz (toGV fglResult) Svg "./out.svg" TODO: Support loading positions
    --LTIO.writeFile "out.dot" $ renderDot $ toDot $ toGV fglResult
    --writeFile "./out.tgf" $ toTGF fglResult
    BSC.writeFile "./out2.json" $ encodePretty fglResult

mainWithArgs :: [String] -> IO ()
mainWithArgs ["download", nStr] = runDownload (read nStr)
mainWithArgs ["join"] = runJoin
mainWithArgs ("web":webArgs) = let
    webArgsSet = S.fromList webArgs
    options = WebOptions{
            svgOnly = "--svg-only" `S.member` webArgsSet
        }
    in runWebserver options
mainWithArgs ("bench" : args) = let
    config = defaultConfig {reportFile = Just("./benchmark.html")}
    in defaultMainWith config [
      bench (unwords args) $ nfIO $ mainWithArgs args
    ]
mainWithArgs _ = putStrLn "Invalid args"

main = getArgs >>= mainWithArgs
