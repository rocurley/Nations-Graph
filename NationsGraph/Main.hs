{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import NationsGraph.Types
import NationsGraph.BuildGraph
import NationsGraph.GraphConversion

import Data.Monoid

import qualified Data.Map as M

import Data.Aeson.Encode.Pretty

import qualified Data.Text.Lazy.IO as LTIO
import qualified Data.ByteString.Lazy.Char8 as BSC

import qualified Network.Wreq.Session as Sess

import System.Environment

import Data.GraphViz
import Data.GraphViz.Printing hiding ((<>))

import Data.Aeson
import qualified Text.XML
import Data.Graph.Inductive (Gr)

import Web.Scotty

--TODO:
--Non-ascii characters

initialGraph = BuildingNationGraph M.empty M.empty M.empty ["Roman Empire"] M.empty

doIt :: Int -> BuildingNationGraph -> IO BuildingNationGraph
doIt n graph= Sess.withSession (\ sess -> doIt' sess n graph) where
    doIt' :: Sess.Session -> Int -> BuildingNationGraph -> IO BuildingNationGraph
    doIt' _ 0 graph = return $ graph
    doIt' sess n graph = do
        next <- getNext sess graph
        doIt' sess (n-1) next

writeGraph = do
    result <- doIt 10 initialGraph
    print result
    let fglResult = toFGL result
    BSC.writeFile "./out.json" $ encodePretty fglResult
    writeFile "./out.tgf" $ toUnlabeledTGF fglResult

loadLayoutGraph :: IO (Gr NationValue ())
loadLayoutGraph = do
    json <- BSC.readFile "./out.json"
    let Just graph = decode json
    graphml <- Text.XML.readFile Text.XML.def "./out.graphml"
    return $ addPositionsToGraph
        (M.fromList $ loadPositionsFromGraphml graphml)
        graph

runWebserver :: IO ()
runWebserver = do
    json <- BSC.readFile "./out.json"
    let Just graph = decode json
    svg <- Text.XML.readFile Text.XML.def "./out.svg"
    template <- Text.XML.readFile Text.XML.def "./base.xhtml"
    let page = mergeSvgXHTML (fillInSvg graph svg) template
    scotty 3000 $ do
        get "/" $ do
            --html $ "<!DOCTYPE html>" <>
            html $ 
                --Text.XML.renderText (Text.XML.def{Text.XML.rsPretty = True})
                Text.XML.renderText Text.XML.def page
            setHeader "Content-Type" "application/xhtml+xml"
        get "/style.css" $ do
            file "./style.css"
            setHeader "Content-Type" "text/css"
            

main = do
    args <- getArgs
    case args of
        ["download",nStr] -> do
            let n = read nStr
            result <- doIt n initialGraph
            print result
            let fglResult = toFGL result
            writeFile "./out.tgf" $ toUnlabeledTGF fglResult
            BSC.writeFile "./out.json" $ encodePretty fglResult
        ["join"] -> do
            fglResult <- loadLayoutGraph
            --runGraphviz (toGV fglResult) Svg "./out.svg" TODO: Support loading positions
            --LTIO.writeFile "out.dot" $ renderDot $ toDot $ toGV fglResult
            --writeFile "./out.tgf" $ toTGF fglResult
            BSC.writeFile "./out2.json" $ encodePretty fglResult
        ["web"] -> runWebserver
        otherwise -> putStrLn "Invalid args"
