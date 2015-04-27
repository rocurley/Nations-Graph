{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import NationsGraph.Types
import NationsGraph.BuildGraph
import NationsGraph.GraphConversion

import qualified Data.Map as M

import Data.Aeson.Encode.Pretty

import qualified Data.Text.Lazy.IO as LTIO
import qualified Data.ByteString.Lazy.Char8 as BSC

import qualified Network.Wreq.Session as Sess

import Data.GraphViz
import Data.GraphViz.Printing hiding ((<>))

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

main = do
    result <- doIt 10 initialGraph
    print result
    let fglResult = toFGL result
    runGraphviz (toGV fglResult) Svg "./out.svg"
    LTIO.writeFile "out.dot" $ renderDot $ toDot $ toGV fglResult
    writeFile "./out.tgf" $ toTGF fglResult
    writeFile "./out2.tgf" $ toUnlabeledTGF fglResult
    BSC.writeFile "./out.json" $ encodePretty fglResult