module Paths_graphviz (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [2999,17,0,3] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/vagrant/Nations-Graph/.cabal-sandbox/bin"
libdir     = "/home/vagrant/Nations-Graph/.cabal-sandbox/lib/x86_64-linux-ghc-7.10.1/graph_DvTyMBhy13m8pOoaPIAgk5"
datadir    = "/home/vagrant/Nations-Graph/.cabal-sandbox/share/x86_64-linux-ghc-7.10.1/graphviz-2999.17.0.3"
libexecdir = "/home/vagrant/Nations-Graph/.cabal-sandbox/libexec"
sysconfdir = "/home/vagrant/Nations-Graph/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "graphviz_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "graphviz_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "graphviz_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "graphviz_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "graphviz_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
