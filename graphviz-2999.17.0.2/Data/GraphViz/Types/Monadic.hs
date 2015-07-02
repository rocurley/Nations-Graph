{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

{- |
   Module      : Data.GraphViz.Types.Monadic
   Description : A monadic interface for making Dot graphs.
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   This module is based upon the /dotgen/ library by Andy Gill:
   <http://hackage.haskell.org/package/dotgen>

   It provides a monadic interface for constructing generalised Dot
   graphs.  Note that this does /not/ have an instance for @DotRepr@
   (e.g. what would be the point of the @fromCanonical@ function, as
   you can't do anything with the result): it is purely for
   construction purposes.  Use the generalised Dot graph instance for
   printing, etc.

   Note that the generalised Dot graph types are /not/ re-exported, in
   case it causes a clash with other modules you may choose to import.

   The example graph in "Data.GraphViz.Types" can be written as:

   > digraph (Str "G") $ do
   >
   >    cluster (Int 0) $ do
   >        graphAttrs [style filled, color LightGray]
   >        nodeAttrs [style filled, color White]
   >        "a0" --> "a1"
   >        "a1" --> "a2"
   >        "a2" --> "a3"
   >        graphAttrs [textLabel "process #1"]
   >
   >    cluster (Int 1) $ do
   >        nodeAttrs [style filled]
   >        "b0" --> "b1"
   >        "b1" --> "b2"
   >        "b2" --> "b3"
   >        graphAttrs [textLabel "process #2", color Blue]
   >
   >    "start" --> "a0"
   >    "start" --> "b0"
   >    "a1" --> "b3"
   >    "b2" --> "a3"
   >    "a3" --> "end"
   >    "b3" --> "end"
   >
   >    node "start" [shape MDiamond]
   >    node "end" [shape MSquare]

 -}
module Data.GraphViz.Types.Monadic
       ( Dot
       , DotM
       , GraphID(..)
         -- * Creating a generalised DotGraph.
       , digraph
       , digraph'
       , graph
       , graph'
         -- * Adding global attributes.
       , graphAttrs
       , nodeAttrs
       , edgeAttrs
         -- * Adding items to the graph.
         -- ** Clusters
       , cluster
         -- ** Nodes
       , node
       , node'
         -- ** Edges
       , edge
       , (-->)
       , (<->)
       ) where

import Data.GraphViz.Attributes        (Attributes)
import Data.GraphViz.Types.Generalised

import           Control.Applicative (Applicative (..))
import           Data.DList          (DList)
import qualified Data.DList          as DL
import qualified Data.Sequence       as Seq

-- -----------------------------------------------------------------------------
-- The Dot monad.

-- | The monadic representation of a Dot graph.
type Dot n = DotM n ()

-- | The actual monad; as with 'Dot' but allows you to return a value
--   within the do-block.  The actual implementation is based upon the
--   Writer monad.
newtype DotM n a = DotM { runDot :: (a, DotStmts n) }

execDot :: DotM n a -> DotStmts n
execDot = snd . runDot

instance Functor (DotM n) where
  fmap f (DotM (a,stmts)) = DotM (f a, stmts)

instance Applicative (DotM n) where
  pure = DotM . flip (,) DL.empty

  (DotM (f,stmts1)) <*> (DotM (a,stmts2)) = DotM (f a, stmts1 `DL.append` stmts2)

instance Monad (DotM n) where
  return = pure

  dt >>= f = DotM
             $ let ~(a,stmts)  = runDot dt
                   ~(b,stmts') = runDot $ f a
               in (b, stmts `DL.append` stmts')

tell :: DotStmts n -> Dot n
tell = DotM . (,) ()

tellStmt :: DotStmt n -> Dot n
tellStmt = tell . DL.singleton

-- -----------------------------------------------------------------------------
-- Creating the DotGraph

-- | Create a directed dot graph with the specified graph ID.
digraph :: GraphID -> DotM n a -> DotGraph n
digraph = mkGraph True . Just

-- | Create a directed dot graph with no graph ID.
digraph' :: DotM n a -> DotGraph n
digraph' = mkGraph True Nothing

-- | Create a undirected dot graph with the specified graph ID.
graph :: GraphID -> DotM n a -> DotGraph n
graph = mkGraph False . Just

-- | Create a undirected dot graph with no graph ID.
graph' :: DotM n a -> DotGraph n
graph' = mkGraph False Nothing

mkGraph :: Bool -> Maybe GraphID -> DotM n a -> DotGraph n
mkGraph isDir mid dot = DotGraph { strictGraph     = False
                                 , directedGraph   = isDir
                                 , graphID         = mid
                                 , graphStatements = execStmts dot
                                 }

-- -----------------------------------------------------------------------------
-- Statements

type DotStmts n = DList (DotStmt n)

execStmts :: DotM n a -> DotStatements n
execStmts = convertStatements . execDot

convertStatements :: DotStmts n -> DotStatements n
convertStatements = Seq.fromList . map convertStatement . DL.toList

data DotStmt n = MA GlobalAttributes
               | MC (Cluster n)
               | MN (DotNode n)
               | ME (DotEdge n)

convertStatement          :: DotStmt n -> DotStatement n
convertStatement (MA gas) = GA gas
convertStatement (MC cl)  = SG . DotSG True (Just $ clID cl)
                                 . execStmts $ clStmts cl
convertStatement (MN dn)  = DN dn
convertStatement (ME de)  = DE de

-- -----------------------------------------------------------------------------
-- Global Attributes

-- | Add graph/sub-graph/cluster attributes.
graphAttrs :: Attributes -> Dot n
graphAttrs = tellStmt . MA . GraphAttrs

-- | Add global node attributes.
nodeAttrs :: Attributes -> Dot n
nodeAttrs = tellStmt . MA . NodeAttrs

-- | Add global edge attributes
edgeAttrs :: Attributes -> Dot n
edgeAttrs = tellStmt . MA . EdgeAttrs

-- -----------------------------------------------------------------------------
-- Clusters

data Cluster n = Cl { clID    :: GraphID
                    , clStmts :: Dot n
                    }

-- | Add a named cluster to the graph.
cluster     :: GraphID -> DotM n a -> Dot n
cluster cid = tellStmt . MC . Cl cid . (>> return ())

-- -----------------------------------------------------------------------------
-- Nodes

-- | Add a node to the graph.
node   :: n -> Attributes -> Dot n
node n = tellStmt . MN . DotNode n

-- | Add a node with no attributes to the graph.
node' :: n -> Dot n
node' = (`node` [])

-- -----------------------------------------------------------------------------
-- Edges

-- | Add an edge to the graph.
edge     :: n -> n -> Attributes -> Dot n
edge f t = tellStmt . ME . DotEdge f t

-- | Add an edge with no attributes.
(-->) :: n -> n -> Dot n
f --> t = edge f t []

infixr 9 -->

-- | An alias for '-->' to make edges look more undirected.
(<->) :: n -> n -> Dot n
(<->) = (-->)

infixr 9 <->

-- -----------------------------------------------------------------------------
