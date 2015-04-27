import NationsGraph.GraphConversion
import NationsGraph.Types

import Data.Graph.Inductive.Arbitrary

import Test.QuickCheck
import Test.Framework as TF (defaultMain, testGroup, Test)
--import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Aeson

import Data.Graph.Inductive (Gr)

prop_toFromJson :: Gr NationValue () -> Bool
prop_toFromJson gr =  fromJSON (toJSON gr) == Data.Aeson.Success gr

main :: IO ()
main = defaultMain tests

tests :: [TF.Test]
tests = [
        testGroup "QuickCheck NationsGraph.GraphConversion" [
                testProperty "toFromJson"
                	prop_toFromJson
                ]
       ]