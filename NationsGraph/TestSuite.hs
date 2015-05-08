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

prop_intervalStabbing :: Int -> [((Int,Int),Char)] -> Bool
prop_intervalStabbing x intervals = let
    properIntervals = map (first $ uncurry min &&& uncurry max) intervals
    in sort (intervalIntersect (fromList properIntervals) x) ==
        sort [((l,r),c)|((l,r),c)<- properIntervals,l <= x && x <= r]

prop_rectangleStabbing :: (Int,Int) -> [((Int,Int),(Int,Int))] -> Bool
prop_rectangleStabbing (x,y) intervals = let
    properIntervals = map
        ((uncurry min &&& uncurry max)***(uncurry min &&& uncurry max))
        intervals
    in sort (rectangleIntersect (iTreeFromList properIntervals) (x,y)) ==
        sort [((l,r),(u,d))|((l,r),(u,d))<- properIntervals,
            l <= x, x <= r, u <= y, y <= d]

main :: IO ()
main = defaultMain tests

tests :: [TF.Test]
tests = [
        testGroup "QuickCheck NationsGraph.GraphConversion" [
                testProperty "toFromJson"
                	prop_toFromJson
                ]
                testProperty "Stabbing Queries" [
                	prop_intervalStabbing,
                	prop_rectangleStabbing
                ]
       ]