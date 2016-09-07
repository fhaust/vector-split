
import           Test.Tasty
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.QuickCheck.Function  as QC

import qualified Data.Vector as V
import qualified Data.Vector.Split as V

import qualified Data.List       as L
import qualified Data.List.Split as L

import           Debug.Trace

main :: IO ()
main = defaultMain tests



tests = testGroup "tests" 
  [ QC.testProperty "chunksOf" prop_chunksOf
  , QC.testProperty "splitPlaces" prop_splitPlaces
  , QC.testProperty "splitPlacesBlanks" prop_splitPlacesBlanks
  , QC.testProperty "chop" prop_chop
  , QC.testProperty "chopGroup" prop_chopGroup
  , QC.testProperty "divvy" prop_divvy
  ]




prop_chunksOf :: QC.Positive Int -> [Double] -> Bool
prop_chunksOf (QC.Positive i) l = ls == map V.toList vs
    where v  = V.fromList l 
          vs = V.chunksOf i v
          ls = L.chunksOf i l

prop_splitPlaces :: [QC.Positive Int] -> [Double] -> Bool
prop_splitPlaces is l = ls == map V.toList vs
    where v   = V.fromList l
          vs  = V.splitPlaces is' v
          ls  = L.splitPlaces is' l
          is' = map QC.getPositive is


prop_splitPlacesBlanks :: [QC.Positive Int] -> [Double] -> Bool
prop_splitPlacesBlanks is l = ls == map V.toList vs
    where v   = V.fromList l
          vs  = V.splitPlacesBlanks is' v
          ls  = L.splitPlacesBlanks is' l
          is' = map QC.getPositive is


prop_chop :: [Double] -> Bool
prop_chop l = ls == map V.toList vs
    where v   = V.fromList l
          vs  = V.chop (V.splitAt 10) v
          ls  = L.chop (L.splitAt 10) l

prop_chopGroup :: [Double] -> Bool
prop_chopGroup l = ls == map V.toList vs
    where v   = V.fromList l
          vs  = V.chop (\xs -> V.span (== V.head xs) xs) v
          ls  = L.chop (\xs -> L.span (== L.head xs) xs) l


prop_divvy :: QC.Positive Int -> QC.Positive Int -> [Double] -> Bool
prop_divvy (QC.Positive n) (QC.Positive m) l = ls == map V.toList vs
    where v   = V.fromList l
          vs  = V.divvy n m v
          ls  = L.divvy n m l
