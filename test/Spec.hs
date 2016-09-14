
{-# LANGUAGE FlexibleInstances #-}

import           Test.Tasty
import qualified Test.Tasty.QuickCheck as QC

import           Test.QuickCheck
import           Test.QuickCheck.Function

import qualified Data.Vector as V
import qualified Data.Vector.Split as V
import qualified Data.Vector.Split.Internal as V

import qualified Data.List       as L
import qualified Data.List.Split as L

import           Data.Char

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
  , QC.testProperty "splitOn" prop_splitOn
  , QC.testProperty "splitOneOf" prop_splitOneOf
  , QC.testProperty "splitWhen" prop_splitWhen
  , QC.testProperty "endBy" prop_endBy
  , QC.testProperty "endByOneOf" prop_endByOneOf
  , QC.testProperty "wordsBy" prop_wordsBy
  , QC.testProperty "linesBy" prop_linesBy
  , testGroup "basic strategies"
    [ QC.testProperty "oneOf" prop_oneOf
    , QC.testProperty "onSublist" prop_onSublist
    ]
  ]

-- Basic Strategies

prop_oneOf :: String -> String -> Bool
prop_oneOf d = listVsVec (L.split (L.oneOf d)) (V.split (V.oneOf (V.fromList d)))

prop_onSublist :: String -> String -> Bool
prop_onSublist d = listVsVec (L.split (L.onSublist d)) (V.split (V.onSublist (V.fromList d)))

prop_chunksOf :: QC.Positive Int -> [Double] -> Bool
prop_chunksOf (QC.Positive i) = listVsVec (L.chunksOf i) (V.chunksOf i)

prop_splitPlaces :: [QC.Positive Int] -> [Double] -> Bool
prop_splitPlaces is = listVsVec (L.splitPlaces is') (V.splitPlaces is')
    where is' = map QC.getPositive is

prop_splitPlacesBlanks :: [QC.Positive Int] -> [Double] -> Bool
prop_splitPlacesBlanks is = listVsVec (L.splitPlacesBlanks is') (V.splitPlacesBlanks is')
    where is' = map QC.getPositive is

prop_chop :: [Double] -> Bool
prop_chop = listVsVec (L.chop (L.splitAt 10)) (V.chop (V.splitAt 10))

prop_chopGroup :: [Double] -> Bool
prop_chopGroup = listVsVec (L.chop (\xs -> L.span (== L.head xs) xs)) (V.chop (\xs -> V.span (== V.head xs) xs))


prop_divvy :: Positive Int -> QC.Positive Int -> [Double] -> Bool
prop_divvy (QC.Positive n) (QC.Positive m) = listVsVec (L.divvy n m) (V.divvy n m)

prop_splitOn :: NonEmptyList Char -> String -> Bool
prop_splitOn (QC.NonEmpty ds) = listVsVec (L.splitOn ds) (V.splitOn (V.fromList ds))

prop_splitOneOf :: NonEmptyList Char -> String -> Bool
prop_splitOneOf (QC.NonEmpty ds) = listVsVec (L.splitOneOf ds) (V.splitOneOf (V.fromList ds))

prop_splitWhen :: Fun Char Bool -> String -> Bool
prop_splitWhen (Fn f) = listVsVec (L.splitWhen f) (V.splitWhen f)

prop_endBy :: NonEmptyList Char -> String -> Bool
prop_endBy (NonEmpty p) = listVsVec (L.endBy p) (V.endBy (V.fromList p))

prop_endByOneOf :: NonEmptyList Char -> String -> Bool
prop_endByOneOf (NonEmpty p) = listVsVec (L.endByOneOf p) (V.endByOneOf (V.fromList p))

prop_wordsBy :: Fun Char Bool -> String -> Bool
prop_wordsBy (Fn f) = listVsVec (L.wordsBy f) (V.wordsBy f)

prop_linesBy :: Fun Char Bool -> String -> Bool
prop_linesBy (Fn f) = listVsVec (L.linesBy f) (V.wordsBy f)

-- | compare a list splitting vs a vector splitting function
listVsVec :: Eq a => ([a] -> [[a]]) -> (V.Vector a -> [V.Vector a]) -> [a] -> Bool
listVsVec a b l = ls == map V.toList vs
    where v = V.fromList l
          ls = a l
          vs = b v
