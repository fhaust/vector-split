


module Elt where

import           Test.QuickCheck
import           Test.QuickCheck.Function

import           Data.Char


newtype Elt = Elt { unElt :: Char } deriving (Eq)

type Elements = [Elt]
type SomeElements = NonEmptyList Elt

instance Show Elt where
    show (Elt c) = show c

instance Arbitrary Elt where
    arbitrary = elements (map Elt "abcde")

instance CoArbitrary Elt where
  coarbitrary = coarbitrary . ord . unElt


instance Function Elt where
  function = functionMap unElt Elt
