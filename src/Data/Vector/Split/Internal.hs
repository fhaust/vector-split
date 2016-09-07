


module Data.Vector.Split.Internal where


import           Data.Vector.Generic (Vector)
import qualified Data.Vector.Generic as V

import qualified Data.Vector as BV



-- | A delimiter is a list of predicates on elements, matched by some
--   contiguous subsequence of a list.
newtype Delimiter a = Delimiter (BV.Vector (a -> Bool))

-- | Try to match a delimiter at the start of a list, either failing
--   or decomposing the list into the portion which matched the delimiter
--   and the remainder.
matchDelim :: Vector v a => Delimiter a -> v a -> Maybe (v a, v a)
matchDelim (Delimiter ds) xs = if match then Just (V.splitAt (V.length ds) xs) else Nothing
    where match = V.and $ V.zipWith ($) ds (V.convert xs)

isDelim :: Vector v a => Delimiter a -> v a -> Bool
isDelim (Delimiter ds) xs = V.and $ V.zipWith ($) ds (V.convert xs)



data Chunk v a = Delim (v a) | Text (v a)
    deriving (Show, Eq)

type SplitList v a = [Chunk v a]


data Splitter a = Splitter {
    delimiter :: Delimiter a
}


{-split :: Vector v a => Splitter a -> v a -> [v a]-}
{-split splitter v = -}


toSplitList :: Vector v a => Delimiter a -> v a -> SplitList v a
toSplitList d v = go 0
    where go i | i < V.length v = case matchDelim d (V.drop i v) of
                                      Just (l,r) -> Text (V.take i v) : Delim l : toSplitList d r
                                      Nothing    -> go (i+1)
               | otherwise      = [Text v]


dropDelims :: Vector v a => SplitList v a -> SplitList v a
dropDelims = filter go 
    where go (Delim _) = False
          go (Text _ ) = True

keepDelimsL :: Vector v a => SplitList v a -> SplitList v a
keepDelimsL (Delim d : Text t : rst) = Text (d V.++ t) : keepDelimsL rst
keepDelimsL (rs : rst)               = rs : keepDelimsL rst
keepDelimsL []                       = []

keepDelimsR :: Vector v a => SplitList v a -> SplitList v a
keepDelimsR (Text t : Delim d : rst) = Text (t V.++ d) : keepDelimsR rst
keepDelimsR (rs : rst)               = rs : keepDelimsR rst
keepDelimsR []                       = []

condense :: Vector v a => SplitList v a -> SplitList v a
condense (Delim a : Delim b : rst) = condense (Delim (a V.++ b) : rst)
condense (rs : rst)                = rs : condense rst
condense []                        = []

dropInitBlank :: Vector v a => SplitList v a -> SplitList v a
dropInitBlank (Text a : rst) | V.null a = rst
dropInitBlank rst                       = rst

dropFinalBlank :: Vector v a => SplitList v a -> SplitList v a
dropFinalBlank (rst : [Text _]) = [rst]
dropFinalBlank (rs : rst)       = rs : dropFinalBlank rst
dropFinalBlank []               = []

dropInnerBlanks :: Vector v a => SplitList v a -> SplitList v a
dropInnerBlanks (Delim a : Text t : Delim b : rst) | V.null t  = Delim a : Delim b : dropInnerBlanks rst
                                                   | otherwise = Delim a : Text t : dropInnerBlanks (Delim b : rst)
dropInnerBlanks (rs : rst)                                     = rs : dropInnerBlanks rst
dropInnerBlanks []                                             = []

{-oneOf :: (Vector v a, Eq a) => BV.Vector a -> v a -> [v a]-}
{-oneOf ds = L.unfoldr go 0-}
{-    where delim = Delimiter (`V.elem` ds)-}
{-          go i  | i < V.length ds = case matchDelim (Delimiter (`V.elem` ds)) of-}
{-                                      Nothing      -> Just (i+1)-}
{-                                      (Just (l,r)) ->  -}
