


module Data.Vector.Split.Internal where


import           Data.Vector.Generic (Vector)
import qualified Data.Vector.Generic as V

import qualified Data.Vector as BV

import qualified Data.List as L



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



{-oneOf :: (Vector v a, Eq a) => BV.Vector a -> v a -> [v a]-}
{-oneOf ds = L.unfoldr go 0-}
{-    where delim = Delimiter (`V.elem` ds)-}
{-          go i  | i < V.length ds = case matchDelim (Delimiter (`V.elem` ds)) of-}
{-                                      Nothing      -> Just (i+1)-}
{-                                      (Just (l,r)) ->  -}
