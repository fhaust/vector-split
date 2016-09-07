


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

type Splitter v a = v a -> SplitList v a




toSplitList :: Vector v a => Delimiter a -> v a -> SplitList v a
toSplitList d v = go 0
    where go i | i < V.length v = case matchDelim d (V.drop i v) of
                                      Just (l,r) -> Text (V.take i v) : Delim l : toSplitList d r
                                      Nothing    -> go (i+1)
               | otherwise      = [Text v]


split :: Vector v a => Splitter v a -> v a -> [v a]
split s v = map go $ s v
    where go (Text t)  = t
          go (Delim d) = d




-- Basic Strategies

-- | A splitting strategy that splits on any one of the given elements. For example:
-- >>> split (oneOf "xyz") "aazbxyzcxd" == ["aa","z","b","x","","y","","z","c","x","d"]
oneOf :: (Vector v a, Eq a) => v a -> Splitter v a
oneOf xs = toSplitList delim
    where delim = Delimiter (BV.singleton (`V.elem` xs))


-- | A splitting strategy that splits on the given list, when it is
-- encountered as an exact subsequence. For example:
--
-- >>> split (onSublist "xyz") "aazbxyzcxd" == ["aazb","xyz","cxd"]
-- Note that splitting on the empty list is a special case, which splits
-- just before every element of the list being split. For example:
--
-- >>> split (onSublist "") "abc" == ["","","a","","b","","c"]
-- >>> split (dropDelims . dropBlanks $ onSublist "") "abc" == ["a","b","c"]
--
-- However, if you want to break a list into singleton elements like this,
-- you are better off using chunksOf 1, or better yet, map (:[]).
onSublist :: (Vector v a, Eq a) => v a -> Splitter v a
onSublist xs = toSplitList delim
    where delim = Delimiter (V.map (==) . V.convert $ xs)

-- | A splitting strategy that splits on any elements that satisfy the given predicate. For example:
--
-- >>> split (whenElt (<0)) [2,4,-3,6,-9,1] == [[2,4],[-3],[6],[-9],[1]]
whenElt :: (Vector v a) => (a -> Bool) -> Splitter v a
whenElt p = toSplitList delim
    where delim = Delimiter (V.singleton p)


-- Strategy Transformers (how does haddock work?)

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
dropInitBlank (Text t : rst) | V.null t = rst
dropInitBlank rst                       = rst

dropFinalBlank :: Vector v a => SplitList v a -> SplitList v a
dropFinalBlank (rst : [Text t]) | V.null t = [rst]
dropFinalBlank (rs : rst)       = rs : dropFinalBlank rst
dropFinalBlank []               = []

dropInnerBlanks :: Vector v a => SplitList v a -> SplitList v a
dropInnerBlanks (Delim a : Text t : Delim b : rst) | V.null t  = Delim a : dropInnerBlanks (Delim b : rst)
                                                   | otherwise = Delim a : Text t : dropInnerBlanks (Delim b : rst)
dropInnerBlanks (rs : rst)                                     = rs : dropInnerBlanks rst
dropInnerBlanks []                                             = []


-- Derived combinators

-- Drop all blank chunks from the output, and condense consecutive
-- delimiters into one. 
-- Equivalent to dropInitBlank . dropFinalBlank . condense. For example:
--
-- >>> split (oneOf ":") "::b:::a" == ["",":","",":","b",":","",":","",":","a"]
-- >>> split (dropBlanks $ oneOf ":") "::b:::a" == ["::","b",":::","a"]
dropBlanks :: Vector v a => SplitList v a -> SplitList v a
dropBlanks = condense . dropInitBlank . dropFinalBlank . dropInnerBlanks

-- Make a strategy that splits a list into chunks that all start with the
-- given subsequence (except possibly the first). Equivalent to
-- dropInitBlank . keepDelimsL . onSublist. For example:
--
-- >>> split (startsWith "app") "applyapplicativeapplaudapproachapple" == ["apply","applicative","applaud","approach","apple"]
startsWith :: (Vector v a, Eq a) => v a -> Splitter v a
startsWith xs = dropInitBlank . keepDelimsL . onSublist xs

-- Make a strategy that splits a list into chunks that all start with one
-- of the given elements (except possibly the first). Equivalent to
-- dropInitBlank . keepDelimsL . oneOf. For example:
--
-- >>> split (startsWithOneOf ['A'..'Z']) "ACamelCaseIdentifier" == ["A","Camel","Case","Identifier"]
startsWithOneOf :: (Vector v a, Eq a) => v a -> Splitter v a
startsWithOneOf xs = dropInitBlank . keepDelimsL . oneOf xs

-- Make a strategy that splits a list into chunks that all end with the
-- given subsequence, except possibly the last. Equivalent to
-- dropFinalBlank . keepDelimsR . onSublist. For example:
--
-- >>> split (endsWith "ly") "happilyslowlygnarlylily" == ["happily","slowly","gnarly","lily"]
endsWith :: (Vector v a, Eq a) => v a -> Splitter v a
endsWith xs = dropFinalBlank . keepDelimsR . onSublist xs



-- Note: this function is not consistent with the Data.List.Split version
--
-- Make a strategy that splits a list into chunks that all end with one of
-- the given elements, except possibly the last. Equivalent to
-- dropFinalBlank . keepDelimsR . oneOf. For example:
--
-- >>> split (condense $ endsWithOneOf ".,?! ") "Hi, there!  How are you?" == ["Hi, ","there!  ","How ","are ","you?"]
endsWithOneOf :: (Vector v a, Eq a) => v a -> Splitter v a
endsWithOneOf xs = dropFinalBlank . keepDelimsR . oneOf xs
