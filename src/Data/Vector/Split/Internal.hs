


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


-- Convenience functions

-- | Split on the given sublist. Equivalent to split . dropDelims . onSublist. For example:
--
-- >>> splitOn ".." "a..b...c....d.." == ["a","b",".c","","d",""]
-- In some parsing combinator frameworks this is also known as sepBy.
--
-- Note that this is the right inverse of the intercalate function from Data.List, that is,
--
-- >>> intercalate x . splitOn x === id
-- splitOn x . intercalate x is the identity on certain lists, but it is
-- tricky to state the precise conditions under which this holds. (For
-- example, it is not enough to say that x does not occur in any elements
-- of the input list. Working out why is left as an exercise for the
-- reader.)
splitOn :: (Vector v a, Eq a) => v a -> v a -> [v a]
splitOn xs = split (dropDelims . onSublist xs)

-- | Split on any of the given elements. Equivalent to split . dropDelims . oneOf. For example:
--
-- >>> splitOneOf ";.," "foo,bar;baz.glurk" == ["foo","bar","baz","glurk"]
splitOneOf :: (Vector v a, Eq a) => v a -> v a -> [v a]
splitOneOf xs = split (dropDelims . oneOf xs)

-- | Split on elements satisfying the given predicate. Equivalent to split . dropDelims . whenElt. For example:
--
-- >>> splitWhen (<0) [1,3,-4,5,7,-9,0,2] == [[1,3],[5,7],[0,2]]
splitWhen :: (Vector v a) => (a -> Bool) -> v a -> [v a]
splitWhen p = split (dropDelims . whenElt p)

-- | Split into chunks terminated by the given subsequence. Equivalent to split . dropFinalBlank . dropDelims . onSublist. For example:
-- 
-- >>> endBy ";" "foo;bar;baz;" == ["foo","bar","baz"]
-- Note also that the lines function from Data.List is equivalent to endBy "\n".
endBy :: (Vector v a, Eq a) => v a -> v a -> [v a]
endBy xs = split (dropFinalBlank . dropDelims . onSublist xs)
-- Basic Strategies

-- | A splitting strategy that splits on any one of the given elements. For example:
-- >>> split (oneOf "xyz") "aazbxyzcxd" == ["aa","z","b","x","","y","","z","c","x","d"]
oneOf :: (Vector v a, Eq a) => v a -> Splitter v a
oneOf xs = toSplitList delim
    where delim = Delimiter (BV.singleton (`V.elem` xs))


-- | Split into chunks terminated by one of the given elements. Equivalent to split . dropFinalBlank . dropDelims . oneOf. For example:
--
-- >>> endByOneOf ";," "foo;bar,baz;" == ["foo","bar","baz"]
endByOneOf :: (Vector v a, Eq a) => v a -> v a -> [v a]
endByOneOf xs = split (dropFinalBlank . dropDelims . oneOf xs)


-- | Split into "words", with word boundaries indicated by the given predicate. Satisfies words === wordsBy isSpace; equivalent to split . dropBlanks . dropDelims . whenElt. For example:
--
-- >>> wordsBy (=='x') "dogxxxcatxbirdxx" == ["dog","cat","bird"]
wordsBy :: Vector v a => (a -> Bool) -> v a -> [v a]
wordsBy p = split (dropBlanks . dropDelims . whenElt p)


-- | Split into "lines", with line boundaries indicated by the given predicate. Satisfies lines === linesBy (=='\n'); equivalent to split . dropFinalBlank . dropDelims . whenElt. For example:
--
-- >>> linesBy (=='x') "dogxxxcatxbirdxx" == ["dog","","","cat","bird",""]
linesBy :: Vector v a => (a -> Bool) -> v a -> [v a]
linesBy p = split (dropFinalBlank . dropDelims . whenElt p)


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
