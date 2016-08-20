{-# LANGUAGE GADTs #-}
module Tarzan (
    -- * Regular expression type
    RE (..),
    -- * Character class
    Character,
    -- * Terminals
    empty,
    nothing,
    anything,
    eps,
    anychar,
    char,
    chars,
    dot,
    string,
    -- * Concatenation
    append,
    (<>),
    -- * Alternation
    union,
    unions,
    -- * Kleene star & plus
    kleene,
    kstar,
    kplus,
    -- * Matching
    -- | Matching implementation is based on ideas in
    -- <http://dl.acm.org/citation.cfm?id=1520288 Janusz A. Brzozowski: Derivatives of Regular Expressions> and
    -- <http://dl.acm.org/citation.cfm?id=1520288 Scott Owens, John Reppy, Aaron Turon: Regular-expression derivatives re-examined>
    nullable,
    derivate,
    matches,
    (~=),
    -- * Equality and partial order
    eq,
    ne,
    leq,
    -- * Pretty printing
    prettyRe,
    -- * Utility
    leadingCSets,
    -- * Internal
    valid,
    ) where

-- http://r6.ca/blog/20110808T035622Z.html

import Prelude ()
import Prelude.Compat hiding (all, any, negate)

import           Data.Set (Set)
import qualified Data.Set as Set

import           Data.RangeSet.List (RSet)
import qualified Data.RangeSet.List as RSet

import Algebra.Lattice
       (BoundedJoinSemiLattice (..), JoinSemiLattice (..))
import Data.Either               (partitionEithers)
import Data.Foldable             (all, any)
import Data.List                 (foldl', intercalate)
import Data.Semigroup            (Semigroup (..))
import Data.String               (IsString (..))
import Text.Printf               (printf)

-- | Regular expression
--
-- Constructors are exposed, but you should use
-- smart constructors in this module to construct 'RE'.
--
-- The 'Eq' and 'Ord' instances are structural. See 'eq', 'ne' if you need
-- regular-expression equivalence.
data RE a
    = REChars (RSet a)      -- ^ Single character
    | REAppend [RE a]       -- ^ Concatenation
    | REUnion (Set (RE a))  -- ^ Union
    | REKleene (RE a)       -- ^ Kleene star
  deriving (Eq, Ord, Show)

-- | Check 'RE' invariants.
--
-- * 'REAppend' has at least two elements
-- * 'REUnion'  has at least two elements
valid :: RE a -> Bool
valid (REChars _)     = True
valid (REAppend rs)   = length rs >= 2 && all valid rs
valid (REUnion rs)    = length rs >= 2 && all valid rs
valid (REKleene r)    = valid r

-- | Character is finite enumeration.
class (Ord a, Enum a, Bounded a) => Character a
instance Character Char

-- | '<>' is 'append'
instance  Character a => Semigroup (RE a) where
    (<>) = append

instance Character a => Monoid (RE a) where
    mempty =  eps
    mappend = append
    mconcat = appends

instance Character a => JoinSemiLattice (RE a) where
    (\/) = union

instance Character a => BoundedJoinSemiLattice (RE a) where
    bottom = empty

instance a ~ Char => IsString (RE a) where
    fromString = string

-- | Never matches.
empty :: RE a
empty = REChars RSet.empty

-- | Synonym to 'empty'.
nothing :: RE a
nothing = empty

-- | Matches everything.
anything :: Character a => RE a
anything = REKleene anychar

-- | Empty string.
eps :: RE a
eps = REKleene nothing

-- | Single character.
char :: a -> RE a
char = REChars . RSet.singleton

-- | Union of character ranges.
-- First 'Bool' argument whether the set should be inverted.
chars :: Character a => Bool -> [(a,a)] -> RE a
chars pos cs = REChars . con $ s
  where
    con | pos       = id
        | otherwise = RSet.complement
    s               = mconcat $ map RSet.singletonRange cs

dotSet :: RSet Char
dotSet = RSet.complement $ RSet.singleton '\n'

-- | Every character, except @'\n'@.
dot :: RE Char
dot = REChars dotSet

string :: Ord a => [a] -> RE a
string = foldr append eps . map char

anychar :: Bounded a => RE a
anychar = REChars RSet.full

appends :: Ord a => [RE a] -> RE a
appends rs
    | elem empty rs' = empty
    | otherwise      = case rs' of
        []   -> eps
        [r]  -> r
        _    -> REAppend rs'
  where
    rs' = filter (/= eps) $ concatMap extractAppends rs
    extractAppends (REAppend rs'') = rs''
    extractAppends r               = [r]

append :: Ord a => RE a-> RE a -> RE a
append a b = appends [a, b]

union :: Character a => RE a -> RE a -> RE a
union a b = unions [a, b]

extractCharacterSets :: RE a -> Either (RSet a) (RE a)
extractCharacterSets (REChars c)  = Left c
extractCharacterSets r            = Right r

unions :: Character a => [RE a] -> RE a
unions = unions' . split . flatten . sortUniq
  where
    flatten = concatMap extract
    extract (REUnion xs) = Set.toList xs
    extract x            = [x]
    split rs  = case partitionEithers $ map extractCharacterSets rs of
        (cs, rs') -> if cs' == empty then rs'' else cs' : rs''
           where
            cs'   = REChars (mconcat cs)
            rs''  = rs'
    unions' []   = empty
    unions' [r]  = r
    unions' rs   = REUnion . Set.fromList $ rs

kleene :: Character a => RE a -> RE a
kleene r
    | r == empty       = eps
    | r == anything    = anything -- this and following are special cases of (REKleene r) case
    | r == eps         = eps
kleene (REKleene r)  = REKleene r
kleene r             = REKleene r

-- | Alias to 'kleene'.
kstar :: Character a => RE a -> RE a
kstar = kleene

-- | Kleene plus, @a+ = aa*@.
kplus :: Character a => RE a -> RE a
kplus r = r <> kstar r

-------------------------------------------------------------------------------
-- Matching
-------------------------------------------------------------------------------

-- | Matches empty string
nullable :: RE a -> Bool
nullable (REChars _)     = False
nullable (REAppend rs)   = all nullable rs
nullable (REUnion rs)    = any nullable rs
nullable (REKleene _)    = True

derivateAppend :: Character a => a -> [RE a] -> RE a
derivateAppend _ []      = nothing
derivateAppend c [r]     = derivate c r
derivateAppend c (r:rs)
    | nullable r           = r' <> appends rs \/ rs'
    | otherwise            = r' <> appends rs
  where
    r'  = derivate c r
    rs' = derivateAppend c rs

derivate :: Character a => a -> RE a -> RE a
derivate c (REChars cs)
  | c `RSet.member` cs      = eps
  | otherwise               = nothing
derivate c (REUnion rs)     = unions (map (derivate c) $ Set.toList rs)
derivate c (REAppend rs)    = derivateAppend c rs
derivate c rs@(REKleene r)  = derivate c r <> rs

matches :: Character a => RE a -> [a] -> Bool
matches r = nullable . foldl' (flip derivate) r

infix 4 ~=

-- | Flipped infix version of 'matches'.
(~=) :: Character a => [a] -> RE a -> Bool
(~=) = flip matches

-- character sets

wedge :: Character a => [RSet a] -> [RSet a] -> [RSet a]
wedge as bs = sortUniq $ filter (not . RSet.null) [ a `RSet.intersection` b | a <- as, b <- bs ]

leadingCSetsAppend :: Character a => [RE a] -> [RSet a]
leadingCSetsAppend []      = [RSet.full]
leadingCSetsAppend [r]     = leadingCSets r
leadingCSetsAppend (r:rs)
  | nullable r             = leadingCSets r `wedge` leadingCSetsAppend rs
  | otherwise              = leadingCSets r

leadingCSets :: Character a => RE a -> [RSet a]
leadingCSets (REChars r)    = [r, RSet.complement r]
leadingCSets (REUnion rs)   = foldl' wedge [RSet.full] $ map leadingCSets $ Set.toList rs
leadingCSets (REAppend rs)  = leadingCSetsAppend rs
leadingCSets (REKleene r)   = leadingCSets r

-- equality

derivatePair :: Character a => a -> (RE a, RE a) -> (RE a, RE a)
derivatePair c (a, b) = (derivate c a, derivate c b)

infix 4 `eq`
infix 4 `ne`

-- | Equivalence of regular expressions.
eq :: Character a => RE a -> RE a -> Bool
eq r s = all nullp $ bfs nextPairs (r, s)
  where
    nullp (a, b)  = nullable a == nullable b

nextPairs ::  Character a => (RE a, RE a) -> [(RE a, RE a)]
nextPairs p@(a, b) = map (flip derivatePair p) chars'
  where
    csets           = leadingCSets a `wedge` leadingCSets b
    chars'          = map RSet.findMin csets

-- | Negation of 'eq'.
ne :: Character a => RE a -> RE a -> Bool
ne r s = not (eq r s)

-- | Partial order of regular-expressions.
--
-- > a `leq` b = a \/ b `eq` b
leq :: Character a => RE a -> RE a -> Bool
a `leq` b = a \/ b `eq` b

bfs :: Ord a => (a -> [a]) -> a -> [a]
bfs f x = go (Set.singleton x) (Set.singleton x)
  where
    go visited curr
        | Set.null curr = []
        | otherwise     = Set.toList curr ++ go visited' next''
      where
        next     = concatMap f (Set.toList curr)
        next'    = Set.fromList next
        visited' = Set.union next' visited
        next''   = Set.difference next' visited

-------------------------------------------------------------------------------
-- Pretty
-------------------------------------------------------------------------------

escapeChar :: Char -> String
escapeChar '\n'   = "\\n"
escapeChar '\t'   = "\\t"
escapeChar '\r'   = "\\r"
escapeChar c
    | ord > 0xffff  = error "escapeChar: out of BMP"
    | ord < 0x20    = '\\' : 'x' : printf "%02x" ord
    | ord >= 0x80   = '\\' : 'u' : printf "%04x" ord
    | c `elem` e    = '\\' : [c]
    | otherwise     = [c]
  where
    e    = "^$?+[]*()|\\/-."
    ord  = fromEnum c

prettyRSetChar :: RSet Char -> String
prettyRSetChar r
    | RSet.null r   = []
    | dotSet == r   = "."
    | s == 1        = escapeChar $ head $ RSet.toList r
    | s < m - s     = prettyRSetChar' r
    | otherwise     = prettyRSetChar'' (RSet.complement r)
  where
    s    = RSet.size r
    m    = fromEnum (maxBound :: Char) - fromEnum (minBound :: Char)

prettyRSetChar' :: RSet Char -> String
prettyRSetChar' r        = "[" ++ concatMap p l ++ "]"
  where
    l                = RSet.toRangeList r
    p (a, b)
        | a == b     = escapeChar a
        | otherwise  = escapeChar a ++ "-" ++ escapeChar b

prettyRSetChar'' :: RSet Char -> String
prettyRSetChar'' r = "[^" ++ concatMap p l ++ "]"
  where
    l              = RSet.toRangeList r
    p (a, b)
        | a == b     = escapeChar a
        | otherwise  = escapeChar a ++ "-" ++ escapeChar b

prettyRe :: RE Char -> String
prettyRe r | r == eps       = ""
prettyRe r | r == nothing   = "[]"
prettyRe r | r == anything  = "[^]*"
prettyRe (REChars cs)       = prettyRSetChar cs
prettyRe (REAppend rs)      = concatMap prettyRe rs
prettyRe (REUnion rs)       = "(?:" ++ intercalate "|" rs' ++ ")" ++ opt
  where
    rs' = map prettyRe $ Set.toList $ Set.delete eps rs
    opt | eps `Set.member` rs = "?"
        | otherwise           = ""
prettyRe (REKleene r)       = case prettyRe r of
    [c]  -> c : "*"
    str  -> "(" ++ str ++ ")*"

-- Utilities

sortUniq :: Ord a => [a] -> [a]
sortUniq = Set.toList . Set.fromList
