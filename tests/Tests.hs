import Data.Semigroup ((<>))
import Prelude ()
import Prelude.Compat
import Algebra.Lattice ((\/))
-- import Data.Either.Compat    (isRight)

import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck

import           Tarzan (RE)
import qualified Tarzan as RE

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [qcProps]

newtype RC = RC { getRC :: RE Char }

instance Show RC where
  show (RC r) = "/" ++ RE.prettyRe r ++ "/ " ++ show r

instance Arbitrary RC where
  arbitrary = RC <$> sized arbitrary'
    where
      arbitrary' :: Int -> Gen (RE Char)
      arbitrary' n
          | n <= 0     = frequency [ (4, return RE.eps)
                                   , (4, return RE.anychar)
                                   , (4, RE.char <$> (arbitrary `suchThat` ((<=0xffff) . fromEnum)))
                                   , (1, return RE.empty)
                                   , (1, return RE.anything)
                                   ]
          | otherwise  = oneof [ arbitrary' 0
                               , RE.append <$> arbitrary'' <*> arbitrary''
                               , RE.union <$> arbitrary'' <*> arbitrary''
                               , RE.kleene <$> arbitrary''
                               ]
        where
          arbitrary'' :: Gen (RE Char)
          arbitrary'' = arbitrary' (n `div` 3)

infix 4 `eq`

eq :: RE Char -> RE Char -> Property
eq x y  = counterexample (RE.prettyRe x ++ " `ne` " ++ RE.prettyRe y) (x `RE.eq` y)

{-
parsePrettied :: RC -> Property
parsePrettied (RC re) = property $ isRight $ RE.parseRe (RE.prettyRe re)

parsePrettyInvoluvitive :: RC -> Property
parsePrettyInvoluvitive (RC re) =
  let (Right re') = RE.parseRe (RE.prettyRe re)
   in Right re' === RE.parseRe (RE.prettyRe re')

parsePrettyEquivalent :: RC -> Property
parsePrettyEquivalent (RC re) =
  let (Right re') = RE.parseRe (RE.prettyRe re)
  in re' `eq` re
-}

appendAssoc :: RC -> RC -> RC -> Property
appendAssoc (RC a) (RC b) (RC c) = a <> (b <> c) `eq` (a <> b) <> c

unionAssoc :: RC -> RC -> RC -> Property
unionAssoc (RC a) (RC b) (RC c) = a \/ (b \/ c) `eq` (a \/ b) \/ c

unionComm :: RC -> RC -> Property
unionComm (RC a) (RC b) = a \/ b `eq` b \/ a

distrib1 :: RC -> RC -> RC -> Property
distrib1 (RC a) (RC b) (RC c) = a <> (b \/ c) `eq` (a <> b) \/ (a <> c)

distrib2 :: RC -> RC -> RC -> Property
distrib2 (RC a) (RC b) (RC c) = (b \/ c) <> a `eq` (b <> a) \/ (c <> a)

appendIdentLeft :: RC -> Property
appendIdentLeft (RC a) = RE.eps <> a `eq` a

appendIdentRight :: RC -> Property
appendIdentRight (RC a) = a <> RE.eps `eq` a

unionIdentLeft :: RC -> Property
unionIdentLeft (RC a) = RE.nothing \/ a `eq` a

unionIdentRight :: RC -> Property
unionIdentRight (RC a) = a \/ RE.nothing `eq` a

nothingAppendLeft :: RC -> Property
nothingAppendLeft (RC a) = RE.nothing <> a `eq` RE.nothing

nothingAppendRight :: RC -> Property
nothingAppendRight (RC a) = a <> RE.nothing `eq` RE.nothing

unionIdemp :: RC -> Property
unionIdemp (RC a) = a \/ a `eq` a

kleeneProps :: TestTree
kleeneProps = testGroup "Kleene algebra laws"
  [ testProperty "append is associative"  appendAssoc
  , testProperty "union is associtive"    unionAssoc
  , testProperty "union is commutative"   unionComm
  , testProperty "distributivity 1"       distrib1
  , testProperty "distributivity 2"       distrib2
  , testProperty "append left identity"   appendIdentLeft
  , testProperty "append right identity"  appendIdentRight
  , testProperty "union left identity"    unionIdentLeft
  , testProperty "union right identity"   unionIdentRight
  , testProperty "nothing left"           nothingAppendLeft
  , testProperty "nothing right"          nothingAppendRight
  , testProperty "union idempotent"       unionIdemp
  ]

qcProps :: TestTree
qcProps = testGroup "QuickCheck properties"
  [ testProperty "generates valids" $ RE.valid . getRC
  {-
  , testProperty "pretty . parse  succeeds"         parsePrettied
  , testProperty "pretty . parse 'involutive"       parsePrettyInvoluvitive
  , testProperty "pretty . parse equivalent to id"  parsePrettyEquivalent
  -}
  , kleeneProps
  ]
