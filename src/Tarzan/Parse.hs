module Tarzan.Parse (parseRe, unsafeParseRe) where

import Algebra.Lattice     ((\/))
import Control.Applicative (Alternative ((<|>)), many)
import Data.Char           (digitToInt)
import Prelude ()
import Prelude.Compat
import Text.Parsec
       (anyChar, char, eof, hexDigit, noneOf, oneOf, option, optionMaybe,
       optional, parse, sepBy1, string, try)
import Text.Parsec.String  (Parser)

import           Tarzan (RE)
import qualified Tarzan as RE

reChar :: Parser (RE Char)
reChar = reCharGroup <|> reAnyChar <|> reTopSingleChar
  where
    reCharGroup               = (\_ a b _ -> RE.chars a b) <$> char '[' <*> reCharGroupPos <*> many reCharPair <*> char ']'
    reCharGroupPos            = option True (const False <$> char '^')
    reCharPair                = reCharPairCon <$> reSingleChar <*> optionMaybe (char '-' *> reSingleChar)
    reCharPairCon c Nothing   = (c, c)
    reCharPairCon a (Just b)  = (a, b)
    reSingleChar              = escapedChar <|> noneOf "]\\"
    reTopSingleChar           = RE.char <$> (escapedChar <|>  noneOf "$[]*+?()|.\\/")
    reAnyChar                 = const RE.dot <$> char '.'

reParens :: Parser (RE Char)
reParens = char '(' *> nonmatch  *> parser <* char ')'
  where nonmatch = optional $ try $ string "?:"

reTerm :: Parser (RE Char)
reTerm = (reParens <|> reChar)

rePostfix :: Parser (RE Char)
rePostfix = f <$> reTerm <*> optionMaybe (oneOf "*+?") <*> optional (char '?')
  where
    f x Nothing    _  = x
    f x (Just '*') _  = RE.kleene x
    f x (Just '?') _  = x \/ RE.eps
    f x (Just '+') _  = RE.kplus x
    f _ (Just x)   _  = error $ "rePostfix: " ++ [x]

append :: Parser (RE Char)
append = foldl RE.append RE.eps <$> many rePostfix

reGroup :: Parser (RE Char)
reGroup = RE.unions <$> sepBy1 append (char '|')

parser :: Parser (RE Char)
parser = reGroup

parseRe :: String -> Either String (RE Char)
parseRe input = case parse (parser <* eof) "" input of
    Right res -> Right res
    Left err  -> Left $ show err

unsafeParseRe :: String -> RE Char
unsafeParseRe = either error id . parseRe

escapedChar :: Parser Char
escapedChar = char '\\' *> (x <|> u <|> n <|> r <|> t <|> anyChar)
  where
    n   = char 'n' *> return '\n'
    r   = char 'r' *> return '\r'
    t   = char 't' *> return '\t'
    x   = x' <$> char 'x' <*> hexDigit <*> hexDigit
    x' _ a b = toEnum $ 0x10 * (digitToInt a) + (digitToInt b)
    u   = u' <$> char 'u' <*> hexDigit <*> hexDigit <*> hexDigit <*> hexDigit
    u' _ a b c d = toEnum $ 0x1000 * (digitToInt a) + 0x100 * (digitToInt b) + 0x10 * (digitToInt c) + (digitToInt d)
