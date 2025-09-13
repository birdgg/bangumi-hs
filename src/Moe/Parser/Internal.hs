module Moe.Parser.Internal where

import Data.Functor
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

-- | try to parse chinese season like `第三季`
chineseSeasonParser :: Parser Int
chineseSeasonParser = char '第' *> chineseDigit <* char '季'

chineseDigit :: Parser Int
chineseDigit =
  (char '一' $> 1)
    <|> (char '二' $> 2)
    <|> (char '三' $> 3)
    <|> (char '四' $> 4)
    <|> (char '五' $> 5)
    <|> (char '六' $> 6)
    <|> (char '七' $> 7)
    <|> (char '八' $> 8)
    <|> (char '九' $> 9)
