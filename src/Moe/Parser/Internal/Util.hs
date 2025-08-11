module Moe.Parser.Internal.Util where

import RIO
import RIO.Text qualified as T
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void T.Text

chineseToInt :: T.Text -> Int
chineseToInt text = case T.unpack text of
    "一" -> 1
    "二" -> 2  
    "三" -> 3
    "四" -> 4
    "五" -> 5
    "六" -> 6
    "七" -> 7
    "八" -> 8
    "九" -> 9
    "十" -> 10
    _ -> 0

parseChineseNumber :: Parser Int
parseChineseNumber = do
      numStr <- Text.Megaparsec.some (oneOf ['一', '二', '三', '四', '五', '六', '七', '八', '九', '十'])
      return $ chineseToInt (T.pack numStr)

parseNumber :: Parser Int
parseNumber = do
  digits <- Text.Megaparsec.some digitChar
  case readMaybe digits of
    Just n -> return n
    Nothing -> fail "Invalid number"

parseChineseSeason :: Parser Int
parseChineseSeason = do
  _ <- optional space  
  _ <- string "第"
  num <- parseChineseNumber
  _ <- string "季"
  return num