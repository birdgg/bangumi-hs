module Moe.Parser.BgmParser where

import Moe.Parser.Internal.Util (parseChineseSeason)
import RIO
import RIO.Text qualified as T
import Text.Megaparsec hiding (try)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char (char)

data BgmBangumi = BgmBangumi
    { title :: T.Text
    , season :: Int
    }
    deriving (Show, Eq)

type Parser = Parsec Void T.Text

parseBangumiTitle :: T.Text -> Either (ParseErrorBundle T.Text Void) BgmBangumi
parseBangumiTitle = parse bangumiTitleParser ""

bangumiTitleParser :: Parser BgmBangumi
bangumiTitleParser = do
    _ <- Text.Megaparsec.many (char ' ')
    MP.try parseWithSeason <|> parseWithoutSeason
  where
    parseWithSeason = do
        titleText <- takeWhileP Nothing (/= '第')
        seasonNum <- parseChineseSeason
        _ <- Text.Megaparsec.many (char ' ')
        eof
        return $ BgmBangumi (T.strip titleText) seasonNum

    parseWithoutSeason = do
        titleText <- takeWhileP Nothing (const True)
        eof
        return $ BgmBangumi (T.strip titleText) 1
