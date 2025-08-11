module Main (main) where

import Moe.Parser.BgmParserSpec
import Moe.Parser.Internal.UtilSpec
import RIO
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "Bangumi Parser Tests"
        [ utilTests
        , bgmParserTests
        ]