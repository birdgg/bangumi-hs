module Moe.Parser.Internal.UtilSpec where

import Moe.Parser.Internal.Util
import RIO
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec

parseChineseSeasonTests :: TestTree
parseChineseSeasonTests =
    testGroup
        "parseChineseSeason"
        [ testCase "parses 第一季" $ do
            let result = parse parseChineseSeason "" "第一季"
            case result of
                Left err -> assertFailure $ "Failed to parse: " <> show err
                Right season -> season @?= 1
        , testCase "parses 第二季" $ do
            let result = parse parseChineseSeason "" "第二季"
            case result of
                Left err -> assertFailure $ "Failed to parse: " <> show err
                Right season -> season @?= 2
        , testCase "parses 第三季" $ do
            let result = parse parseChineseSeason "" "第三季"
            case result of
                Left err -> assertFailure $ "Failed to parse: " <> show err
                Right season -> season @?= 3
        , testCase "parses 第十季" $ do
            let result = parse parseChineseSeason "" "第十季"
            case result of
                Left err -> assertFailure $ "Failed to parse: " <> show err
                Right season -> season @?= 10
        , testCase "parses with leading space" $ do
            let result = parse parseChineseSeason "" " 第二季"
            case result of
                Left err -> assertFailure $ "Failed to parse: " <> show err
                Right season -> season @?= 2
        , testCase "fails on invalid input" $ do
            let result = parse parseChineseSeason "" "season 2"
            case result of
                Left _ -> return ()
                Right _ -> assertFailure "Should have failed to parse"
        , testCase "fails on missing 季" $ do
            let result = parse parseChineseSeason "" "第二"
            case result of
                Left _ -> return ()
                Right _ -> assertFailure "Should have failed to parse"
        ]

parseChineseNumberTests :: TestTree
parseChineseNumberTests =
    testGroup
        "parseChineseNumber"
        [ testCase "parses 一" $ do
            let result = parse parseChineseNumber "" "一"
            case result of
                Left err -> assertFailure $ "Failed to parse: " <> show err
                Right num -> num @?= 1
        , testCase "parses 二" $ do
            let result = parse parseChineseNumber "" "二"
            case result of
                Left err -> assertFailure $ "Failed to parse: " <> show err
                Right num -> num @?= 2
        , testCase "parses 十" $ do
            let result = parse parseChineseNumber "" "十"
            case result of
                Left err -> assertFailure $ "Failed to parse: " <> show err
                Right num -> num @?= 10
        , testCase "fails on arabic number" $ do
            let result = parse parseChineseNumber "" "1"
            case result of
                Left _ -> return ()
                Right _ -> assertFailure "Should have failed to parse"
        , testCase "fails on non-chinese character" $ do
            let result = parse parseChineseNumber "" "a"
            case result of
                Left _ -> return ()
                Right _ -> assertFailure "Should have failed to parse"
        ]

parseNumberTests :: TestTree
parseNumberTests =
    testGroup
        "parseNumber"
        [ testCase "parses single digit" $ do
            let result = parse parseNumber "" "1"
            case result of
                Left err -> assertFailure $ "Failed to parse: " <> show err
                Right num -> num @?= 1
        , testCase "parses multi-digit number" $ do
            let result = parse parseNumber "" "123"
            case result of
                Left err -> assertFailure $ "Failed to parse: " <> show err
                Right num -> num @?= 123
        , testCase "fails on chinese character" $ do
            let result = parse parseNumber "" "一"
            case result of
                Left _ -> return ()
                Right _ -> assertFailure "Should have failed to parse"
        , testCase "fails on letter" $ do
            let result = parse parseNumber "" "a"
            case result of
                Left _ -> return ()
                Right _ -> assertFailure "Should have failed to parse"
        ]

utilTests :: TestTree
utilTests =
    testGroup
        "Moe.Parser.Internal.Util"
        [ parseChineseSeasonTests
        , parseChineseNumberTests
        , parseNumberTests
        ]