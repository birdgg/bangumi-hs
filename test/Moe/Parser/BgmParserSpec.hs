module Moe.Parser.BgmParserSpec where

import Moe.Parser.BgmParser
import RIO
import Test.Tasty
import Test.Tasty.HUnit

parseBangumiTitleTests :: TestTree
parseBangumiTitleTests =
    testGroup
        "parseBangumiTitle"
        [ testCase "parses title with season" $ do
            let result = parseBangumiTitle "怪兽8号第二季"
            case result of
                Left err -> assertFailure $ "Failed to parse: " <> show err
                Right BgmBangumi{title, season} -> do
                    title @?= "怪兽8号"
                    season @?= 2
        , testCase "parses title with season and space" $ do
            let result = parseBangumiTitle "怪兽8号 第三季"
            case result of
                Left err -> assertFailure $ "Failed to parse: " <> show err
                Right BgmBangumi{title, season} -> do
                    title @?= "怪兽8号"
                    season @?= 3
        , testCase "parses title without season" $ do
            let result = parseBangumiTitle "怪兽8号"
            case result of
                Left err -> assertFailure $ "Failed to parse: " <> show err
                Right BgmBangumi{title, season} -> do
                    title @?= "怪兽8号"
                    season @?= 1
        , testCase "parses title with first season" $ do
            let result = parseBangumiTitle "败犬女主太多了！第一季"
            case result of
                Left err -> assertFailure $ "Failed to parse: " <> show err
                Right BgmBangumi{title, season} -> do
                    title @?= "败犬女主太多了！"
                    season @?= 1
        , testCase "parses title with tenth season" $ do
            let result = parseBangumiTitle "长寿动漫第十季"
            case result of
                Left err -> assertFailure $ "Failed to parse: " <> show err
                Right BgmBangumi{title, season} -> do
                    title @?= "长寿动漫"
                    season @?= 10
        , testCase "handles extra whitespace" $ do
            let result = parseBangumiTitle "  测试动画   第二季  "
            case result of
                Left err -> assertFailure $ "Failed to parse: " <> show err
                Right BgmBangumi{title, season} -> do
                    title @?= "测试动画"
                    season @?= 2
        , testCase "parses English title with Chinese season" $ do
            let result = parseBangumiTitle "Attack on Titan第三季"
            case result of
                Left err -> assertFailure $ "Failed to parse: " <> show err
                Right BgmBangumi{title, season} -> do
                    title @?= "Attack on Titan"
                    season @?= 3
        ]

bgmParserTests :: TestTree
bgmParserTests =
    testGroup
        "Moe.Parser.BgmParser"
        [ parseBangumiTitleTests
        ]