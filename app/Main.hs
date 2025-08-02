module Main (main) where

import qualified Bangumi (someFunc)

main :: IO ()
main = do
    putStrLn "Hello, Haskell!"
    Bangumi.someFunc
