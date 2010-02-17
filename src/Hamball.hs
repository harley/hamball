module Main where

import System( getArgs )

main :: IO ()
main = do
    args <- getArgs
    print $ "haha" ++ show args

