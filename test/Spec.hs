module Main (main) where

import WaiRateLimitPostgres (projectName)


main :: IO ()
main = putStrLn ("Tests for " ++ projectName)
