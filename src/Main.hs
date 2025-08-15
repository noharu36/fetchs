module Main (main) where

import Lib

main :: IO ()
main = do
    sysInfo <- getSysInfo
    mapM_ putStrLn sysInfo
