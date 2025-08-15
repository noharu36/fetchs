module Lib
    ( getSysInfo
    ) where

import qualified System.Info (os)
import Data.List (isInfixOf)
import Control.Monad.State
import SystemData hiding (trim, runCommand, applyLens, getHostName, getKernel, getShellInfo, getTerminalInfo, getStrage)
import MacOS (getMacOSInfo)
import Linux
import Ascii.Ascii (getAsciiLines)

getSysInfo :: IO [String]
getSysInfo = do
  case System.Info.os of
    "linux" -> undefined
    "darwin" -> execStateT getMacOSInfo initialState >>= formatFetchData
    "mingw32" -> return undefined
    _ -> return undefined

formatFetchData :: SysInfo -> IO [String]
formatFetchData info = do
    let maybeOS = os info
        in case maybeOS of
            Just osName
                | "mac" `isInfixOf` osName -> do
                    let infoList = infoToList info
                    ascii <- getAsciiLines "Mac"
                    return $ zipString ascii infoList
                | "Linux" `isInfixOf` osName -> undefined
                | otherwise -> return [osName]
            Nothing -> do
                let infoList = infoToList info
                ascii <- getAsciiLines "unknown"
                return $ zipString ascii infoList
        

zipString :: [String] -> [String] -> [String]
zipString [] [] = []
zipString (x:xs) [] = x : zipString xs []
zipString [] (y:ys) = y : zipString [] ys
zipString (x:xs) (y:ys) = (x ++ y) : zipString xs ys
    

infoToList :: SysInfo -> [String]
infoToList info =
  [ label ++ ": " ++ val | (label, Just val) <- allPairs ]
  where
    allPairs :: [(String, Maybe String)]
    allPairs =
      [ ("HostName", hostName info),
        ("Machine", machine info),
        ("Kernel", kernel info),
        ("OS", os info),
        ("DE", de info),
        ("WM", wm info),
        ("Packages", packages info),
        ("Shell", shell info),
        ("Terminal", terminal info),
        ("Uptime", uptime info),
        ("CPU", cpu info),
        ("cpuLoad", cpuLoad info),
        ("Memory", memory info),
        ("Disk", disk info),
        ("Battery", battery info)
      ]
    
