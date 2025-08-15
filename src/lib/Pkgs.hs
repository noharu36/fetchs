module Pkgs
    ( getPackageCounts
    ) where

import Control.Exception (try, SomeException)
import Data.Maybe (catMaybes)
import Data.List (intercalate)
import SystemData (runCommand)

packageManagers :: [(String, String)]
packageManagers =
  [ ("Homebrew", "brew list | wc -l"),
    ("cargo",    "cargo install --list | grep ':' | wc -l")
    -- ("npm",      "npm list -g --depth=0 | grep -c ' -> '"),
    -- ("pip",      "pip list | tail -n +3 | wc -l"),
    -- ("pacman",   "pacman -Q | wc -l"),
    -- ("dpkg",     "dpkg -l | grep -c '^ii'")
  ]

countPkgs :: (String, String) -> IO (Maybe (String, String))
countPkgs (name, command) = do
    result <- try (concat <$> runCommand "sh" ["-c", command]) :: IO (Either SomeException String)
    case result of
        Left _ -> return Nothing -- 失敗したらNothing
        Right count -> do
            if count /= "" && count /= "0"
                then return $ Just (name, count)
            else return Nothing

getPackageCounts :: IO String
getPackageCounts = do
    results <- mapM countPkgs packageManagers

    let counts = catMaybes results

    let formatted = map (\(name, count) -> count ++ " (" ++ name ++ ")") counts

    return $ intercalate ", " formatted
