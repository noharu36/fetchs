module MacOS
    ( getMacOSInfo
    ) where

import Data.List (isPrefixOf)
import Text.Read (readMaybe)
import Data.Char (isDigit)
import Text.Printf
import Control.Monad.State
import SystemData hiding (trim, initialState)
import Pkgs (getPackageCounts)

debugIO :: String -> IO String -> IO String
debugIO label fetchAction = do
    result <- fetchAction
    putStrLn $ "--- DEBUG [" ++ label ++ "]: " ++ result
    return result

getLineWithKey :: Foldable t => (a -> Bool) -> t a -> Maybe a
getLineWithKey p = foldr (\x acc -> if p x then Just x else acc) Nothing

extractValue :: String -> String
extractValue line = drop 1 val
    -- 警告が出ているが、ここは必ずマッチするパターンになっている
    where (_, ':':val) = break (== ':') line

getSystemProfiler :: String -> IO (Maybe String)
getSystemProfiler s = do
    output <- runCommand "system_profiler" ["SPHardwareDataType"]
    let modelName = getLineWithKey (s `isPrefixOf`) output
    return modelName

getMem :: IO String
getMem = do
    totalMemStr <- concat <$> runCommand "sysctl" ["-n", "hw.memsize"]
    pageSizeStr <- concat <$> runCommand "sysctl" ["-n", "vm.pagesize"]
    vmStat <- runCommand "vm_stat" []

    let maybeValues = do
            totalBytes <- readMaybe totalMemStr :: Maybe Integer
            pageSize   <- readMaybe pageSizeStr :: Maybe Integer

            pagesActive  <- vmValue ("Pages active:" `isPrefixOf`) vmStat
            pagesWired   <- vmValue ("Pages wired down:" `isPrefixOf`) vmStat
            pagesInactive <- vmValue ("Pages inactive:" `isPrefixOf`) vmStat

            return (totalBytes, pageSize, pagesActive, pagesWired, pagesInactive)

    case maybeValues of
        Just (totalBytes, pageSize, active, wired, inactive) -> do
            let usedBytes = (active + wired + inactive) * pageSize

            let bytesPerGB = 1024 * 1024 * 1024 :: Double
            let totalGB = fromIntegral totalBytes / bytesPerGB
            let usedGB  = fromIntegral usedBytes  / bytesPerGB

            let percentage = round (usedGB / totalGB * 100) :: Int

            let mem = printf "%.2fGB/%.2fGB (%d%%)" usedGB totalGB percentage :: String

            return mem

        Nothing -> do
            return "unknown"

    where vmValue key lineList = do
            line <- getLineWithKey key lineList
            let num = last (words line)
            readMaybe (filter isDigit num) :: Maybe Integer

getMacOSInfo :: StateT SysInfo IO ()
getMacOSInfo = do
    applyLens hostLens getHostName

    modelName <- liftIO $ getSystemProfiler "Model Name:"
    let name = maybe "Mac" extractValue modelName
    modelIdentifier <- liftIO $ getSystemProfiler "Model Identifier:"
    let identifier = maybe "unknown" extractValue modelIdentifier
    let machineName = name ++ "(" ++ identifier ++ ")"
    applyLens machineLens (pure machineName)

    applyLens kernelLens getKernel

    os_name <- liftIO $ concat <$>runCommand "sw_vers" ["-productName"]
    os_version <- liftIO $ concat <$> runCommand "sw_vers" ["-productVersion"]
    let osInfo = os_name ++ " " ++ os_version
    applyLens osLens (pure osInfo)

    let desktop = case read os_version :: Double of
            version' | version' > 15 -> "Liquid Glass"
                     | version' < 10 -> "Platinum"
                     | otherwise -> "Aqua"

    applyLens deLens (pure desktop)
    applyLens wmLens (pure "Quartz Compositor")

    applyLens pacLens getPackageCounts
    applyLens shLens getShellInfo
    applyLens termLens getTerminalInfo

    chip_raw <- liftIO $ getSystemProfiler "Chip:"
    let chip = maybe "unknown" extractValue chip_raw
    numberOfCores <- liftIO $ getSystemProfiler "Total Number of Cores:"
    let num = maybe "?" (take 1 .extractValue) numberOfCores
    let cpuInfo = chip ++ " (" ++ num ++ ")"
    applyLens cpuLens (pure cpuInfo)

    applyLens memLens getMem
    applyLens diskLens getStrage
