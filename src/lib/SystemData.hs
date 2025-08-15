{-# LANGUAGE RankNTypes #-}

module SystemData where

import System.Process (readProcess)
import System.FilePath (takeBaseName)
import System.Environment (getEnv)
import Control.Exception (try, SomeException)
import Control.Monad.State
import Control.Lens (Lens', lens, (?~))
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Text.Read (readMaybe)
import Text.Printf

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

runCommand :: FilePath -> [String] -> IO [String]
runCommand cmd args = map trim . lines <$> readProcess cmd args ""

data SysInfo = SI {
    hostName :: Maybe String,
    machine :: Maybe String,
    kernel :: Maybe String,
    os :: Maybe String,
    de :: Maybe String,
    wm :: Maybe String,
    packages :: Maybe String,
    shell :: Maybe String,
    terminal :: Maybe String,
    uptime :: Maybe String,
    cpu :: Maybe String,
    cpuLoad :: Maybe String,
    memory :: Maybe String,
    disk :: Maybe String,
    battery :: Maybe String
} deriving (Show)

initialState :: SysInfo
initialState = SI Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

hostLens :: Lens' SysInfo (Maybe String)
hostLens = lens hostName (\record newHost -> record { hostName = newHost })

machineLens :: Lens' SysInfo (Maybe String)
machineLens = lens machine (\record newMachine -> record { machine = newMachine })

kernelLens :: Lens' SysInfo (Maybe String)
kernelLens = lens kernel (\record newKernel -> record { kernel = newKernel })

osLens :: Lens' SysInfo (Maybe String)
osLens = lens os (\record newOS -> record { os = newOS })

deLens :: Lens' SysInfo (Maybe String)
deLens = lens de (\record newDE -> record { de = newDE })

wmLens :: Lens' SysInfo (Maybe String)
wmLens = lens wm (\record newWM -> record { wm = newWM })

pacLens :: Lens' SysInfo (Maybe String)
pacLens = lens packages (\record newPac -> record { packages = newPac })

shLens :: Lens' SysInfo (Maybe String)
shLens = lens shell (\record newSh -> record { shell = newSh })

termLens :: Lens' SysInfo (Maybe String)
termLens = lens terminal (\record newTerm -> record { terminal = newTerm })

upLens :: Lens' SysInfo (Maybe String)
upLens = lens uptime (\record newUp -> record { uptime = newUp })

cpuLens :: Lens' SysInfo (Maybe String)
cpuLens = lens cpu (\record newCPU -> record { cpu = newCPU })

cplLens :: Lens' SysInfo (Maybe String)
cplLens = lens cpuLoad (\record newCpl -> record { cpuLoad = newCpl })

memLens :: Lens' SysInfo (Maybe String)
memLens = lens memory (\record newMem -> record { memory = newMem })

diskLens :: Lens' SysInfo (Maybe String)
diskLens = lens disk (\record newDisk -> record { disk = newDisk })

batLens :: Lens' SysInfo (Maybe String)
batLens = lens battery (\record newBat -> record { battery = newBat })

applyLens :: Lens' SysInfo (Maybe String) -> IO String -> StateT SysInfo IO ()
applyLens fieldLens fetchAction = do
    info <- liftIO fetchAction
    modify (fieldLens ?~ info)

getHostName :: IO String
getHostName = do
    user <- runCommand "whoami" []
    host <- runCommand "hostname" []
    return $ concat user ++ "@" ++ concat host

getKernel :: IO String
getKernel = do
    concat <$> runCommand "uname" ["-r"]

getShellInfo :: IO String
getShellInfo = do
    result <- try (getEnv "SHELL") :: IO (Either SomeException String)

    case result of
        Left _ -> return "unknown"
        Right shellPath -> do
            let shellExecutable = takeBaseName shellPath
      
            versionResult <- try (concat <$> runCommand shellExecutable ["--version"]) :: IO (Either SomeException String)
      
            case versionResult of
                Right versionOutput -> return $ shellExecutable ++ " (" ++ versionOutput ++ ")"
                Left _ -> return shellExecutable

getTerminalInfo :: IO String
getTerminalInfo = do
    termProgramResult <- try (getEnv "TERM_PROGRAM") :: IO (Either SomeException String)

    case termProgramResult of
        Right termProgram -> return termProgram
        Left _ -> do
            termResult <- try (getEnv "TERM") :: IO (Either SomeException String)
            case termResult of
                Right term -> return term
                Left _ -> return "unknown"

getStrage :: IO String
getStrage = do
    lines_ <- runCommand "df" ["-P", "-k", "."]
    if length lines_ < 2 || length (words (lines_ !! 1)) < 4
        then return "N/A"
    else do
        let parts = words (lines_ !! 1)
        let maybeTotalKB = readMaybe (parts !! 1) :: Maybe Double
        let maybeUsedKB  = readMaybe (parts !! 2) :: Maybe Double
        let percent = parts !! 4

        case (maybeTotalKB, maybeUsedKB) of
            (Just totalKB, Just usedKB) ->
                let totalGB = totalKB / 1024 / 1024
                    usedGB = usedKB / 1024 / 1024
                in return $ printf "%.1fGB / %.1fGB (%s)" usedGB totalGB percent
            _ -> return "N/A"
