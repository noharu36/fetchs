module Ascii.Ascii
    ( getAsciiLines
    ) where

getAsciiLines :: String -> IO [String]
getAsciiLines key = do
    case key of
        "Mac" -> logoFormat . lines <$> readFile "./src/lib/Ascii/macos.txt"
        -- ディストリビューションごとに追加
        -- "Nix" -> undefined
        _ -> logoFormat . lines <$> readFile "./src/lib/Ascii/fhs.txt"

logoFormat :: [String] -> [String]
logoFormat = map (\line -> line ++ replicate (max 0 (40 - length line)) ' ')
