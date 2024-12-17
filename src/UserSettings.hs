{-# LANGUAGE ImportQualifiedPost #-}

module UserSettings
  ( SortColumn (..),
    GraphOptions (..),
    PaneOrientation (..),
    UserSettings (..),
    saveUserSettings,
    loadUserSettings,
    userSettingsP,
  )
where

import Control.Applicative
import Parser
import Parser qualified as P

data SortColumn
  = SortPID
  | SortCommand
  | SortCPU
  | SortMem
  | SortVSZ
  | SortRSS
  | SortTTY
  | SortStat
  | SortStarted
  | SortTime
  | SortUser
  deriving (Show, Eq, Read)

data GraphOptions
  = CpuPct
  | MemPct
  deriving (Show, Eq, Read)

data PaneOrientation
  = LeftRight
  | RightLeft
  | TopBottom
  | BottomTop
  deriving (Show, Eq, Read)

data UserSettings = UserSettings
  { savedTableSort :: SortColumn,
    savedSelectedGraph :: GraphOptions,
    savedOrientation :: PaneOrientation
  }
  deriving (Show, Eq)

-- Parser for key-value pairs like "key=value"
keyValueP :: String -> Parser String
keyValueP key = P.string key *> P.char '=' *> many (P.satisfy (/= '\n'))

-- Parse a specific setting using Read typeclass
readSettingP :: (Read a) => String -> Parser a
readSettingP key =
  ( \val -> case reads val of
      [(x, "")] -> x
      _ -> error "Failed to parse setting"
  )
    <$> keyValueP key

-- Parser for UserSettings using applicative style
userSettingsP :: Parser UserSettings
userSettingsP =
  UserSettings
    <$> readSettingP "tableSort"
    <* many (P.char '\n')
    <*> readSettingP "selectedGraph"
    <* many (P.char '\n')
    <*> readSettingP "orientation"

saveUserSettings :: UserSettings -> FilePath -> IO ()
saveUserSettings settings path = do
  let contents =
        unlines
          [ "tableSort=" ++ show (savedTableSort settings),
            "selectedGraph=" ++ show (savedSelectedGraph settings),
            "orientation=" ++ show (savedOrientation settings)
          ]
  writeFile path contents

loadUserSettings :: FilePath -> IO (Maybe UserSettings)
loadUserSettings path = do
  contents <- readFile path
  return $ case parse userSettingsP contents of
    Left _ -> Nothing
    Right settings -> Just settings
