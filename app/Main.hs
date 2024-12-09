{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import CPUCollector (getCommandOutput, processListP)
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Parser (Parser (..))
import Parser qualified as P

-- Parse /proc/stat to extract CPU times

main :: IO ()
main = forever $ do
  content <- getCommandOutput
  putStrLn "Debug output:"
  putStrLn content
  case P.parse processListP content of
    Right processes -> mapM_ print processes
    Left err -> putStrLn $ "Error: " ++ err

  threadDelay 1000000 -- 1 second