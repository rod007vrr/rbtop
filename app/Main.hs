{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import CPUCollector (getCommandOutput, processListP)
import Parser (Parser (..))
import Parser qualified as P

-- Parse /proc/stat to extract CPU times

main :: IO ()
main = do
  content <- getCommandOutput
  putStrLn "Debug output:" -- Add this debug line
  putStrLn content
  case P.parse processListP content of
    Right processes -> mapM_ print processes -- Print each process
    Left err -> putStrLn $ "Error: " ++ err