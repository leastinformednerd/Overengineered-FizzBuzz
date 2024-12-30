module Main where

import System.Environment (getArgs)
import System.FilePath
import System.IO

import System.IO.Strict as SIO

import GHC.List

import Compiler
import Parser

processFile :: String -> IO (AbstractSyntaxList)
processFile path = withFile path ReadMode $ \h -> parseProgram `fmap` SIO.hGetContents h

compileToFile :: String -> AbstractSyntaxList -> IO ()
compileToFile path asl = withFile path WriteMode $ (\h -> hPutStr h $ compile asl)

main :: IO ()
main = do
  args <- fmap uncons getArgs
  file <- case args of 
    Just (file, _) -> return $ Just file :: IO (Maybe String)
    Nothing -> putStrLn "No FilePath Provided" >> return Nothing
  parsed <- case file of
    Just file -> fmap Just $ processFile file
    Nothing -> return Nothing

  case (file, parsed) of 
    (Just file, Just parsed) -> putStrLn "Parsed. Compiling" >>
      compileToFile (file -<.> ".c") parsed >>
      putStrLn "Done"
    _ -> putStrLn "Failed to parse"
