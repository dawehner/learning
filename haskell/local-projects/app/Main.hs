{-# LANGUAGE NoImplicitPrelude #-} 
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import RIO
import Prelude (putStrLn)
import qualified System.Directory as Directory
import qualified RIO.List as List

type Folder = String

data ProjectType = Elm | Haskell | JavaScript | PHP | Elixir | Rust | Python
  deriving (Show, Eq)

main :: IO ()
main = do
  let folder = "/Users/dawehner/Projects/learning"
  runRIO folder $ do
    doGetFiles

recursivelyGetFolders :: FilePath -> IO [FilePath]
recursivelyGetFolders folder = do
  Directory.setCurrentDirectory folder
  files <- Directory.listDirectory folder
  let filteredFiles = List.deleteFirstsBy (==) files excludedFiles
  filesAbsolute <- mapM Directory.makeAbsolute filteredFiles
  folders <- filterM Directory.doesDirectoryExist filesAbsolute

  ((++) folders) <$> concat <$> mapM recursivelyGetFolders folders

excludedFiles :: [FilePath]
excludedFiles = ["node_modules", ".cache", "vendor", ".idea", ".unison", ".stack-work", "elm-stuff", ".git", "__pycache__"]

projectFilters :: [([Char] -> Bool, ProjectType)]
projectFilters = [
  (List.isInfixOf "elm.json", Elm)
  , (List.isInfixOf "elm-package.json", Elm)
  , (List.isInfixOf "package.yaml", Haskell)
  , (List.isInfixOf "stack.yaml", Haskell)
  , (List.isInfixOf "package.json", JavaScript)
  , (List.isInfixOf "composer.json", PHP)
  , (List.isInfixOf "mix.exs", Elixir)
  , (List.isInfixOf "Cargo.toml", Rust)
  , (List.isInfixOf "__pycache__", Python)
  ]

identifyProjectFolder :: FilePath -> IO (Maybe ProjectType)
identifyProjectFolder folder = do
  Directory.setCurrentDirectory folder
  files <- Directory.listDirectory folder
  filesAbsolute <- mapM Directory.makeAbsolute files
  realFiles <- filterM Directory.doesFileExist filesAbsolute
  let projectTypes = List.concat $ (\path -> map snd $ filter (\(cond, _) -> cond path) projectFilters) <$> realFiles

  return $ List.headMaybe projectTypes

doGetFiles :: RIO Folder ()
doGetFiles = do
  folder <- ask
  liftIO $ Directory.setCurrentDirectory folder
  folders <- liftIO $ recursivelyGetFolders folder
  types <- liftIO $ mapM (\path -> (\projectType -> (path, projectType)) <$> identifyProjectFolder path) folders
  liftIO $ putStrLn $ show $ filter (snd >>> (/=) Nothing) types
