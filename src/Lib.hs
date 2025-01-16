module Lib (
    fullpath,
    getDirs,
    getFiles,
    ignore
) where

import System.Path.WildMatch
import Control.Monad
import System.Directory
import System.FilePath

fullpath :: FilePath -> FilePath -> FilePath
fullpath dir x = dir ++ "/" ++ x

getDirs :: FilePath -> IO [FilePath]
getDirs dir = do
  fs <- listDirectory dir
  let fs' = fmap (fullpath dir) fs
  filterM doesDirectoryExist fs'

getFiles :: FilePath -> IO [FilePath]
getFiles dir = do
  fs <- listDirectory dir
  filterM doesFileExist fs

ignore :: FilePath -> [String] -> Bool
ignore filename wildcards =
  let xs = map (\x -> wildCheckCase x filename) wildcards
  in or xs
