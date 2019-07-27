module Main where

import System.Path.WildMatch
import Control.Monad
import Control.Applicative
import System.Directory
import System.FilePath

fullpath :: FilePath -> FilePath ->FilePath
fullpath dir x = dir ++ "/" ++ x

checkProd :: IO Bool
checkProd  = do
  dirs <- getDirs "../"
  let xs = filter (=="..//prod") dirs 
  return  (not $ null xs)

getDirs :: FilePath -> IO [FilePath]
getDirs dir = do
  fs <- listDirectory dir
  let fs' = fmap (fullpath dir) fs
  dirs <- filterM doesDirectoryExist fs'
  return dirs

getFiles :: FilePath -> IO [FilePath]
getFiles dir = do
  fs <- listDirectory dir
  files <- filterM doesFileExist fs
  return files

ignore :: FilePath -> [String] -> Bool
ignore filename wildcards = 
  let xs = map (\x->wildCheckCase x filename) wildcards
  in or xs

copyFiles :: FilePath -> FilePath -> [String] -> IO()
copyFiles source dest ws = do
  fs <- getFiles source
  let fs' = filter (\x -> not $ ignore x ws) fs
  forM fs' (\x -> do copyFile (source </> x) (dest </> x )) 
  return ()

readConf :: IO [String]
readConf = do
  let filename = ".prodignore"
  exist <- doesFileExist filename
  if exist then do
    s <- readFile filename
    return (lines s ++ [".*"])
  else return [".*"] 


main :: IO ()
main = do 
  prodExist <- checkProd
  rules <- readConf
  if prodExist then do
     copyFiles "./" "../prod" rules
     putStrLn "files copied"
  else putStrLn "prod not found"
