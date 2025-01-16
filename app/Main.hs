module Main where

import Lib (fullpath, getDirs, getFiles, ignore)
import System.Directory
import System.FilePath
import Control.Monad

checkProd :: IO Bool
checkProd = do
  dirs <- getDirs "../"
  let xs = filter (=="..//prod") dirs 
  return (not $ null xs)

copyFiles :: FilePath -> FilePath -> [String] -> IO ()
copyFiles source dest ws = do
  fs <- getFiles source
  let fs' = filter (\x -> not $ ignore x ws) fs
  forM_ fs' (\x -> copyFile (source </> x) (dest </> x))
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
