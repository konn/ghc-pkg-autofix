{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
module Main where
import Distribution.InstalledPackageInfo
import Distribution.Package hiding (depends)
import System.Process
import System.IO
import Data.List
import Control.Applicative
import System.FilePath
import Data.Maybe
import Data.List.Split

import PkgCheckParser

main :: IO ()
main = do
  (_, _, hErr, _) <- runInteractiveProcess "ghc-pkg" ["check"] Nothing Nothing
  src <- hGetContents hErr
  if null src
     then putStrLn "Nothing to fix. exit."
     else case parseCheck src of
            Left err -> hPrint stderr err
            Right ps -> do
              print ps
              mapM_ procPackage ps
              _ <- readProcess "ghc-pkg" ["recache", "--user"] ""
              return ()

procPackage :: BrokenPackage -> IO ()
procPackage P{packageID, brokenDeps} = do
  Just packID <- getPackageID packageID
  newPIDs <- mapM (getPackageID . getPackageName) brokenDeps
  let table = catMaybes $ zipWith (fmap.(,)) brokenDeps newPIDs
  infoPath <- getPackageInfoPath packID
  rslt  <- parseInstalledPackageInfo <$> readFile infoPath
  case rslt of
    ParseFailed err -> hPrint stderr err
    ParseOk _ pack  -> do
      let pack' = pack{depends=foldr (uncurry replace) (depends pack) table}
      writeFile infoPath $ showInstalledPackageInfo pack'

getPackageName :: String -> String
getPackageName pid = intercalate "-" $ take 2 $ splitOn "-" pid

getPackageID :: String -> IO (Maybe String)
getPackageID pName = do
  rsl <- parseInstalledPackageInfo <$> readProcess "ghc-pkg" ["describe", pName] ""
  case rsl of
    ParseFailed _  -> return Nothing
    ParseOk _ pack -> do
      let InstalledPackageId pid = installedPackageId pack
      return (Just pid)

replace :: String -> String -> [InstalledPackageId] -> [InstalledPackageId]
replace old new = intercalate [ InstalledPackageId new ] .
                    splitOn [InstalledPackageId old]

getPackageInfoPath :: String -> IO FilePath
getPackageInfoPath pid = do
  dir <- getConfDir
  return $ dir </> (pid++".conf")

getConfDir :: IO FilePath
getConfDir = do
  src <- readProcess "ghc-pkg" ["list", "--user"] ""
  return $ init $ head $ filter ((=='/').head) $ lines src
