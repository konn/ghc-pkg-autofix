{-# LANGUAGE NamedFieldPuns, RecordWildCards, DeriveDataTypeable #-}
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
import Control.Exception
import System.Console.CmdArgs

import PkgCheckParser

data Opts = Opts { sandbox :: Maybe FilePath }
          deriving (Show, Eq, Typeable, Data)

defOpts :: Opts
defOpts = Opts { sandbox = Nothing
                           &= help "Check cabal-dev's sandbox (default: cabal-dev)"
                           &= typ "PATH"
                           &= opt "cabal-dev" &= name "s" &= name "cabal-dev"
               } &= program "ghc-pkg-autofix"
                 &= summary "ghc-pkg-autofix: An automatic broken dependency fixer for Cabal"

main :: IO ()
main = do
  opts <- cmdArgs defOpts
  (_, _, hErr, _) <-
    case sandbox opts of
      Just path -> runInteractiveProcess "cabal-dev" ["-s", path, "ghc-pkg", "check"] Nothing Nothing
      Nothing   -> runInteractiveProcess "ghc-pkg" ["check"] Nothing Nothing
  src <- hGetContents hErr
  if null src
     then putStrLn "Nothing to fix. exit."
     else case parseCheck src of
            Left err -> hPrint stderr err
            Right ps -> do
              print ps
              mapM_ (procPackage (sandbox opts)) ps
              _ <- case sandbox opts of
                     Nothing -> readProcess "ghc-pkg" ["recache", "--user"] ""
                     Just dv -> readProcess "cabal-dev" ["-s", dv, "ghc-pkg", "recache", "--user"] ""
              return ()

procPackage :: Maybe FilePath -> BrokenPackage -> IO ()
procPackage mfp P{packageID, brokenDeps} = do
  Just packID <- getPackageID mfp packageID
  newPIDs <- mapM (getPackageID mfp . getPackageName) brokenDeps
  let table = catMaybes $ zipWith (fmap.(,)) brokenDeps newPIDs
  infoPath <- getPackageInfoPath mfp packID
  rslt  <- parseInstalledPackageInfo <$> readFile infoPath
  print (infoPath, rslt)
  case rslt of
    ParseFailed err -> hPrint stderr err
    ParseOk _ pack  -> do
      let pack' = pack{depends=foldr (uncurry replace) (depends pack) table}
      writeFile infoPath $ showInstalledPackageInfo pack'

init' :: [a] -> [a]
init' = reverse . drop 1 . reverse

getPackageName :: String -> String
getPackageName pid = intercalate "-" $ init' $ splitOn "-" pid

getPackageID :: Maybe FilePath -> String -> IO (Maybe String)
getPackageID mfp pName = handle handler $ do
  rsl <- parseInstalledPackageInfo <$>
           case mfp of
             Nothing -> readProcess "ghc-pkg" ["describe", pName] ""
             Just fp -> readProcess "cabal-dev" ["-s", fp, "ghc-pkg", "describe", pName] ""
  case rsl of
    ParseFailed _  -> return Nothing
    ParseOk _ pack -> do
      let InstalledPackageId pid = installedPackageId pack
      return (Just pid)
  where
    handler :: SomeException -> IO (Maybe String)
    handler e = print e >> return Nothing

replace :: String -> String -> [InstalledPackageId] -> [InstalledPackageId]
replace old new = intercalate [ InstalledPackageId new ] . splitOn [InstalledPackageId old]

getPackageInfoPath :: Maybe FilePath -> String -> IO FilePath
getPackageInfoPath mfp pid = do
  dir <- getConfDir mfp
  return $ dir </> (pid++".conf")

getConfDir :: Maybe FilePath -> IO FilePath
getConfDir mfp = do
  src <- case mfp of
           Nothing -> readProcess "ghc-pkg" ["list", "--user"] ""
           Just fp -> readProcess "cabal-dev" ["-s", fp, "ghc-pkg", "list", "--user"] ""
  let ans = init $ head $ drop 1 $ filter ("/" `isPrefixOf`) $ lines src
  return ans
