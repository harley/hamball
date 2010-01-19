module Paths_Yampa (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,9,2,3], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/accts/agt5/hamsters/bin"
libdir     = "/home/accts/agt5/hamsters/lib/Yampa-0.9.2.3/ghc-6.10.4"
datadir    = "/home/accts/agt5/hamsters/share/Yampa-0.9.2.3"
libexecdir = "/home/accts/agt5/hamsters/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "Yampa_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "Yampa_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "Yampa_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "Yampa_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
