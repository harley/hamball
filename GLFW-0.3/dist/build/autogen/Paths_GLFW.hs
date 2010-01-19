module Paths_GLFW (
	version,
	getBinDir, getLibDir, getDataDir, getLibexecDir,
	getDataFileName
	) where

import Data.Version

version :: Version
version = Version {versionBranch = [0,3], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/accts/agt5/bin"
libdir     = "/home/accts/agt5/lib/GLFW-0.3/ghc-6.8.2"
datadir    = "/home/accts/agt5/share/GLFW-0.3"
libexecdir = "/home/accts/agt5/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = return bindir
getLibDir = return libdir
getDataDir = return datadir
getLibexecDir = return libexecdir

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = return (datadir ++ "/" ++ name)
