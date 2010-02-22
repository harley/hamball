-- For development without Cabal.
-- Cabal uses its own getDataFileName to find the right data files -Harley
module Paths_HamsterBalls where

getDataFileName :: FilePath -> IO FilePath
getDataFileName = return

