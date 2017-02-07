{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_airtable_api (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/anand/Development/as/airtable-api/.stack-work/install/x86_64-linux/lts-7.19/8.0.1/bin"
libdir     = "/home/anand/Development/as/airtable-api/.stack-work/install/x86_64-linux/lts-7.19/8.0.1/lib/x86_64-linux-ghc-8.0.1/airtable-api-0.1.0.0-Kj2kZ5gQnWLJYb1VNpnNG3"
datadir    = "/home/anand/Development/as/airtable-api/.stack-work/install/x86_64-linux/lts-7.19/8.0.1/share/x86_64-linux-ghc-8.0.1/airtable-api-0.1.0.0"
libexecdir = "/home/anand/Development/as/airtable-api/.stack-work/install/x86_64-linux/lts-7.19/8.0.1/libexec"
sysconfdir = "/home/anand/Development/as/airtable-api/.stack-work/install/x86_64-linux/lts-7.19/8.0.1/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "airtable_api_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "airtable_api_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "airtable_api_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "airtable_api_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "airtable_api_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
