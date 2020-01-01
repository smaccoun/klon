{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_klon (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
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
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/stevenmaccoun/.cabal/bin"
libdir     = "/Users/stevenmaccoun/.cabal/lib/x86_64-osx-ghc-8.2.2/klon-0.1.0.0-inplace"
dynlibdir  = "/Users/stevenmaccoun/.cabal/lib/x86_64-osx-ghc-8.2.2"
datadir    = "/Users/stevenmaccoun/.cabal/share/x86_64-osx-ghc-8.2.2/klon-0.1.0.0"
libexecdir = "/Users/stevenmaccoun/.cabal/libexec/x86_64-osx-ghc-8.2.2/klon-0.1.0.0"
sysconfdir = "/Users/stevenmaccoun/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "klon_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "klon_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "klon_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "klon_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "klon_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "klon_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
