{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_advent_of_code (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/alexdovzhanyn/personal/AdventOfCode/.stack-work/install/x86_64-osx/c527a6d576f11a1aa9d023cced98052fd74c4af83fc32e5235fd932a9ae46f99/9.6.6/bin"
libdir     = "/Users/alexdovzhanyn/personal/AdventOfCode/.stack-work/install/x86_64-osx/c527a6d576f11a1aa9d023cced98052fd74c4af83fc32e5235fd932a9ae46f99/9.6.6/lib/x86_64-osx-ghc-9.6.6/advent-of-code-0.1.0.0-3mNQPc0eOyLGGez4KaIgLN-advent-of-code"
dynlibdir  = "/Users/alexdovzhanyn/personal/AdventOfCode/.stack-work/install/x86_64-osx/c527a6d576f11a1aa9d023cced98052fd74c4af83fc32e5235fd932a9ae46f99/9.6.6/lib/x86_64-osx-ghc-9.6.6"
datadir    = "/Users/alexdovzhanyn/personal/AdventOfCode/.stack-work/install/x86_64-osx/c527a6d576f11a1aa9d023cced98052fd74c4af83fc32e5235fd932a9ae46f99/9.6.6/share/x86_64-osx-ghc-9.6.6/advent-of-code-0.1.0.0"
libexecdir = "/Users/alexdovzhanyn/personal/AdventOfCode/.stack-work/install/x86_64-osx/c527a6d576f11a1aa9d023cced98052fd74c4af83fc32e5235fd932a9ae46f99/9.6.6/libexec/x86_64-osx-ghc-9.6.6/advent-of-code-0.1.0.0"
sysconfdir = "/Users/alexdovzhanyn/personal/AdventOfCode/.stack-work/install/x86_64-osx/c527a6d576f11a1aa9d023cced98052fd74c4af83fc32e5235fd932a9ae46f99/9.6.6/etc"

getBinDir     = catchIO (getEnv "advent_of_code_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "advent_of_code_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "advent_of_code_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "advent_of_code_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "advent_of_code_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "advent_of_code_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
