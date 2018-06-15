#!/usr/bin/env runghc

{-
 - Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 -                Institute of Computer Science,
 -               University of Innsbruck, Austria
 -
 - This file is part of the INSIEME Compiler and Runtime System.
 -
 - This program is free software: you can redistribute it and/or modify
 - it under the terms of the GNU General Public License as published by
 - the Free Software Foundation, either version 3 of the License, or
 - (at your option) any later version.
 -
 - This program is distributed in the hope that it will be useful,
 - but WITHOUT ANY WARRANTY; without even the implied warranty of
 - MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 - GNU General Public License for more details.
 -
 - You should have received a copy of the GNU General Public License
 - along with this program.  If not, see <http://www.gnu.org/licenses/>.
 -
 -
 - If you require different license terms for your intended use of the
 - software, e.g. for proprietary commercial or industrial use, please
 - contact us at:
 -                   insieme@dps.uibk.ac.at
 -
 - We kindly ask you to acknowledge the use of this software in any
 - publication or other disclosure of results by referring to the
 - following citation:
 -
 - H. Jordan, P. Thoman, J. Durillo, S. Pellegrini, P. Gschwandtner,
 - T. Fahringer, H. Moritsch. A Multi-Objective Auto-Tuning Framework
 - for Parallel Codes, in Proc. of the Intl. Conference for High
 - Performance Computing, Networking, Storage and Analysis (SC 2012),
 - IEEE Computer Society Press, Nov. 2012, Salt Lake City, USA.
 -}

import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Data.Function
import System.Exit
import System.Directory
import System.Environment
import System.FilePath
import System.Process
import System.Posix.Directory

getCabalFile :: FilePath -> IO FilePath
getCabalFile dir = do
  [cabal_file] <- filter ((==".cabal") . takeExtensions) <$> listDirectory dir
  return cabal_file

getPackageTopLevelField :: String -> FilePath -> IO String
getPackageTopLevelField f cabal_file = do
  [':':ver_field]
      <- map (dropWhile (/=':')) .  filter ((f ++ ":") `isPrefixOf`) . lines
           <$> readFile cabal_file
  return $ dropWhile isSpace ver_field

getPackageVersion, getPackageName :: FilePath -> IO String
getPackageVersion = getPackageTopLevelField "version"
getPackageName = getPackageTopLevelField "name"

getTargetPlatform :: IO (String, String, String)
getTargetPlatform = do
  [arch, vendor, os]
      <- split '-' . dropWhileEnd isSpace
         <$> readProcess "ghc" ["--print-target-platform"] ""
  return (arch, vendor, os)

getCabalTargetPlatform :: IO String
getCabalTargetPlatform = do
  (arch, _, os) <- getTargetPlatform
  ver <- getGHCVersion
  return $ intercalate "-" [arch, os, "ghc-" ++ ver]

split c str =
    filter (not . null) $ map catMaybes $ groupBy ((&&) `on` isJust) $ go c str
  where
    go _ [] = []
    go c (c':rest)
        | c == c'   = Nothing : go c rest
        | otherwise = Just c' : go c rest

getGHCVersion :: IO String
getGHCVersion =
    dropWhileEnd isSpace <$> readProcess "ghc" ["--numeric-version"] ""

parseComp "%library" = Lib
parseComp ('%':'e':'x':'e':':':comp) = (OtherComp 'x' comp)
parseComp ('%':'f':'l':'i':'b':':':comp) = (OtherComp 'f' comp)
-- parseComp ('%':'c':'o':'m':'p':':':comp) = (OtherComp 'f' comp)

main :: IO ()
main = do
  platform@(arch, _, os) <- getTargetPlatform
  ghc_ver  <- getGHCVersion

  let compBuildDir' = compBuildDir platform ghc_ver
  let compDistDir' = compDistDir platform ghc_ver

  prog <- getProgName
--  print =<< getArgs
  case prog of
    "hat-exec"    -> exec compBuildDir'
    "hat-haddock" -> haddock compDistDir'
    _             -> copyLib compBuildDir'

exec compBuildDir' = do
  builddir:cmd:args <- getArgs
  cabal_file <- getCabalFile =<< getCurrentDirectory

  pkg_name <- getPackageName    cabal_file
  pkg_ver  <- getPackageVersion cabal_file

  let distdir = builddir </> "dist-newstyle"
      dir = compBuildDir' distdir pkg_name pkg_ver (OtherComp 'x' cmd)

  exitWith =<< rawSystem (dir </> cmd) args

haddock compDistDir' = do
  builddir:comp:args <- getArgs
  cabal_file <- getCabalFile =<< getCurrentDirectory

  pkg_name <- getPackageName    cabal_file
  pkg_ver  <- getPackageVersion cabal_file

  let distdir = builddir </> "dist-newstyle"
      dir = compDistDir' distdir pkg_name pkg_ver (parseComp comp)

  void $ rawSystem "cabal" $ ["act-as-setup", "--", "haddock", "--builddir="++dir]++args
  void $ rawSystem "cp" ["-av", dir </> "doc" , builddir]
  putStrLn dir

data Comp = Lib | OtherComp { cType :: CompTy, cName :: CompName }

type CompTy = Char

compBuildDir, compDistDir
    :: (String, String, String) -> Version -> FilePath -> PkgName -> Version -> Comp -> FilePath

compBuildDir sys ghc_ver builddir pkg_name pkg_ver Lib =
    libCompBuildDir sys ghc_ver builddir pkg_name pkg_ver
compBuildDir sys ghc_ver builddir pkg_name pkg_ver (OtherComp ty comp) =
    normCompBuildDir sys ghc_ver builddir pkg_name pkg_ver comp ty

compDistDir sys ghc_ver builddir pkg_name pkg_ver Lib =
    libCompBuildDir sys ghc_ver builddir pkg_name pkg_ver
compDistDir sys ghc_ver builddir pkg_name pkg_ver (OtherComp ty comp) =
    normCompDistDir sys ghc_ver builddir pkg_name pkg_ver comp ty


normCompBuildDir, normCompDistDir :: (String, String, String)
             -> Version
             -> FilePath
             -> PkgName
             -> Version
             -> CompName
             -> CompTy
             -> FilePath
normCompBuildDir sys ghc_ver builddir pkg_name pkg_ver comp_name ty =
  libCompBuildDir sys ghc_ver builddir pkg_name pkg_ver
    </> [ty]
    </> comp_name
    </> "build"
    </> comp_name

normCompDistDir sys ghc_ver builddir pkg_name pkg_ver comp_name ty =
  libCompBuildDir sys ghc_ver builddir pkg_name pkg_ver
    </> [ty]
    </> comp_name

libCompBuildDir
    :: (String, String, String) -> Version -> FilePath -> PkgName -> Version -> FilePath
libCompBuildDir (arch, _, os) ghc_ver builddir pkg_name pkg_ver =
  builddir
    </> "build"
    </> (arch ++ "-" ++ os)
    </> ("ghc-" ++ ghc_ver)
    </> (pkg_name ++ "-" ++ pkg_ver)

type Version = String
type CompName = String
type PkgName = String

copyLib :: (FilePath -> PkgName -> Version -> Comp -> FilePath) -> IO ()
copyLib compBuildDir' = do
  [cabal_file, comp, builddir, destdir] <- getArgs

  pkg_name <- getPackageName    cabal_file
  pkg_ver  <- getPackageVersion cabal_file

  let distdir = builddir </> "dist-newstyle"
      srcdir = compBuildDir' distdir pkg_name pkg_ver (parseComp comp)
      lib = "lib" ++ pkg_name ++ ".so"
      srclib  = srcdir </> lib
      destlib = destdir </> lib

  copyFileWithMetadata srclib destlib
