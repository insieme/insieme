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

getTargetPlatform :: FilePath -> IO (String, String, String)
getTargetPlatform ghcPath = do
  [arch, vendor, os]
      <- split '-' . dropWhileEnd isSpace
         <$> readProcess ghcPath ["--print-target-platform"] ""
  return (arch, vendor, os)

getCabalTargetPlatform :: FilePath -> IO String
getCabalTargetPlatform ghcPath = do
  (arch, _, os) <- getTargetPlatform ghcPath
  ver <- getGHCVersion ghcPath
  return $ intercalate "-" [arch, os, "ghc-" ++ ver]

split c str =
    filter (not . null) $ map catMaybes $ groupBy ((&&) `on` isJust) $ go c str
  where
    go _ [] = []
    go c (c':rest)
        | c == c'   = Nothing : go c rest
        | otherwise = Just c' : go c rest

getGHCVersion :: FilePath -> IO String
getGHCVersion ghcPath =
    dropWhileEnd isSpace <$> readProcess ghcPath ["--numeric-version"] ""

parseComp "%library" = Lib
parseComp ('%':'e':'x':'e':':':comp) = (OtherComp 'x' comp)
parseComp ('%':'t':'e':'s':'t':':':comp) = (OtherComp 't' comp)
parseComp ('%':'f':'l':'i':'b':':':comp) = (OtherComp 'f' comp)
-- parseComp ('%':'c':'o':'m':'p':':':comp) = (OtherComp 'f' comp)

compName Lib = "library"
compName (OtherComp _ comp) = comp

main :: IO ()
main = do
  prog <- getProgName
  case prog of
    "hat-exec"    -> exec
    "hat-haddock" -> haddock
    _             -> copyLib

exec = do
  builddir:cmd:args <- getArgs
  cabal_file <- getCabalFile =<< getCurrentDirectory
  compBuildDir' <- applyGhcInfo builddir compBuildDir

  exes <- case cmd of
    '%':_ -> (:[]) <$> findExePath builddir cabal_file compBuildDir' (parseComp cmd)
    _     -> mapM (findExePath builddir cabal_file compBuildDir' . flip OtherComp cmd) ['x', 't']

  fexes <- mapM exists exes
  case catMaybes fexes of
    []  -> error $ "Couldn't find any of:\n" ++ intercalate "\n" (map ("    "++) exes) ++ "\n"
    [exe] -> exitWith =<< rawSystem exe args
    _   -> error $ "Ambigous command '" ++ cmd ++ "' please specify component: maybe %exe:" ++ cmd ++ " or %test:" ++ cmd
                ++ "\n"

  where
    exists path = do
         ex <- doesFileExist path
         return $ if ex then Just path else Nothing

    findExePath :: FilePath -> FilePath -> (FilePath -> String -> Version -> Comp -> FilePath) -> Comp -> IO FilePath
    findExePath builddir cabal_file compBuildDir' comp = do
        withCabalFileCtx cabal_file $ \pkg_name pkg_ver -> do
            let distdir = distdir_path builddir
                dir = compBuildDir' distdir pkg_name pkg_ver comp
                exe = dir </> compName comp
            return $ exe

distdir_path builddir = builddir </> "hat-vanilla-project" </> "dist-newstyle"

haddock = do
  builddir:comp:args <- getArgs
  cabal_file <- getCabalFile =<< getCurrentDirectory

  compDistDir' <- applyGhcInfo builddir compDistDir
  pkg_name <- getPackageName    cabal_file
  pkg_ver  <- getPackageVersion cabal_file

  let distdir = distdir_path builddir
      dir = compDistDir' distdir pkg_name pkg_ver (parseComp comp)

  void $ rawSystem "cabal" $ ["act-as-setup", "--", "haddock", "--builddir="++dir]++args
  void $ rawSystem "cp" ["-av", dir </> "doc" , builddir]
  putStrLn dir

copyLib = do
  [cabal_file, comp, builddir, destdir] <- getArgs

  compBuildDir' <- applyGhcInfo builddir compBuildDir
  pkg_name <- getPackageName    cabal_file
  pkg_ver  <- getPackageVersion cabal_file

  let distdir = distdir_path builddir
      srcdir = compBuildDir' distdir pkg_name pkg_ver (parseComp comp)
      lib = "lib" ++ pkg_name ++ ".so"
      srclib  = srcdir </> lib
      destlib = destdir </> lib

  copyFileWithMetadata srclib destlib


withCabalFileCtx :: FilePath -> (String -> Version -> IO a) -> IO a
withCabalFileCtx cabal_file f = do
  pkg_name <- getPackageName    cabal_file
  pkg_ver  <- getPackageVersion cabal_file
  f pkg_name pkg_ver


applyGhcInfo :: FilePath -> ((String, String, String) -> Version -> a) -> IO a
applyGhcInfo builddir g = do
    let ghcPath = builddir </> "third_party" </> "ghc" </> "bin" </> "ghc"
    platform@(arch, _, os) <- getTargetPlatform ghcPath
    ghc_ver  <- getGHCVersion ghcPath
    return $ g platform ghc_ver


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
type CompTy = Char
type PkgName = String
data Comp = Lib | OtherComp { cType :: CompTy, cName :: CompName }
