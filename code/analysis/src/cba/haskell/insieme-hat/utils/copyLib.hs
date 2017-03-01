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
import Data.Time.Clock
import System.Directory
import System.Exit
import System.Environment
import System.FilePath
import System.FilePath.Glob

outOfDate :: UTCTime -> FilePath -> IO Bool
outOfDate ts dst = do
    exists <- doesFileExist dst
    if not exists
       then return True
       else do
           mtime <- getModificationTime dst
           return $ mtime < ts

main :: IO ()
main = do

    args  <- getArgs
    if null args then exitFailure else return ()

    -- find shared libraries
    libs  <- head . fst <$> globDir [(compile "install/**/libHSinsieme-hat-*.so")] ".stack-work"

    -- attach timestamps
    ts    <- mapM getModificationTime libs

    -- find latest lib
    let (latestts, latestlib) = maximum $ zip ts libs

    -- check for out-of-date
    isOod <- outOfDate latestts (args !! 0 </> "libHSinsieme-hat.so")

    -- copy original + simple name
    if isOod
        then do
            putStrLn $ "Copying over lib " ++ show latestlib
            copyFile latestlib  (args !! 0 </> "libHSinsieme-hat.so")
            -- setModificationTime (args !! 0 </> "libHSinsieme-hat.so") latestts
            copyFile latestlib  (args !! 0 </> takeFileName latestlib)
            -- setModificationTime (args !! 0 </> takeFileName latestlib) latestts
        else return ()
