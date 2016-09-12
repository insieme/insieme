{-
 - Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
 -                Institute of Computer Science,
 -               University of Innsbruck, Austria
 -
 - This file is part of the INSIEME Compiler and Runtime System.
 -
 - We provide the software of this file (below described as "INSIEME")
 - under GPL Version 3.0 on an AS IS basis, and do not warrant its
 - validity or performance.  We reserve the right to update, modify,
 - or discontinue this software at any time.  We shall have no
 - obligation to supply such updates or modifications or any other
 - form of support to you.
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
 -
 - All copyright notices must be kept intact.
 -
 - INSIEME depends on several third party software packages. Please
 - refer to http://www.dps.uibk.ac.at/insieme/license.html for details
 - regarding third party software licenses.
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
