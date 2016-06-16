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
    putStrLn $ "Is ood: " ++ show isOod
    if isOod
        then do
            putStrLn $ "Copying over lib " ++ show latestlib
            copyFile latestlib  (args !! 0 </> "libHSinsieme-hat.so")
            -- setModificationTime (args !! 0 </> "libHSinsieme-hat.so") latestts
            copyFile latestlib  (args !! 0 </> takeFileName latestlib)
            -- setModificationTime (args !! 0 </> takeFileName latestlib) latestts
        else return ()
