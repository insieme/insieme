import System.Directory
import System.Exit
import System.Environment
import System.FilePath
import System.FilePath.Glob

main :: IO ()
main = do

    args <- getArgs
    if null args then exitFailure else return ()

    -- find shared libraries
    libs <- head . fst <$> globDir [(compile "install/**/libHSinsieme-hat-*.so")] ".stack-work"

    -- attach timestamps
    ts   <- mapM getModificationTime libs

    -- find latest lib
    let latestlib = snd $ maximum $ zip ts libs

    -- copy original + simple name
    copyFile latestlib ((args !! 0) </> "libHSinsieme-hat.so")
    copyFile latestlib ((args !! 0) </> takeFileName latestlib)
