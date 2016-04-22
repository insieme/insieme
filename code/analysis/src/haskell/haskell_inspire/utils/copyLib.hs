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
    libs <- head . fst <$> globDir [(compile "install/**/libHShaskell-inspire-*.so")] ".stack-work"

    -- attach timestamps
    ts   <- mapM getModificationTime libs

    -- find latest lib
    let latestlib = snd $ maximum $ zip ts libs

    -- copy original + simple name
    copyFile latestlib ((args !! 0) </> "libHShaskell-inspire.so")
    copyFile latestlib ((args !! 0) </> takeFileName latestlib)
