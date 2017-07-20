#!/usr/bin/env runghc
-- To run directly you need 'process-listlike' installed
{-# LANGUAGE ViewPatterns, RecordWildCards #-}

import Control.Arrow
import Control.Monad
import Data.ByteString.Lazy (ByteString)
import Data.Function
import Data.List
import Data.Map (Map)
import Data.Maybe
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.FilePath
import System.Process (rawSystem)
import Text.Printf
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.Map as Map

usage = do
  prog <- getProgName
  putStrLn $ unlines $ map concat [
    [ "Usage: ", prog, " variables FILE.prof STATS" ],
    [ "       ",       " callgraph  (entries | ticks | bytes) FILE.prof" ],
    [ "       ",       " flamegraph (entries | ticks | bytes) FILE.prof" ]
   ]

main = do
  args <- getArgs
  case args of
    "variables":prof_file:stats_file:[] -> do
      prof  <- readFile prof_file
      stats <- readFile stats_file
      variablizeProfile opts prof stats

    "callgraph":ty:prof_file:[] | validTy ty -> do
      let opts' = graphviz_opts { oStat = stat ty }
      prof <- toStack opts' . parseProfile . lines <$> readFile prof_file
      putStrLn $ graphifyProfile opts' prof

    "flamegraph":ty:prof_file:[] | validTy ty -> do
      let opts' = opts { oStat = stat ty }
      prof <- parseProfile . lines <$> readFile prof_file
      -- mapM_ print prof
      putStr $ unlines $ ppStack opts' $ toStack opts' $ prof

    _ -> usage
 where
   validTy = (`elem` [ "entries", "ticks", "bytes" ])
   stat "entries" = plEntries
   stat "ticks"   = plTicks
   stat "bytes"   = plBytes

data Opts = Opts {
      oFormat    :: Id -> String
    , oStat      :: ProfileLine -> Int
    , oLabel     :: String
    , oFlamingra :: Bool
    }

opts, graphviz_opts :: Opts
opts =
    Opts (\(Id f m) -> m++":"++f) plTicks "ticks" True
graphviz_opts =
    Opts (\(Id f m) -> m++"<BR/>:"++f) plTicks "ticks" False

type SCC = String
type ANLY = String

variablizeProfile :: Opts -> String -> String -> IO ()
variablizeProfile opts@Opts {..} prof' stats' = do
  let
      prof = parseProfile $ lines prof'

      stats :: Map ANLY Int
      stats = Map.fromList $ map (\(t, (v, _, _, _)) -> (t,v))
            $ parseStats $ lines stats'

      ts = sum $ map oStat prof

      isccs :: Map SCC Int
      isccs = collect opts interesting_sccs prof

      gisccs :: Map ANLY Int
      gisccs = Map.mapKeysWith (+) (\k -> fromJust $ lookup k rev_table) isccs

      percentages :: Map ANLY Double
      percentages = Map.map (\i -> (fromIntegral i / fromIntegral ts :: Double))
                          gisccs

      mm = Map.intersectionWith (\s p -> p / fromIntegral s) stats percentages

  print ("total ticks", ts)
  print gisccs
  print mm

  forM (Map.toList mm) $
      \(anly, percpervar) ->
          printf "%15s c %6.10f\n" anly percpervar
  -- print $ flip mapMaybe anly_table $ \(t, mr) -> 
  --     flip fmap mr $ \(_, sccs) -> 
  --         sum $ flip mapMaybe sccs $ \scc -> 
  --             Map.lookup scc tm
  return ()


-- @dot -Tpng -o foo.png foo.dot@
graphifyProfile :: Opts -> StackProfile -> String
graphifyProfile Opts {..} prof =
    "digraph {\n"
     ++ "splines=ortho;\n"
--     ++ "nodesep=1;\n"
     ++ concatMap node (sumNodes graph) ++ "\n\n"
     ++ concatMap edge graph
     ++ "}\n"
 where
   graph = collapse $ map (first extractEdge) prof

   extractEdge :: [Id] -> (Id, Id)
   extractEdge (f:t:_) = (f,t)
   extractEdge [x] = (x, Id "ROOT" "ROOT")
   extractEdge [] = error "bäh, invalid profile"

   collapse :: [((Id, Id), Int)] -> [((Id, Id), Int)]
   collapse = nubWithBy (\(_, w) (x, w') -> (x, w + w')) (\(x,_) -> x)

   sumNodes :: [((Id,Id), Int)] -> [(Id, Int)]
   sumNodes g = Map.toList $ Map.fromListWith (+) $ map (first fst) g

   node :: (Id, Int) -> String
   node (n, w) =
       "\""++ oFormat n ++"\""
           ++ "[shape=box"
           ++ ",label=<"
                  ++ "<FONT POINT-SIZE=\""++ (show (v 50 w)) ++"\">"
                       ++ oFormat n ++"</FONT>"
                  ++ "<BR ALIGN=\"CENTER\"/>"
                  ++ show w ++ " " ++ oLabel
                ++ ">"
           ++ "];\n"

   ilog :: Int -> Float
   ilog = log . fromIntegral

   v :: Float -> Int -> Int
   v s w = min 255 $ floor $ max 8 $ s * ( ilog (1 + w) / ilog 10 )

   edge :: ((Id,Id), Int) -> String
   edge ((f,t), w) | w > 0 =
       "\""++ oFormat t ++"\" -> \""++ oFormat f ++ "\""
           ++"[xlabel=\""++ show w ++"\""
--           ++",weight="++ show (v 0.5 w - 8)
           ++",penwidth=1" -- ++ show (v 1 w)
           ++"];\n"
   edge ((f,t), w) | w == 0 =
       "\""++ oFormat t ++"\" -> \""++ oFormat f ++"\" [color=lightgray]\n"


nubWithBy :: (Ord a, Ord k) => (a -> a -> a) -> (a -> k) -> [a] -> [a]
nubWithBy f g = map snd . Map.toList . Map.fromListWith f . map (g &&& id)

type ParsedProfile = [ProfileLine]
type StackProfile = [([Id], Int)]

data Id = Id { idFunction :: String, idModule :: String }
          deriving (Eq, Ord, Read, Show)

data ProfileLine = ProfileLine { plLevel   :: Int
                               , plId      :: Id
                               , plEntries :: Int
                               , plTicks   :: Int
                               , plBytes   :: Int
                               }
          deriving (Eq, Ord, Read, Show)

parseProfile :: [String] -> [ProfileLine]
parseProfile ps =
      map ((\ (l, (i, e, t, b)) -> ProfileLine l i e t b )
           . (level &&& (parseLine . words)))
    $ drop 3
    $ dropWhile (not . ((&&) <$> ("COST CENTRE" `isPrefixOf`)
                             <*> ("no." `isInfixOf`)))
    $ ps

  where
    parseLine [ fun, mod, _src, _no, entries
              , _indTime, _indAlloc
              , _inhTime, _inhAlloc
              , ticks, bytes]
        = (Id fun mod, read entries, read ticks, read bytes)
    parseLine [ fun,mod, _no, entries
              , _indTime, _indAlloc
              , _inhTime, _inhAlloc
              , ticks, bytes ]
        = (Id fun mod, read entries, read ticks, read bytes)
    parseLine l = error $ "Unrecognized profile line: '"++show l++"'"

    level = (subtract 1) . length . takeWhile (==' ')

toStack :: Opts -> ParsedProfile -> [([Id], Int)]
toStack Opts {..} ccs =
    catMaybes $ snd $ mapAccumL go (0, []) ccs
  where
    go (l, s) p@(ProfileLine l' i _ _ _) 
        | not oFlamingra || oStat p > 0 = 
            let s' = i : drop (length s - l') s in
            ((l', s'), Just (s', oStat p) )
        | otherwise = 
            ((l', s), Nothing )

ppStack :: Opts -> [([Id], Int)] -> [String]
ppStack Opts {..} =
    concatMap go
 where
   go (s, ts) | ts == 0 = []
   go (s, ts)           = 
       [intercalate ";" (map oFormat (reverse s)) ++ " " ++ show ts]

collect :: Opts -> [String] -> ParsedProfile -> Map String Int
collect Opts {..} sccs ls = ls
                & filter ((`elem` sccs) . idFunction . plId)
                & map ((idFunction . plId) &&& oStat)
                & Map.fromListWith (+)

parseStats :: [String] -> [(String, (Int, Int, String, Int))]
parseStats ls = ls
    & map words
    & map (\[t, v, u, uv, dt, dtv, rst, rstv] ->
        if "Total" `isPrefixOf` t
           then []
           else [(takeWhile (/=':') t, (read v, read u, dt, read rst))])
    & takeWhile (not . null)
    & concat

anly_table:: [(ANLY, Maybe [SCC])]
anly_table = [ ( "WS"             , Nothing )
             , ( "B"              , Nothing )
             , ( "RecLambdaRefs"  , Nothing )
             , ( "MAP"            , Nothing )
             , ( "C"              , Nothing )
             , ( "CS"             , Nothing )
             , ( "R"              , Nothing )
             , ( "MR"             , Nothing )
             , ( "I"              , Nothing )
             , ( "MA"             , Nothing )
             , ( "DV-A"           , Nothing )
             , ( "A"              , Nothing )
             , ( "EP"             , Just [ "dps__exitpoints_dep"] )
             , ( "pred_of"        , Just [ "dps__pred_pre_dep"
                                         , "dps__pred_internal_dep"
                                         , "dps__pred_post_dep"
                                         ] )
             , ( "FreeLambdaRefs" , Just ["dps__freelam_dep"] )
             , ( "RD"             , Just ["dps__memorystate_dep"
                                         , "dps__programpoint_dep"
                                         ] )
             , ( "DP"             , Just [ "dps__datapath_member_dep"
                                         , "dps__datapath_element_dep"
                                         ] )
             , ( "MDP"            , Nothing )
             , ( "MI"             , Nothing )
             , ( "AP"             , Nothing )
             , ( "WriteSet"       , Nothing )
             , ( "R[in]"          , Nothing )
             , ( "R[out]"         , Nothing )
             ]

rev_table :: [(SCC, ANLY)]
rev_table =
    concat $ flip map anly_table $ \(t, msccs) ->
        [ (scc, t) | scc <- concat (maybeToList msccs) ]

interesting_sccs = concat $ mapMaybe snd anly_table
