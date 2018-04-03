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

{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Insieme.Solver.Statistics where

import Data.List
import Data.Bifunctor
import Text.Printf
import GHC.Stats

import qualified Data.Graph as Graph
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Insieme.Solver.Constraint
import Insieme.Solver.Identifier
import Insieme.Solver.SolverState
import Insieme.Solver.Var
import Insieme.Solver.VariableIndex
import Insieme.Solver.AssignmentView

showSolverStatistics :: SolverStats -> String
showSolverStatistics SolverStats {..} = unlines
  [ "========================================================= Solver Statistic =============================================================================================="
  , "             Analysis                #Vars              Updates          Updates/Var            ~Time[us]        ~Time/Var[us]               Resets           Resets/Var "
  , "========================================================================================================================================================================="
  , unlines $ map showAnalysisStats analysesStats
  , "-------------------------------------------------------------------------------------------------------------------------------------------------------------------------"
  , "               Total: " ++ concat
        [ printf "%20d" varsTotal
        , printf " %20d" updatesTotal, printf " %20.3f" updatesPerVar
        , printf " %20d" (round (pico_to_micro_sec timeTotal) :: Integer),    printf " %20.3f" (pico_to_micro_sec timePerVar)
        , printf " %20d" resetsTotal,  printf " %20.3f" resetsPerVar
        ]
  , "========================================================================================================================================================================="
  , showVarDependencyStats variableStats
  , "========================================================================================================================================================================="
  ]

pico_to_micro_sec :: (Real a) => a -> Double
pico_to_micro_sec a =  (realToFrac a / (10^6))

showGipedaSolverStatistics :: String -> SolverStats -> String
showGipedaSolverStatistics keyPostfix SolverStats {..} =
  unlines $ map (\(k,v) -> k ++ ";" ++ v) $ postfix ("/" ++ keyPostfix) $ concat $ 
    [ prefix "solver/" $ postfix "/totals" $
      [ value "vars"    varsTotal
      , value "updates" updatesTotal
      , value "time_us" (round (pico_to_micro_sec timeTotal))
      , value "resets"  resetsTotal
      ]
    , prefix "solver/" $ concat $
        flip concatMap analysesStats $ \AnalysisStats {..} -> 
          [ postfix ("/per_analysis/" ++ show asAnalysisId) $ concat
              [ [value "vars"     asVars]
              , [value "updates"  asUpdates]
              , [value "time_us" (round (pico_to_micro_sec asTime))]
              , [value "resets"   asResets]
              ]
          , postfix ("/per_var/" ++ show asAnalysisId) $
              [ value "updates"  asUpdatesPerVar
              , value "time_us" (pico_to_micro_sec asTimePerVar)
              , value "resets"   asResetsPerVar
              ]
          ]
    , case gcStats of 
        Nothing -> []
        Just GCStats {..} -> prefix "gc/" $
          [ value "total_gcs" numGcs
          , value "major_gcs" numByteUsageSamples
          , value "bytes_copied" bytesCopied
          , value "max_bytes_used" maxBytesUsed
          , value "total_memory_in_use_MB" peakMegabytesAllocated
          , value "MUT_cpu_sec" mutatorCpuSeconds
          , value "MUT_wall_sec" mutatorWallSeconds
          , value "GC_cpu_sec" gcCpuSeconds
          , value "GC_wall_sec" gcWallSeconds
          , value "TOTAL_cpu_sec" cpuSeconds
          , value "TOTAL_wall_sec" wallSeconds
          ]
    ]
  where
    prefix, postfix :: String -> [(String, a)] -> [(String, a)]
    prefix  str = map (first (str++))
    postfix str = map (first (++str))

    value :: Show a => String -> a -> (String, String)
    value key a = (key, show a)

showAnalysisStats :: AnalysisStats -> String
showAnalysisStats AnalysisStats {..} = 
    printf " %20s: %20d %20d %20.3f %20d %20.3f %20d %20.3f" 
        (show asAnalysisId)
        asVars
        asUpdates
        asUpdatesPerVar
        (round (pico_to_micro_sec asTime) :: Integer)
        (pico_to_micro_sec asTimePerVar)
        asResets
        asResetsPerVar

data SolverStats = 
    SolverStats
    { varsTotal     :: Integer
    , updatesTotal  :: Integer
    , timeTotal     :: Integer -- | in picoseconds (10^-12)
    , resetsTotal   :: Integer

    , updatesPerVar :: Float
    , timePerVar    :: Float
    , resetsPerVar  :: Float
      
    , analysesStats :: [AnalysisStats]
    , variableStats :: VarDependencyStats
    , gcStats       :: Maybe GCStats
    }

data AnalysisStats = 
    AnalysisStats
    { asAnalysisId    :: AnalysisIdentifier
    , asVars          :: Integer
    , asUpdates       :: Integer
    , asTime          :: Integer
    , asResets        :: Integer

    , asUpdatesPerVar :: Float
    , asTimePerVar    :: Float
    , asResetsPerVar  :: Float
    }

solverStats :: SolverState -> Maybe GCStats -> SolverStats
solverStats s@SolverState 
                    { numSteps  = Map.map toInteger -> numSteps
                    , numResets = Map.map toInteger -> numResets
                    , cpuTimes  = cpuTimes
                    } gcStats =
    SolverStats {..} 
  where
    vars = knownVariables $ variableIndex s

    varsTotal    = toInteger $ Set.size vars
    updatesTotal = sum numSteps
    timeTotal    = sum cpuTimes
    resetsTotal  = sum numResets

    updatesPerVar = perVar updatesTotal
    timePerVar    = perVar timeTotal
    resetsPerVar  = perVar resetsTotal

    perVar :: Integer -> Float
    perVar x = fromInteger x / fromInteger varsTotal

    analysisVars :: Map AnalysisIdentifier Integer
    analysisVars = Map.fromListWith (+) $ 
      map (\v -> (analysis (varIdent v), 1)) $ Set.toList vars

    analysesStats = Map.elems $ Map.mapWithKey go analyses_map
    variableStats = varDependencyStats s

    analyses_map
        :: Map AnalysisIdentifier (Integer, (Integer, (Integer, Integer)))
    analyses_map = 
        analysisVars >< numSteps >< cpuTimes >< numResets

    go asAnalysisId (asVars, (asUpdates, (asTime, asResets))) = 
        AnalysisStats {..}
      where
        asUpdatesPerVar = perAnlyVar asUpdates
        asTimePerVar    = perAnlyVar asTime
        asResetsPerVar  = perAnlyVar asResets
        
        perAnlyVar :: Integer -> Float
        perAnlyVar x = fromInteger x / fromInteger asVars

    -- | Stick maps with the same key type together into a tupel.
    (><) :: Ord k => Map k a -> Map k b -> Map k (a,b)
    (><) = Map.intersectionWith (,)
    infixr 9 ><

data VarDependencyStats = 
    VarDependencyStats
    { vdsNumSCCs     :: Int
    , vdsLargestSCCs :: [Int]
    }

varDependencyStats :: SolverState -> VarDependencyStats
varDependencyStats s = 
    VarDependencyStats 
    { vdsNumSCCs     = length sccs
    , vdsLargestSCCs = take 10 $ reverse $ sort $ length . Graph.flattenSCC <$> sccs
    }

  where
    ass = assignment s
    index = variableIndex s
    vars = knownVariables index

    nodes = go <$> Set.toList vars
      where
        go v = (v, v, dep v)
        dep v = foldr (\c l -> (dependingOn c $ UnfilteredView ass) ++ l) [] (constraints v)

    sccs = Graph.stronglyConnComp nodes


showVarDependencyStats :: VarDependencyStats -> String
showVarDependencyStats VarDependencyStats {..} = unlines
  [ "  Number of SCCs: " ++ show vdsNumSCCs
  , "  largest SCCs:   " ++ show vdsLargestSCCs
  ]
