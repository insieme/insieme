module Main where

import Criterion.Main
import Control.Monad
import Control.DeepSeq (NFData)

import qualified Data.Map as Map

import Insieme.Benchmark.InputData
import Insieme.Benchmark.Utils (ensureInspire, ensureHaskellDumper)

import Insieme.Inspire.Visit
import Insieme.Inspire.NodeAddress

import qualified Insieme.Inspire as IR

fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

visitAll :: IR.Tree -> Int -- [NodeAddress]
visitAll = length . collectAll (const True) . mkNodeAddress []

benchOnTree :: NFData a => (IR.Tree -> a) -> (FilePath, IR.Tree) -> Benchmark
benchOnTree f (p,ir) = bench p $ nf f ir

main = do

    ensureInspire
    ensureHaskellDumper

    inputs <- loadAllInputs

    defaultMain [
        bgroup "visitAll" $ benchOnTree visitAll <$> Map.toList inputs
                        --   [ bench "1"  $ whnf fib 1
                        --   , bench "5"  $ whnf fib 5
                        --   , bench "9"  $ whnf fib 9
                        --   , bench "11" $ whnf fib 11
                        --   ]
      ]
