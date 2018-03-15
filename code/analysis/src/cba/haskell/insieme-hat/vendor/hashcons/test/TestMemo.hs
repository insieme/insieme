{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE TemplateHaskell #-}

module TestMemo (runTests) where

import Data.HashCons
import Data.HashCons.Memo

import Control.Concurrent.Async
import Control.DeepSeq
import Control.Exception (evaluate)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

genString :: Gen (HC String)
genString = hc <$> Gen.string (Range.linear 0 30) Gen.unicode


rev  = hc . reverse . getVal
mrev = memo rev

prop_rev = property $ do
  str <- forAll genString
  mrev str === rev str

mrev' = uncheckedMemo rev

prop_rev_unchecked = property $ do
  str <- forAll genString
  mrev' str === rev str


app :: HC String -> HC String -> HC String
app s1 s2 = hc $ getVal s1 ++ getVal s2
mapp = memo2 app

prop_app = property $ do
  s1 <- forAll genString
  s2 <- forAll genString
  s1 `mapp` s2 === s1 `app` s2


genStringList :: Gen [HC String]
genStringList = Gen.list (Range.linear 100 200) genString


prop_app_list = property $ do
  strs <- forAll genStringList
  map mrev strs === map rev strs

prop_app_list_conc = property $ do
  strs1 <- forAll genStringList
  strs2 <- forAll genStringList
  let revAll = evaluate . force . map mrev
  (strs1', strs2') <- evalIO $
    concurrently (revAll strs1) (revAll strs2)
  assert $ strs1' == map rev strs1 &&
           strs2' == map rev strs2


runTests :: IO Bool
runTests = checkSequential $$(discover)
