{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}

module TestHashCons (runTests) where

import Data.HashCons
import GHC.Generics

import Control.Concurrent.Async
import Control.DeepSeq
import Control.Exception (evaluate)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


genString :: Gen String
genString = Gen.string (Range.linear 0 30) Gen.unicode

copyString :: String -> String
copyString = foldr (:) []


data HcString' =
    SNil
  | Char :> HcString
  deriving (Eq, Show, Generic)

type HcString = HC HcString'

instance NFData HcString' where
  rnf SNil      = ()
  rnf (c :> cs) = c `seq` rnf (getVal cs)

instance Hashable HcString'
instance HashCons HcString'

fromString :: String -> HcString
fromString = foldr (\x xs -> hc $ x :> xs) (hc SNil)

toString :: HcString -> String
toString = go . getVal where
  go SNil      = ""
  go (c :> cs) = c : toString cs

genHcString :: Gen HcString
genHcString = fromString <$> genString


genStringListWith :: String -> Gen (Int, [String])
genStringListWith str = do
  let listHalf = Gen.list (Range.linear 50 100) genString
  xs <- listHalf
  ys <- listHalf
  pure (length xs, xs ++ [str] ++ ys)


prop_hc_get_nonrec = property $ do
  a <- forAll genString
  getVal (hc a) === a

prop_eq_eq = property $ do
  a <- forAll genString
  let b = copyString a
  assert $ hc a == hc b

prop_rec = property $ do
  a <- forAll genHcString
  getVal a `deepseq` success

prop_neq_neq = property $ do
  a <- forAll genString
  b <- forAll $ Gen.filter (/= a) genString
  assert $ hc a /= hc b

prop_conc = property $ do
  str <- forAll genString
  (xi, xs) <- forAll $ genStringListWith str
  (yi, ys) <- forAll $ genStringListWith str
  let hcAll = evaluate . force . map fromString
  (xs', ys') <- evalIO $ concurrently (hcAll xs) (hcAll ys)
  assert $ xs'!!xi == ys'!!yi && toString (xs'!!xi) == str


runTests :: IO Bool
runTests = checkSequential $$(discover)
