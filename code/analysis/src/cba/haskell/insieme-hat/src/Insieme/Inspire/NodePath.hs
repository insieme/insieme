{-# LANGUAGE FlexibleContexts, RankNTypes, ScopedTypeVariables, MagicHash #-}
module Insieme.Inspire.NodePath 
    ( NodePath
    , ppNodePathStr
--    , ppNodePathBS
--    , ppNodePathVec
    ) where

-- import Control.Monad.ST
-- import qualified Data.ByteString as BS
-- import           Data.Vector.Generic (Vector)
-- import qualified Data.Vector.Generic as V
-- import qualified Data.Vector.Unboxed as UV
-- import qualified Data.Vector.Storable as SV
-- import qualified Data.Vector.Generic.Mutable.Dynamic as MDV
-- import           Data.Vector.Generic.Mutable.Dynamic (MVector)
-- import           Data.Vector.Storable.ByteString (vectorToByteString)
-- import           Data.List
-- import           Data.Word
-- import           Data.Char

type NodePath = [Int]

ppNodePathStr :: NodePath -> String
ppNodePathStr np = '0' : go np 
  where
    go [] = []
    go (n:np) = '-' : shows n (go np)


-- ppNodePathStr :: NodePath -> String
-- ppNodePathStr = map (chr . fromIntegral) . V.toList . storableVec . ppNodePathVec 100
--   where
--     storableVec :: UV.Vector a -> UV.Vector a
--     storableVec = id

-- ppNodePathBS :: NodePath -> BS.ByteString
-- ppNodePathBS = vectorToByteString . ppNodePathVec 10

-- -- This makes alloc% much, much worse. Not sure why.
-- ppNodePathVec :: Vector v Word8 => Int -> [Int] -> v Word8
-- {-# SPECIALIZE ppNodePathVec :: Int -> [Int] -> SV.Vector Word8 #-}
-- {-# SPECIALIZE ppNodePathVec :: Int -> [Int] -> UV.Vector Word8 #-}
-- {-# INLINE ppNodePathVec #-}
-- ppNodePathVec initial_size np = runST $ MDV.unsafeFreeze =<< do
--     mv <- MDV.new 0
--     MDV.reserve mv initial_size
--     go mv $ map ppIntRev (0:np)
--     return mv
--   where
--     go :: V.Vector v Word8 => MVector v s Word8 -> [[Word8]] -> ST s ()
--     go _  []     = return ()
--     go mv [x]    = mapM_ (MDV.pushBack mv) (reverse x)
--     go mv (x:xs) = do
--         mapM_ (MDV.pushBack mv) (reverse x)
--         MDV.pushBack mv dash
--         go mv xs

--     dash :: Word8
--     dash = fromIntegral $ ord '-'


-- ppIntRev :: Int -> [Word8]
-- {-# INLINE ppIntRev #-}
-- ppIntRev 0 = [fromIntegral $ ord '0']
-- ppIntRev x' = flip unfoldr x' $ \x ->
--         let (rest, lastDigit) = quotRem x 10
--             z = fromIntegral $ ord '0'
--             res = (z + fromIntegral lastDigit, rest)
--         in if x == 0
--              then Nothing
--              else Just res

