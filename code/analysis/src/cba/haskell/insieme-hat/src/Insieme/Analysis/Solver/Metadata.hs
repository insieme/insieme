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

{-# LANGUAGE 
  NamedFieldPuns,
  RecordWildCards,
  TupleSections,
  DeriveGeneric,
  DeriveAnyClass,
  OverloadedStrings,
  ScopedTypeVariables,
  FlexibleInstances,
  GeneralizedNewtypeDeriving
  #-}

module Insieme.Analysis.Solver.Metadata where

import Control.DeepSeq
import GHC.Generics
import Data.Aeson as A
import Data.Aeson.Encoding as A
import qualified Data.ByteString.Builder as BSB
import Data.Char
import Data.List
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Monoid

import Insieme.Inspire (NodePath, parseNodePathStr)

type PPNodePath = String
type NodeId = String
type VarId = String
type TplId = String

data MetadataFile = MetadataFile
    { mfBookmarks  :: [PPNodePath]
    , mfExpands    :: [PPNodePath]
    , mfLabels     :: [(PPNodePath, String)]
    , mfHighlights :: [PPNodePath]
    , mfBodies     :: Map PPNodePath MetadataBody
    } deriving (Eq, Show, Generic, NFData)

data MetadataBody = MetadataBody
    { mGroups  :: [(String , MetadataGroup)]
    } deriving (Eq, Show, Generic, NFData)

data MetadataGroup = MetadataGroup
    { mgLabel      :: String
    , mgSummary    :: String
    , mgDetails    :: Maybe String
    , mgLinkGroups :: [MetadataLinkGroup]
    } deriving (Eq, Show, Generic, NFData)

data MetadataLinkGroup = MetadataLinkGroup
    { mlgHeading :: String
    , mlgLinks   :: [MetadataLink]
    } deriving (Eq, Show, Generic, NFData)

data MetadataLink = MetadataLink
    { mlIdentifier :: String
    , mlLabel      :: String
    , mlAddress    :: PPNodePath
    , mlActive     :: Bool
    } deriving (Eq, Show, Generic, NFData)
---------------------------------------

newtype PPNodePath' = PPNodePath' String deriving (Eq, Ord)

instance ToJSONKey PPNodePath' where
    toJSONKeyList = undefined
    toJSONKey = ToJSONKeyText f g
      where
        f (PPNodePath' _) = error ""
        g (PPNodePath' np) = A.unsafeToEncoding $ 
            BSB.char7 '"' <> BSB.string7 np <> BSB.char7 '"'

instance ToJSON PPNodePath' where
    toJSON = error "ToJSON PPNodePath'"
    toEncoding = unsafeToEncoding . 
                 fromEncoding . 
                 let ToJSONKeyText _ f = toJSONKey in f

instance ToJSON MetadataFile where
    toJSON = undefined
    toEncoding MetadataFile{..}
        = pairs $
            "magic"      .= ("This is an Inspyer node-metadata dump" :: String) <>
            "bookmarks"  .= mfBookmarks <>
            "expands"    .= mfExpands <>
            "labels"     .= mfLabels <>
            "highlights" .= mfHighlights <>
            "bodies_v2"  .= Map.mapKeys PPNodePath' mfBodies

instance ToJSON MetadataBody where
    toJSON = undefined
    toEncoding MetadataBody {..}
        = pairs $
            "groups"    .= map fst mGroups <>
            "group_map" .= Map.fromListWithKey checkDup mGroups
      where
        checkDup k a b
            | a == b = a
            | otherwise = error $ "MetadataBody: duplicate group key: " ++ k


instance ToJSON MetadataGroup where
    toJSON = undefined
    toEncoding MetadataGroup {..}
        = pairs $
            "label"       .= mgLabel <>
            "summary"     .= mgSummary <>
            "details"     .= mgDetails <>
            "link_groups" .= mgLinkGroups

instance ToJSON MetadataLinkGroup where
    toJSON = undefined
    toEncoding MetadataLinkGroup {..}
        = pairs $
            "heading" .= mlgHeading <>
            "links"   .= mlgLinks

instance ToJSON MetadataLink where
    toJSON = undefined
    toEncoding MetadataLink {..} 
        = pairs $
            "identifier" .= mlIdentifier <>
            "label"      .= mlLabel <>
            "address"    .= PPNodePath' mlAddress <>
            "active"     .= mlActive


splitSummary :: String -> (String, Maybe String)
splitSummary xs =
    case splitAt 100 xs of
      (s,[]) -> (s, Nothing)
      (s,rs) -> (s ++ takeWhile (not . isSpace) rs, Just xs)

scanForNodeAddrs :: (String -> (String, String)) -> String -> String
scanForNodeAddrs f [] = []
scanForNodeAddrs f xs@('0':'-':_) =
    let (x,r) = f xs in x ++ scanForNodeAddrs f r
scanForNodeAddrs f (x:xs) = x : scanForNodeAddrs f xs

shortenNodeAddr :: Map NodePath NodeId -> String -> (String, String)
shortenNodeAddr np_ni_map xs =
    let Just ((np, ppnp), r) = parseNodePathStr xs in
    (,r) $ case Map.lookupLE np np_ni_map of
      Just (np'@(_:_:_:_:_:_:_:_), ni)
          | np == np'
              -> "0-!"++ni++"-0"
          | Just npr <- stripPrefix np' np
              -> "0-:"++ni++"-" ++ intercalate "-" (map show npr)
          --  otherwise
          --     -> error $ "shortenNodeAddr: found addr not a prefix: " ++ show (np, np')
      _       -> ppnp
