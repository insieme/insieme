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
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Monoid

type PPNodePath = String

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



