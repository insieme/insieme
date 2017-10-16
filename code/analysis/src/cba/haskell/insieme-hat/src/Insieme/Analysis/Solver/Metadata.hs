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

{-# LANGUAGE NamedFieldPuns #-}

module Insieme.Analysis.Solver.Metadata where

import Data.Bifunctor
import Text.JSON

import Insieme.Inspire (NodePath)
import qualified Insieme.Inspire as I

data MetadataFile = MetadataFile
    { mfBookmarks  :: [NodePath]
    , mfExpands    :: [NodePath]
    , mfLabels     :: [(NodePath, String)]
    , mfHighlights :: [NodePath]
    , mfBodies     :: [(NodePath, MetadataBody)]
    }

data MetadataBody = MetadataBody
    { mGroups  :: [(String, MetadataGroup)]
    }

data MetadataGroup = MetadataGroup
    { mgLabel      :: String
    , mgSummary    :: String
    , mgDetails    :: Maybe String
    , mgLinkGroups :: [MetadataLinkGroup]
    }

data MetadataLinkGroup = MetadataLinkGroup
    { mlgHeading :: String
    , mlgLinks   :: [MetadataLink]
    }

data MetadataLink = MetadataLink
    { mlIdentifier :: String
    , mlLabel      :: String
    , mlAddress    :: NodePath
    , mlActive     :: Bool
    }

instance JSON MetadataFile where
    readJSON = undefined
    showJSON MetadataFile
                 {mfBookmarks, mfExpands, mfLabels, mfHighlights, mfBodies} =
        JSObject $ toJSObject $
            [ ("magic", showJSON "This is an Inspyer node-metadata dump")
            , ("bookmarks", showJSON mfBookmarks)
            , ("expands", showJSON mfExpands)
            , ("labels", showJSON mfLabels)
            , ("highlights", showJSON mfHighlights)
            , ("bodies_v2",
                  JSObject $ toJSObject $ map (bimap I.pprintNodePath showJSON) mfBodies)
            ]

instance JSON MetadataBody where
    readJSON = undefined
    showJSON MetadataBody {mGroups} =
        JSObject $ toJSObject
            [ ("groups", showJSON $ map fst mGroups)
            , ("group_map", showJSON $ toJSObject $ map (second showJSON) mGroups)
            ]

instance JSON MetadataGroup where
    readJSON = undefined
    showJSON MetadataGroup {mgLabel, mgSummary, mgDetails, mgLinkGroups} =
        JSObject $ toJSObject
            [ ("label", showJSON mgLabel)
            , ("summary", showJSON mgSummary)
            , ("details", maybe JSNull showJSON mgDetails)
            , ("link_groups", showJSON mgLinkGroups)
            ]

instance JSON MetadataLinkGroup where
    readJSON = undefined
    showJSON MetadataLinkGroup {mlgHeading, mlgLinks} =
        JSObject $ toJSObject
            [ ("heading", showJSON mlgHeading)
            , ("links", showJSON mlgLinks)
            ]

instance JSON MetadataLink where
    readJSON = undefined
    showJSON MetadataLink {mlIdentifier, mlLabel, mlAddress, mlActive} =
        JSObject $ toJSObject
            [ ("identifier", showJSON mlIdentifier)
            , ("label", showJSON mlLabel)
            , ("address", showJSON $ I.pprintNodePath mlAddress)
            , ("active", showJSON mlActive)
            ]
