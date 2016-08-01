module Insieme.Analysis.Entities.ProgramPoint where

import Insieme.Inspire.NodeAddress

-- The execution phase of an expresssion
data Phase = Pre | Internal | Post 
    deriving (Eq, Ord, Show)

-- A point in the execution of a program
data ProgramPoint = ProgramPoint NodeAddress Phase
    deriving (Eq, Ord, Show)

