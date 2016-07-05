{-# LANGUAGE TemplateHaskell #-}

module Insieme.Inspire where

import Control.Applicative
import Insieme.Inspire.ThHelpers
import Language.Haskell.TH

#c
#define CONCRETE(name) NT_##name,
enum NodeType {
#include "insieme/core/ir_nodes.def"
};
#undef CONCRETE
#endc

-- | NodeType taken from Insieme header files.
{#enum NodeType {} deriving (Eq, Show)#}

-- | Generates Inspire data structure.
$(let

    base :: Q [Dec]
    base =
      [d|
        data Inspire =
              BoolValue   Bool
            | CharValue   Char
            | IntValue    Int
            | UIntValue   Int
            | StringValue String
          deriving (Eq, Ord, Show, Read)
      |]

    extend :: Q [Dec] -> Q [Dec]
    extend base = do
        [DataD [] n [] cons ds]       <- base
        TyConI (DataD [] _ [] nt_cons _) <- reify ''NodeType
        let cons' = genCons <$> filter (not . isLeaf) nt_cons
        return $ [DataD [] n [] (cons ++ cons') ds]

    genCons :: Con -> Con
    genCons (NormalC n _) = NormalC (removePrefix "NT_" n) []

  in
    extend base
 )

-- | Generates toNodeType and fromNodeType functions.
$(let

    baseFromNodeType :: Q [Dec]
    baseFromNodeType =
      [d|
        fromNodeType :: NodeType -> Inspire
        fromNodeType NT_BoolValue   = BoolValue False
        fromNodeType NT_CharValue   = CharValue ' '
        fromNodeType NT_IntValue    = IntValue 0
        fromNodeType NT_UIntValue   = UIntValue 0
        fromNodeType NT_StringValue = StringValue ""
      |]

    baseToNodeType :: Q [Dec]
    baseToNodeType =
      [d|
        toNodeType :: Inspire -> NodeType
        toNodeType (BoolValue   _) = NT_BoolValue
        toNodeType (CharValue   _) = NT_CharValue
        toNodeType (IntValue    _) = NT_IntValue
        toNodeType (UIntValue   _) = NT_UIntValue
        toNodeType (StringValue _) = NT_StringValue
      |]

    extend :: Q [Dec] -> (Con -> Clause) -> Q [Dec]
    extend base gen = do
        [d, FunD n cls]               <- base
        TyConI (DataD [] _ [] cons _) <- reify ''NodeType
        let cls' = gen <$> filter (not . isLeaf) cons
        return $ [d,FunD n (cls ++ cls')]

    genClauseFromNodeType :: Con -> Clause
    genClauseFromNodeType (NormalC n _) = Clause [ConP n []] (NormalB (ConE (removePrefix "NT_" n))) []

    genClauseToNodeType :: Con -> Clause
    genClauseToNodeType (NormalC n _) = Clause [ConP (removePrefix "NT_" n) []] (NormalB (ConE n)) []

  in
    (++) <$> extend baseFromNodeType genClauseFromNodeType
         <*> extend baseToNodeType   genClauseToNodeType
 )
