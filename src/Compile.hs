-- Compile.hs
-- Copyright 2015 Remy E. Goldschmidt <taktoa@gmail.com>
-- This file is part of hskpipe.
--    hskpipe is free software: you can redistribute it and/or modify
--    it under the terms of the GNU General Public License as published by
--    the Free Software Foundation, either version 3 of the License, or
--    (at your option) any later version.
--
--    hskpipe is distributed in the hope that it will be useful,
--    but WITHOUT ANY WARRANTY-- without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--    GNU General Public License for more details.
--
--    You should have received a copy of the GNU General Public License
--    along with hskpipe. If not, see <http://www.gnu.org/licenses/>.

module Compile where

import           Data.Functor    ((<$>))
import           Data.Map.Strict (Map, fromList)
import qualified Data.Map.Strict as M (lookup)
import           Data.Text       (Text)
import           Expr

symMap :: Map Text TType
symMap = fromList [ ("symbol", TSymbol)
                  , ("string", TString)
                  , ("number", TNumber)
                  , ("bool",   TBool)
                  , ("stream", TStream)
                  , ("error",  TError)
                  , ("stmt",   TStmt)
                  , ("any",    TAny)
                  ]


symToType :: Expr -> Maybe TType
symToType (ESymbol t) = M.lookup t symMap
symToType _           = Nothing

data TType = TSymbol
           | TString
           | TNumber
           | TBool
           | TStream
           | TError
           | TVector TType
           | TFunc  [TType] TType
           | TList  TType
           | TTuple [TType]
           | TClsr TType
           | TCont TType
           | TStmt
           | TAny
             deriving (Eq, Show, Read)

typeCheck' :: [(Expr, TType)] -> TType -> Maybe TType
typeCheck' [] k            = Just k
typeCheck' ((_, TAny):r) k = typeCheck' r k
typeCheck' ((e, t):rest) k
  | typeCheck e == Just t  = typeCheck' rest k
  | otherwise              = Nothing
typeCheck' _ _ = Nothing

typeCheck :: Expr -> Maybe TType
--- Primitives
typeCheck (ESymbol _)     = Just TSymbol
typeCheck (EString _)     = Just TString
typeCheck (EInt _)        = Just TNumber
typeCheck (EFloat _)      = Just TNumber
typeCheck ETrue           = Just TBool
typeCheck EFalse          = Just TBool
typeCheck ENil            = Just (TList TAny)
--- Control functions
typeCheck (EIf b x y)
  | typeCheck x == typeCheck y                = typeCheck x >>= typeCheck' [(b, TBool)]
  | otherwise                                 = Nothing
typeCheck (EAnd a b)      = typeCheck' [(a, TBool), (b, TBool)] TBool
typeCheck (EOr a b)       = typeCheck' [(a, TBool), (b, TBool)] TBool
--- Symbol functions
typeCheck (EIntern a)     = typeCheck' [(a, TString)] TSymbol
--- String functions
typeCheck (EPos a b)      = typeCheck' [(a, TString), (b, TNumber)] TString
typeCheck (ETlstr a)      = typeCheck' [(a, TString)] TString
typeCheck (ECn a b)       = typeCheck' [(a, TString), (b, TString)] TString
typeCheck (EStr _)        = Just TString
typeCheck (ENStr a)       = typeCheck' [(a, TNumber)] TString
typeCheck (EStrN a)       = typeCheck' [(a, TString)] TNumber
--- Errors
typeCheck (ESErr a)       = typeCheck' [(a, TString)] TAny
typeCheck (EEtS a)        = typeCheck' [(a, TError)] TString
typeCheck (ETrap a b)
  | typeCheck b == (TFunc [TError] <$> at)    = at
  | otherwise                                 = Nothing
  where
    at = typeCheck a
--- Lists
typeCheck (ECons a ENil)  = TList <$> typeCheck a
typeCheck (ECons a b)
  | typeCheck b == (TList <$> at)             = TList <$> at
  | otherwise                                 = Nothing
  where
    at = typeCheck a
typeCheck (EHd a)         = case typeCheck a of
                             Just (TList t) -> Just t
                             _              -> Nothing
typeCheck (ETl a)         = case typeCheck a of
                             Just (TList t) -> Just (TList t)
                             _              -> Nothing
--- Generic functions
typeCheck (EApp f a)      = case typeCheck f of
                             Just (TFunc as o) -> if typeCheck a == Just (TTuple as)
                                                  then Just o
                                                  else Nothing
                             _                 -> Nothing
typeCheck (EEval f)       = typeCheck f
typeCheck (EESeq t)       = case typeCheck t of
                            Just (TTuple xs) -> Just (last xs)
                            _                -> Nothing
typeCheck (ESet s a)      = typeCheck' [(s, TSymbol), (a, TAny)] TStmt
typeCheck (EVal s)        = typeCheck' [(s, TSymbol)] TAny
typeCheck EDefun{}        = Just TStmt
typeCheck (ELam s b)      = typeCheck b >>= typeCheck' [(s, TSymbol)]
typeCheck (ELet s a b)    = typeCheck b >>= typeCheck' [(s, TSymbol), (a, TAny)]
typeCheck (EFreeze a)     = TCont <$> typeCheck a
typeCheck (EType a s)     = case typeCheck a of
                             Just TAny -> symToType s
                             Just t    -> Just t
                             _         -> Nothing
--- Vectors
typeCheck (EVec a)        = case typeCheck a of
                             Just (TList t) -> Just (TVector t)
                             _              -> Nothing
typeCheck (EVPut a n v)   = case typeCheck a of
                             Just t -> typeCheck' [(n, TNumber), (v, TVector t)] t
                             _      -> Nothing
typeCheck (EVGet v n)     = case typeCheck v of
                             Just (TVector t) -> typeCheck' [(n, TNumber)] t
                             _                -> Nothing
--- Streams
typeCheck (ESWrite n s)   = typeCheck' [(n, TNumber), (s, TStream)] TNumber
typeCheck (ESRead s)      = typeCheck' [(s, TStream)] TNumber
typeCheck (ESOpen s)      = typeCheck' [(s, TString)] TStream
typeCheck (ESClose s)     = typeCheck' [(s, TStream)] (TList TNumber)
--- Time
typeCheck (ETime s)       = typeCheck' [(s, TSymbol)] TNumber
--- Arithmetic
typeCheck (EAdd a b)      = typeCheck' [(a, TNumber), (b, TNumber)] TNumber
typeCheck (ESub a b)      = typeCheck' [(a, TNumber), (b, TNumber)] TNumber
typeCheck (EMul a b)      = typeCheck' [(a, TNumber), (b, TNumber)] TNumber
typeCheck (EDiv a b)      = typeCheck' [(a, TNumber), (b, TNumber)] TNumber
--- Predicates
typeCheck (EEQ a b)
  | typeCheck a == typeCheck b                = Just TBool
  | otherwise                                 = Nothing
typeCheck (ELT a b)       = typeCheck' [(a, TNumber), (b, TNumber)] TBool
typeCheck (EGT a b)       = typeCheck' [(a, TNumber), (b, TNumber)] TBool
typeCheck (ELE a b)       = typeCheck' [(a, TNumber), (b, TNumber)] TBool
typeCheck (EGE a b)       = typeCheck' [(a, TNumber), (b, TNumber)] TBool
typeCheck (ENump n)       = typeCheck' [(n, TAny)] TBool
typeCheck (EStrp s)       = typeCheck' [(s, TAny)] TBool
typeCheck (EConsp c)      = typeCheck' [(c, TAny)] TBool
typeCheck (EVecp v)       = typeCheck' [(v, TAny)] TBool
typeCheck (EInline _)     = Just TAny
--typeCheck _               = Nothing

prettyc :: Expr -> String
prettyc = show
