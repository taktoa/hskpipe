-- Expr.hs
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

module Expr where

import           Data.Text (Text)

data KSymbolT
data KNumberT
data KStringT
data KBoolT
data KStreamT
data KErrorT
data KVectorT
data KFuncT
data KListT
data KTupleT
data KClsrT
data KContT

type KESymbolT = KExpr KSymbolT
type KENumberT = KExpr KNumberT
type KEStringT = KExpr KStringT
type KEBoolT   = KExpr KBoolT
type KEStreamT = KExpr KStreamT
type KEErrorT  = KExpr KErrorT
type KEVectorT = KExpr KVectorT
type KEFuncT   = KExpr KFuncT
type KEListT   = KExpr KListT
type KETupleT  = KExpr KTupleT
type KEClsrT   = KExpr KClsrT
type KEContT   = KExpr KContT
type KEStmtT   = KExpr ()

data KExpr a where
--- Primitives
  KSymbol :: Text      -> KESymbolT
  KString :: Text      -> KEStringT
  KInt    :: Int       -> KENumberT
  KFloat  :: Float     -> KENumberT
  KNil    :: KEListT
--- Control functions
  KIf     :: KEBoolT   -> KExpr a   -> KExpr a   -> KExpr a
  KAnd    :: KEBoolT   -> KEBoolT   -> KEBoolT
  KOr     :: KEBoolT   -> KEBoolT   -> KEBoolT
--- Symbol functions
  KIntern :: KEStringT -> KESymbolT
--- String functions
  KPos    :: KEStringT -> KENumberT -> KEStringT
  KTlstr  :: KEStringT -> KEStringT
  KCn     :: KEStringT -> KEStringT -> KEStringT
  KStr    :: KExpr a   -> KEStringT
  KNStr   :: KENumberT -> KEStringT
  KStrN   :: KEStringT -> KENumberT
--- Errors
  KSErr   :: KEStringT -> KExpr a
  KEtS    :: KEErrorT  -> KEStringT
  KTrap   :: KExpr a   -> KEFuncT   -> KExpr a
--- Lists
  KCons   :: KExpr a   -> KEListT   -> KEListT
  KHd     :: KEListT   -> KExpr a
  KTl     :: KEListT   -> KEListT
--- Generic functions
  KApp    :: KEFuncT   -> KEListT   -> KExpr a
  KEval   :: KExpr a   -> KExpr a
  KESeq   :: KEListT   -> KExpr a
  KSet    :: KESymbolT -> KExpr a   -> KEStmtT
  KVal    :: KESymbolT -> KExpr a
  KDefun  :: KESymbolT -> KEListT   -> KExpr a   -> KEStmtT
  KLam    :: KESymbolT -> KExpr a   -> KEFuncT
  KLet    :: KESymbolT -> KExpr a   -> KExpr a   -> KExpr a
  KEq     :: KExpr a   -> KExpr a   -> KEBoolT
  KFreeze :: KExpr a   -> KEContT
  KType   :: KExpr a   -> KESymbolT -> KExpr a
--- Vectors
  KVec    :: KEListT   -> KEVectorT
  KVPut   :: KExpr a   -> KENumberT -> KEVectorT -> KExpr a
  KVGet   :: KEVectorT -> KENumberT -> KExpr a
  KVecp   :: KExpr a   -> KEBoolT
--- Streams
  KSWrite :: KENumberT -> KEStreamT -> KENumberT
  KSRead  :: KEStreamT -> KENumberT
  KSOpen  :: KEStringT -> KEStreamT
  KSClose :: KEStreamT -> KEListT
--- Time
  KTime   :: KESymbolT -> KENumberT
--- Arithmetic
  KAdd    :: KENumberT -> KENumberT -> KENumberT
  KSub    :: KENumberT -> KENumberT -> KENumberT
  KMul    :: KENumberT -> KENumberT -> KENumberT
  KDiv    :: KENumberT -> KENumberT -> KENumberT
  KLT     :: KENumberT -> KENumberT -> KEBoolT
  KGT     :: KENumberT -> KENumberT -> KEBoolT
  KLE     :: KENumberT -> KENumberT -> KEBoolT
  KGE     :: KENumberT -> KENumberT -> KEBoolT
--- Predicates
  KStrp   :: KExpr a   -> KEBoolT
  KConsp  :: KExpr a   -> KEBoolT
  KNump   :: KExpr a   -> KEBoolT

-- import           Control.Applicative (Applicative (..))
-- import           Control.Monad       (ap, liftM)
-- import           Data.IntMap.Strict  (IntMap)
-- import qualified Data.IntMap.Strict  as IM (empty, union)
-- import           Data.Map.Strict     (Map, empty, union)
-- import           Data.String         (IsString (..))
-- import           Data.Text           (Text, pack)

-- newtype Name = Name Text
--              deriving (Eq, Ord, Read, Show)

-- instance IsString Name where
--   fromString = Name . pack

-- type Env = Map Name Int

-- type Store = (Int, IntMap Expr)

-- data Expr = ELam Name  Expr
--           | EMu  Name  Expr
--           | EApp Expr  Expr
--           | ERef Name
--           | ERat Rational
--           | ETF  Bool
--           | ELE  Expr  Expr
--           | EIf  Expr  Expr Expr
--           | ENeg Expr
--           | EAdd Expr  Expr
--           | ERcp Expr
--           | EMul Expr  Expr
--             deriving (Eq, Show, Read)

-- data GClosure e = Clsr Env Store e
--                 deriving (Eq, Show, Read)

-- type Closure = GClosure Expr
-- type EClosure = GClosure ()

-- instance Functor GClosure where
--     fmap = liftM

-- instance Applicative GClosure where
--     pure  = return
--     (<*>) = ap

-- instance Monad GClosure where
--   return  = Clsr empty (0, IM.empty)
--   a >>= f = Clsr (e' `union` e) (i' + i, IM.union s' s) x'
--     where
--       Clsr e  (i,  s)  x  = a
--       Clsr e' (i', s') x' = f x
