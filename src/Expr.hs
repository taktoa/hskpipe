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

import           Data.Text        (Text, unpack)
import qualified LLVM.General.AST as L

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
type KEInlineT = KExpr L.Module
type KEStmtT   = KExpr ()

data KExpr a where
--- Primitives
  KSymbol :: Text      -> KESymbolT
  KString :: Text      -> KEStringT
  KInt    :: Int       -> KENumberT
  KFloat  :: Double    -> KENumberT
  KTrue   :: KEBoolT
  KFalse  :: KEBoolT
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
  KApp    :: KEFuncT   -> KETupleT  -> KExpr a
  KEval   :: KExpr a   -> KExpr a
  KESeq   :: KETupleT  -> KExpr a
  KSet    :: KESymbolT -> KExpr a   -> KEStmtT
  KVal    :: KESymbolT -> KExpr a
  KDefun  :: KESymbolT -> KETupleT  -> KExpr a   -> KEStmtT
  KLam    :: KESymbolT -> KExpr a   -> KEFuncT
  KLet    :: KESymbolT -> KExpr a   -> KExpr a   -> KExpr a
  KFreeze :: KExpr a   -> KEContT
  KType   :: KExpr a   -> KESymbolT -> KExpr a
--- Vectors
  KVec    :: KEListT   -> KEVectorT
  KVPut   :: KExpr a   -> KENumberT -> KEVectorT -> KExpr a
  KVGet   :: KEVectorT -> KENumberT -> KExpr a
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
--- Predicates
  KEQ     :: KExpr a   -> KExpr a   -> KEBoolT
  KLT     :: KENumberT -> KENumberT -> KEBoolT
  KGT     :: KENumberT -> KENumberT -> KEBoolT
  KLE     :: KENumberT -> KENumberT -> KEBoolT
  KGE     :: KENumberT -> KENumberT -> KEBoolT
  KNump   :: KExpr a   -> KEBoolT
  KStrp   :: KExpr a   -> KEBoolT
  KConsp  :: KExpr a   -> KEBoolT
  KVecp   :: KExpr a   -> KEBoolT
--- Other
  KInline :: L.Module  -> KEInlineT

--  KHd     :: KEListT   -> KExpr a
--  KTl     :: KEListT   -> KEListT

instance Show (KExpr a) where
  show (KInt i)      = show i
  show (KFloat f)    = show f
  show (KSymbol s)   = unpack s
  show (KString s)   = show s
  show KNil          = "()"
  show (KInline l)   = "(inline \"" ++ show l ++ "\")"
--- Lists
  show (KHd xs)      = "(hd "  ++ show xs ++ ")"
  show (KTl xs)      = "(tl "  ++ show xs ++ ")"
  show (KCons x xs)  = "(cons "  ++ show x  ++ " " ++ show xs ++ ")"
--- Vectors
  show (KVec xs)     = "(absvector " ++ show xs ++ ")"
  show (KVPut e a v) = "(address-> " ++ show e ++ " " ++ show a ++ " " ++ show v ++ ")"
  show (KVGet a v)   = "(<-address " ++ show a ++ " " ++ show v ++ ")"
--- Streams
  show (KSWrite i s) = "(write-byte " ++ show i ++ " " ++ show s ++ ")"
  show (KSRead s)    = "(read-byte " ++ show s ++ ")"
  show (KSOpen p)    = "(open " ++ show p ++ ")"
  show (KSClose p)   = "(close " ++ show p ++ ")"
--- Time
  show (KTime s)     = "(get-time " ++ show s  ++ ")"
--- Arithmetic
  show (KAdd i1 i2)  = "(+ "    ++ show i1 ++ " " ++ show i2 ++ ")"
  show (KSub i1 i2)  = "(- "    ++ show i1 ++ " " ++ show i2 ++ ")"
  show (KMul i1 i2)  = "(* "    ++ show i1 ++ " " ++ show i2 ++ ")"
  show (KDiv i1 i2)  = "(/ "    ++ show i1 ++ " " ++ show i2 ++ ")"
--- Predicates
  show (KEQ i1 i2)   = "(= "    ++ show i1 ++ " " ++ show i2 ++ ")"
  show (KLT i1 i2)   = "(< "    ++ show i1 ++ " " ++ show i2 ++ ")"
  show (KGT i1 i2)   = "(< "    ++ show i1 ++ " " ++ show i2 ++ ")"
  show (KLE i1 i2)   = "(<= "   ++ show i1 ++ " " ++ show i2 ++ ")"
  show (KGE i1 i2)   = "(>= "   ++ show i1 ++ " " ++ show i2 ++ ")"
  show (KStrp a)     = "(string? "    ++ show a ++ ")"
  show (KConsp a)    = "(cons? "      ++ show a ++ ")"
  show (KNump a)     = "(number? "    ++ show a ++ ")"
  show (KVecp a)     = "(absvector? " ++ show a ++ ")"

data Expr =
--- Primitives
            ESymbol Text
          | EString Text
          | EInt    Int
          | EFloat  Double
          | ETrue
          | EFalse
          | ENil
--- Control functions
          | EIf     Expr Expr Expr
          | EAnd    Expr Expr
          | EOr     Expr Expr
--- Symbol functions
          | EIntern Expr
--- String functions
          | EPos    Expr Expr
          | ETlstr  Expr
          | ECn     Expr Expr
          | EStr    Expr
          | ENStr   Expr
          | EStrN   Expr
--- Errors
          | ESErr   Expr
          | EEtS    Expr
          | ETrap   Expr Expr
--- Lists
          | ECons   Expr Expr
          | EHd     Expr
          | ETl     Expr
--- Generic functions
          | EApp    Expr Expr
          | EEval   Expr
          | EESeq   Expr
          | ESet    Expr Expr
          | EVal    Expr
          | EDefun  Expr Expr Expr
          | ELam    Expr Expr
          | ELet    Expr Expr Expr
          | EFreeze Expr
          | EType   Expr Expr
--- Vectors
          | EVec    Expr
          | EVPut   Expr Expr Expr
          | EVGet   Expr Expr
--- Streams
          | ESWrite Expr Expr
          | ESRead  Expr
          | ESOpen  Expr
          | ESClose Expr
--- Time
          | ETime   Expr
--- Arithmetic
          | EAdd    Expr Expr
          | ESub    Expr Expr
          | EMul    Expr Expr
          | EDiv    Expr Expr
--- Predicates
          | EEQ     Expr Expr
          | ELT     Expr Expr
          | EGT     Expr Expr
          | ELE     Expr Expr
          | EGE     Expr Expr
          | ENump   Expr
          | EStrp   Expr
          | EConsp  Expr
          | EVecp   Expr
--- Other
          | EInline L.Module
            deriving (Show)


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
