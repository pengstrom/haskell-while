module While.Types where

import qualified Data.Map as Map

type Name = String

type Env = Map.Map Name Integer

data AExpr = Lit Integer | Var Name | Add AExpr AExpr | Mult AExpr AExpr | Sub AExpr AExpr
  deriving (Eq, Show)

data BExpr = TT | FF | Eq AExpr AExpr | Lte AExpr AExpr | Neg BExpr | Con BExpr BExpr
  deriving (Eq, Show)

data Stmt = Ass Name AExpr | Comp [Stmt] | If BExpr Stmt Stmt | While BExpr Stmt | Skip
  deriving (Eq, Show)
