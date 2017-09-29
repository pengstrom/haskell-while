module While.Eval where

import Control.Monad.Identity

import qualified Data.Map as Map
import Data.Maybe (fromJust)
import While.Types

import Debug.Trace

run0 :: Env -> Stmt -> Env
run0 env Skip = env
run0 env (Ass name a) = Map.insert name (aeval0 env a) env
run0 env (Comp (x:xs)) = run0 env' (Comp xs)
  where
    env' = run0 env x
run0 env (If b s1 s2) = if bv then env1 else env2
  where
    bv = beval0 env b
    env1 = run0 env s1
    env2 = run0 env s2
run0 env stmt@(While b s) = if bv then run0 env' stmt else env
  where
    bv   = beval0 env b
    env' = run0 env s

aeval0 :: Env -> AExpr -> Integer
aeval0 env (Lit n) = n
aeval0 env (Var x) = fromJust $ Map.lookup x env
aeval0 env (Add e1 e2) = v1 + v2
  where
    v1 = aeval0 env e1
    v2 = aeval0 env e2
aeval0 env (Mult e1 e2) = v1 * v2
  where
    v1 = aeval0 env e1
    v2 = aeval0 env e2
aeval0 env (Sub e1 e2) = v1 - v2
  where
    v1 = aeval0 env e1
    v2 = aeval0 env e2

beval0 :: Env -> BExpr -> Bool
beval0 env TT = True
beval0 env FF = False
beval0 env (Eq e1 e2) = v1 == v2
  where
    v1 = aeval0 env e1
    v2 = aeval0 env e2
beval0 env (Lte e1 e2) = v1 <= v2
  where
    v1 = aeval0 env e1
    v2 = aeval0 env e2
beval0 env (Neg b) = not $ beval0 env b
beval0 env (Con b1 b2) = v1 && v2
  where
    v1 = beval0 env b1
    v2 = beval0 env b2

type Eval1 a = Identity a
runEval1 :: Eval1 a -> a
runEval1 = runIdentity

aeval1 :: Env -> AExpr -> Eval1 Integer
aeval1 env (Lit n) = return n
aeval1 env (Var x) = case Map.lookup x env of
                       (Just n) -> return n
                       Nothing -> fail "Missing binding"
aeval1 env (Add e1 e2) = do
  v1 <- aeval1 env e1
  v2 <- aeval1 env e2
  return $ v1 + v2
aeval1 env (Sub e1 e2) = do
  v1 <- aeval1 env e1
  v2 <- aeval1 env e2
  return $ v1 - v2
aeval1 env (Mult e1 e2) = do
  v1 <- aeval1 env e1
  v2 <- aeval1 env e2
  return $ v1 * v2

beval1 :: Env -> BExpr -> Eval1 Bool
beval1 env TT = return True
beval1 env FF =  return False
beval1 env (Eq e1 e2) = do
  v1 <- aeval1 env e1
  v2 <- aeval1 env e2
  return $ v1 == v2
beval1 env (Lte e1 e2) = do
  v1 <- aeval1 env e1
  v2 <- aeval1 env e2
  return $ v1 <= v2
beval1 env (Neg b) = do
  bv <- beval1 env b
  return $ not bv
beval1 env (Con b1 b2) = do
  v1 <- beval1 env b1
  v2 <- beval1 env b2
  return $ v1 && v2

run1 :: Env -> Stmt -> Eval1 Env
run1 env Skip = return env
run1 env (Ass name a) = do
  v1 <- aeval1 env a
  return $ Map.insert name v1 env
run1 env (Comp []) = return env
run1 env (Comp (x:xs)) = do
  env'  <- run1 env x
  run1 env' (Comp xs)
run1 env (If b s1 s2) = do
  bv <- beval1 env b
  if bv
    then run1 env s1
    else run1 env s2
run1 env stmt@(While b s) = do
  bv <- beval1 env b
  if bv
    then do
      env' <- run1 env s
      run1 env' stmt
    else return env
