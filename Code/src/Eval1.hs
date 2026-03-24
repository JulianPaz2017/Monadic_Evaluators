module Eval1
  ( eval
  , Env
  )
where

import           AST
import           Monads
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Prelude                 hiding ( fst
                                                , snd
                                                )
import           Data.Strict.Tuple
import           Control.Monad                  ( liftM
                                                , ap
                                                )

-- Entornos
type Env = M.Map Variable Int

-- Entorno nulo
initEnv :: Env
initEnv = M.empty

-- Mónada estado
newtype State a = State { runState :: Env -> Pair a Env }

instance Monad State where
  return x = State (\s -> (x :!: s))
  m >>= f = State (\s -> let (v :!: s') = runState m s in runState (f v) s')

-- Para calmar al GHC
instance Functor State where
  fmap = liftM

instance Applicative State where
  pure  = return
  (<*>) = ap

instance MonadState State where
  lookfor v = State (\s -> (lookfor' v s :!: s))
    where lookfor' v s = fromJust $ M.lookup v s
  update v i = State (\s -> (() :!: update' v i s)) where update' = M.insert

-- Ejercicio 1.b: Implementar el evaluador utilizando la monada State

-- Evalua un programa en el estado nulo
eval :: Comm -> Env
eval p = snd (runState (stepCommStar p) initEnv)

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: MonadState m => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: MonadState m => Comm -> m Comm
stepComm (Let var intExp)                 = 
  do {
      e <- evalExp intExp;
      update var e;
      return Skip
     }
stepComm (Seq Skip comm2)                 = return comm2
stepComm (Seq comm1 comm2)                = 
  do {
      c1 <- stepComm comm1;
      return (Seq c1 comm2)
     } 
stepComm (IfThenElse boolExp comm1 comm2) = 
  do {
      b <- evalExp boolExp;
      if b then return comm1 else return comm2
     }
stepComm (Repeat boolExp comm)            = 
  do {
      let comm' = IfThenElse boolExp Skip (Repeat boolExp comm)
      in return (Seq comm comm')
     }

-- Evalua una expresion
evalExp :: MonadState m => Exp a -> m a
evalExp (Const n)               = return n
evalExp (Var var)               = lookfor var
evalExp (UMinus intExp)         = 
  do {
      e <- evalExp intExp;
      return (-e)
     }
evalExp (Plus intExp1 intExp2)  = 
  do {
      e1 <- evalExp intExp1;
      e2 <- evalExp intExp2;
      return (e1 + e2)
     }
evalExp (Minus intExp1 intExp2) = 
  do {
      e1 <- evalExp intExp1;
      e2 <- evalExp intExp2;
      return (e1 - e2)
     }
evalExp (Times intExp1 intExp2) = 
  do {
      e1 <- evalExp intExp1;
      e2 <- evalExp intExp2;
      return (e1 * e2)
     }
evalExp (Div intExp1 intExp2)   = 
  do {
      e1 <- evalExp intExp1;
      e2 <- evalExp intExp2;
      return (e1 `div` e2)
     }
-- Bool
evalExp BTrue                   = return True
evalExp BFalse                  = return False
evalExp (Lt intExp1 intExp2)    = 
  do {
      e1 <- evalExp intExp1;
      e2 <- evalExp intExp2;
      return (e1 <= e2)
     }
evalExp (Gt intExp1 intExp2)    = 
  do {
      e1 <- evalExp intExp1;
      e2 <- evalExp intExp2;
      return (e1 >= e2)
     }
evalExp (Eq intExp1 intExp2)    = 
  do {
      e1 <- evalExp intExp1;
      e2 <- evalExp intExp2;
      return (e1 == e2)
     }
evalExp (NEq intExp1 intExp2)   = 
  do {
      e1 <- evalExp intExp1;
      e2 <- evalExp intExp2;
      return (not (e1 == e2))
     }
evalExp (And boolExp1 boolExp2) = 
  do {
      b1 <- evalExp boolExp1;
      b2 <- evalExp boolExp2;
      return (b1 && b2)
     }
evalExp (Or boolExp1 boolExp2)  = 
  do {
      b1 <- evalExp boolExp1;
      b2 <- evalExp boolExp2;
      return (b1 || b2)
     }
evalExp (Not boolExp)           = 
  do {
      b <- evalExp boolExp;
      return (not b)
     }
