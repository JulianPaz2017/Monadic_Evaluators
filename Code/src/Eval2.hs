module Eval2
  ( eval
  , Env
  )
where

import           AST
import           Monads
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Data.Strict.Tuple
import           Control.Monad                  ( liftM
                                                , ap
                                                )

-- Entornos
type Env = M.Map Variable Int

-- Entorno nulo
initEnv :: Env
initEnv = M.empty

-- Mónada estado, con manejo de errores
newtype StateError a =
  StateError { runStateError :: Env -> Either Error ( Pair a Env) }


-- Para calmar al GHC
instance Functor StateError where
  fmap = liftM

instance Applicative StateError where
  pure  = return
  (<*>) = ap

-- Ejercicio 2.a: Dar una instancia de Monad para StateError:
instance Monad StateError where
  return x = StateError (\s -> Right (x :!: s)) 
  m >>= f = StateError (\s -> case (runStateError m) s of
                                Left e           -> Left e
                                Right (x :!: s') -> runStateError (f x) s')

-- Ejercicio 2.b: Dar una instancia de MonadError para StateError:
instance MonadError StateError where
  throw = (\e -> StateError (\_ -> Left e))

-- Ejercicio 2.c: Dar una instancia de MonadState para StateError:
instance MonadState StateError where
  lookfor v = StateError (\s -> case M.lookup v s of
                                  Nothing -> Left UndefVar
                                  Just n  -> Right (n :!: s))
  update v i = StateError (\s -> Right (() :!: (M.insert v i s)))

-- Ejercicio 2.d: Implementar el evaluador utilizando la monada StateError.
-- Evalua un programa en el estado nulo
eval :: Comm -> Either Error Env
eval p = case runStateError (stepCommStar p) initEnv of
           Left e          -> Left e
           Right (_ :!: s) -> Right s

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: (MonadState m, MonadError m) => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: (MonadState m, MonadError m) => Comm -> m Comm
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
evalExp :: (MonadState m, MonadError m) => Exp a -> m a
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
      if e2 == 0 then throw DivByZero else return (e1 `div` e2)
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

