module Eval3
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

-- Ejercicio 3.a: Proponer una nueva m\'onada que  
-- lleve una traza de ejecución (además de manejar errores y estado).
-- y dar su instancia de mónada. Llamarla |StateErrorTrace|. 

newtype StateErrorTrace a = 
  StateErrorTrace { runStateErrorTrace :: Env -> Pair Trace (Either Error (Pair a Env)) }

instance Monad StateErrorTrace where
    return x = StateErrorTrace (\s -> ("" :!: Right (x :!: s)))
    m >>= f = 
      StateErrorTrace $ \s ->
        case runStateErrorTrace m s of
            t :!: Left e -> 
                t :!: Left e
            t :!: Right (x :!: s') ->
                case runStateErrorTrace (f x) s' of
                    t' :!: Left e' -> 
                        (t' ++ t) :!: Left e'
                    t' :!: Right (x' :!: s'') -> 
                        (t' ++ t) :!: Right (x' :!: s'')
                                              
-- Recuerde agregar las siguientes instancias para calmar al GHC:
instance Functor StateErrorTrace where
  fmap = liftM

instance Applicative StateErrorTrace where
  pure  = return
  (<*>) = ap

-- Ejercicio 3.b: Resolver en Monad.hs


-- Ejercicio 3.c: Dar una instancia de MonadTrace para StateErrorTrace.
instance MonadTrace StateErrorTrace where
  putTrace t = StateErrorTrace $ \s-> (t :!: Right (() :!: s))

-- Ejercicio 3.d: Dar una instancia de MonadError para StateErrorTrace.
instance MonadError StateErrorTrace where
  throw = (\e -> StateErrorTrace (\s -> ("Throw " ++ (show e)) :!: (Left e)))

-- Ejercicio 3.e: Dar una instancia de MonadState para StateErrorTrace.
varToString :: Variable -> String
varToString v = v
instance MonadState StateErrorTrace where
  lookfor v = StateErrorTrace $ \s ->
                (("Lookfor " ++ (varToString v)) :!: 
                 case M.lookup v s of
                   Nothing -> Left UndefVar
                   Just n -> Right (n :!: s))

  update v i = StateErrorTrace $ \s -> 
                 (("update " ++ varToString v ++ show i) :!:
                  (Right (() :!: (M.insert v i s))))
                                        

-- Ejercicio 3.f: Implementar el evaluador utilizando la monada StateErrorTrace.
-- Evalua un programa en el estado nulo

eval :: Comm -> Either Error (Env, Trace)
eval p = case runStateErrorTrace (stepCommStar p) initEnv of
           (_ :!: Left e) -> Left e
           (t :!: Right (_ :!: s)) -> Right (s, t)

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
-- stepCommStar :: [dar el tipo segun corresponda]
stepCommStar :: (MonadTrace m, MonadError m, MonadState m) => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
-- stepComm :: [dar el tipo segun corresponda]
stepComm :: (MonadTrace m, MonadError m, MonadState m) => Comm -> m Comm
stepComm = undefined

-- Evalua una expresion 
-- evalIntExp :: [dar el tipo segun corresponda]
evalExp :: (MonadTrace m, MonadError m, MonadState m) => Exp a -> m a
evalExp = undefined
