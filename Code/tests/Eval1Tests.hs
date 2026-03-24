module Eval1Tests where

import Test.HUnit

import Data.Map (fromList, empty)
import Eval1 (eval)
import Parser (parseComm)
import AST (Comm)

-- | Tests cases definition

-- Comandos básicos
testSkip :: Comm -> Test
testSkip comm = TestCase $ assertEqual msg res (eval comm)
  where
    msg = "Error on skip"
    res = empty

testSqrt :: Comm -> Test
testSqrt comm = TestCase $ assertEqual msg res (eval comm)
  where
    msg = "Error on sqrt computation"
    res = fromList [("i",5),("n",25),("t",25)]

-- Expresiones aritméticas básicas
testConstante :: Comm -> Test
testConstante comm = TestCase $ assertEqual msg res (eval comm)
  where
    msg = "Error on constante"
    res = fromList [("x", 42)]

testVariablesSimples :: Comm -> Test
testVariablesSimples comm = TestCase $ assertEqual msg res (eval comm)
  where
    msg = "Error on variables simples"
    res = fromList [("x", 5), ("y", 5)]

testSuma :: Comm -> Test
testSuma comm = TestCase $ assertEqual msg res (eval comm)
  where
    msg = "Error on suma"
    res = fromList [("x", 7)]

testResta :: Comm -> Test
testResta comm = TestCase $ assertEqual msg res (eval comm)
  where
    msg = "Error on resta"
    res = fromList [("x", 7)]

testMultiplicacion :: Comm -> Test
testMultiplicacion comm = TestCase $ assertEqual msg res (eval comm)
  where
    msg = "Error on multiplicacion"
    res = fromList [("x", 42)]

testDivision :: Comm -> Test
testDivision comm = TestCase $ assertEqual msg res (eval comm)
  where
    msg = "Error on division"
    res = fromList [("x", 5)]

testMenosUnario :: Comm -> Test
testMenosUnario comm = TestCase $ assertEqual msg res (eval comm)
  where
    msg = "Error on menos unario"
    res = fromList [("x", -5), ("y", 5)]

-- Precedencia y asociatividad
testPrecedenciaMultSuma :: Comm -> Test
testPrecedenciaMultSuma comm = TestCase $ assertEqual msg res (eval comm)
  where
    msg = "Error on precedencia mult suma"
    res = fromList [("x", 14)] -- 2 + 3 * 4 = 2 + 12 = 14

testPrecedenciaDivResta :: Comm -> Test
testPrecedenciaDivResta comm = TestCase $ assertEqual msg res (eval comm)
  where
    msg = "Error on precedencia div resta"
    res = fromList [("x", 6)] -- 10 - 8 / 2 = 10 - 4 = 6

testAsociatividadSuma :: Comm -> Test
testAsociatividadSuma comm = TestCase $ assertEqual msg res (eval comm)
  where
    msg = "Error on asociatividad suma"
    res = fromList [("x", 5)] -- 10 - 3 - 2 = 7 - 2 = 5

testAsociatividadMult :: Comm -> Test
testAsociatividadMult comm = TestCase $ assertEqual msg res (eval comm)
  where
    msg = "Error on asociatividad mult"
    res = fromList [("x", 1)] -- 8 / 4 / 2 = 2 / 2 = 1

testParentesisPrecedencia :: Comm -> Test
testParentesisPrecedencia comm = TestCase $ assertEqual msg res (eval comm)
  where
    msg = "Error on parentesis precedencia"
    res = fromList [("x", 20)] -- (2 + 3) * 4 = 5 * 4 = 20

testMenosUnarioPrecedencia :: Comm -> Test
testMenosUnarioPrecedencia comm = TestCase $ assertEqual msg res (eval comm)
  where
    msg = "Error on menos unario precedencia"
    res = fromList [("x", 1)] -- -2 + 3 = 1

testExpresionCompleja :: Comm -> Test
testExpresionCompleja comm = TestCase $ assertEqual msg res (eval comm)
  where
    msg = "Error on expresion compleja"
    res = fromList [("x", -15)] -- -(3 + 4) * 2 - 1 = -7 * 2 - 1 = -14 - 1 = -15

-- Expresiones booleanas
testConstantesBool :: Comm -> Test
testConstantesBool comm = TestCase $ assertEqual msg res (eval comm)
  where
    msg = "Error on constantes bool"
    res = fromList [("x", 1)] -- Solo se ejecuta if true

testComparaciones :: Comm -> Test
testComparaciones comm = TestCase $ assertEqual msg res (eval comm)
  where
    msg = "Error on comparaciones"
    res = fromList [("x", 5), ("y", 4)] -- Todas las condiciones se cumplen

testNegacion :: Comm -> Test
testNegacion comm = TestCase $ assertEqual msg res (eval comm)
  where
    msg = "Error on negacion"
    res = fromList [("x", 5), ("y", 1)] -- !(x == 3) es true

testAndLogico :: Comm -> Test
testAndLogico comm = TestCase $ assertEqual msg res (eval comm)
  where
    msg = "Error on and logico"
    res = fromList [("x", 5), ("y", 1)] -- x > 2 && x < 10 es true

testOrLogico :: Comm -> Test
testOrLogico comm = TestCase $ assertEqual msg res (eval comm)
  where
    msg = "Error on or logico"
    res = fromList [("x", 1), ("y", 1)] -- x < 0 || x > 0 es true

-- Asignación múltiple
testAsignacionMultiple :: Comm -> Test
testAsignacionMultiple comm = TestCase $ assertEqual msg res (eval comm)
  where
    msg = "Error on asignacion multiple"
    res = fromList [("x", 1), ("y", 2), ("z", 3)]

testAsignacionExpresiones :: Comm -> Test
testAsignacionExpresiones comm = TestCase $ assertEqual msg res (eval comm)
  where
    msg = "Error on asignacion expresiones"
    res = fromList [("x", 5), ("y", 8), ("z", 16)]

-- Secuencias
testSecuenciaSimple :: Comm -> Test
testSecuenciaSimple comm = TestCase $ assertEqual msg res (eval comm)
  where
    msg = "Error on secuencia simple"
    res = fromList [("x", 4)] -- ((1 + 1) * 2) = 4

testSecuenciasAnidadas :: Comm -> Test
testSecuenciasAnidadas comm = TestCase $ assertEqual msg res (eval comm)
  where
    msg = "Error on secuencias anidadas"
    res = fromList [("x", 1), ("y", 2), ("z", 3)]

-- If-then-else
testIfSimpleTrue :: Comm -> Test
testIfSimpleTrue comm = TestCase $ assertEqual msg res (eval comm)
  where
    msg = "Error on if simple true"
    res = fromList [("x", 5), ("y", 10)]

testIfSimpleFalse :: Comm -> Test
testIfSimpleFalse comm = TestCase $ assertEqual msg res (eval comm)
  where
    msg = "Error on if simple false"
    res = fromList [("x", 3)] -- y no se asigna

testIfElseThen :: Comm -> Test
testIfElseThen comm = TestCase $ assertEqual msg res (eval comm)
  where
    msg = "Error on if else then"
    res = fromList [("x", 5), ("y", 1)]

testIfElseElse :: Comm -> Test
testIfElseElse comm = TestCase $ assertEqual msg res (eval comm)
  where
    msg = "Error on if else else"
    res = fromList [("x", 3), ("y", 2)]

testIfAnidados :: Comm -> Test
testIfAnidados comm = TestCase $ assertEqual msg res (eval comm)
  where
    msg = "Error on if anidados"
    res = fromList [("x", 5), ("y", 3), ("z", 8)]

testIfElseIf :: Comm -> Test
testIfElseIf comm = TestCase $ assertEqual msg res (eval comm)
  where
    msg = "Error on if else if"
    res = fromList [("x", 7), ("y", 2)] -- 7 < 10, entonces y = 2

-- Repeat-until
testRepeatSimple :: Comm -> Test
testRepeatSimple comm = TestCase $ assertEqual msg res (eval comm)
  where
    msg = "Error on repeat simple"
    res = fromList [("x", 3)]

testRepeatNoEntra :: Comm -> Test
testRepeatNoEntra comm = TestCase $ assertEqual msg res (eval comm)
  where
    msg = "Error on repeat no entra"
    res = fromList [("x", 6)] -- Se ejecuta una vez: 5 + 1 = 6

testRepeatMultiplesVars :: Comm -> Test
testRepeatMultiplesVars comm = TestCase $ assertEqual msg res (eval comm)
  where
    msg = "Error on repeat multiples vars"
    res = fromList [("x", 6), ("y", 4)] -- Se encuentran en x=5, y=5

testRepeatAnidados :: Comm -> Test
testRepeatAnidados comm = TestCase $ assertEqual msg res (eval comm)
  where
    msg = "Error on repeat anidados"
    res = fromList [("x", 3), ("y", 2)]

-- Operador ++
testIncrementoSimple :: Comm -> Test
testIncrementoSimple comm = TestCase $ assertEqual msg res (eval comm)
  where
    msg = "Error on incremento simple"
    res = fromList [("x", 6), ("y", 6)]

testIncrementoExpresion :: Comm -> Test
testIncrementoExpresion comm = TestCase $ assertEqual msg res (eval comm)
  where
    msg = "Error on incremento expresion"
    res = fromList [("x", 11), ("y", 16)]

testIncrementoIf :: Comm -> Test
testIncrementoIf comm = TestCase $ assertEqual msg res (eval comm)
  where
    msg = "Error on incremento if"
    res = fromList [("x", 5), ("y", 2)]

testIncrementoRepeat :: Comm -> Test
testIncrementoRepeat comm = TestCase $ assertEqual msg res (eval comm)
  where
    msg = "Error on incremento repeat"
    res = fromList [("x", 5), ("sum", 15)]

-- Case
testCasePrimeraCondicion :: Comm -> Test
testCasePrimeraCondicion comm = TestCase $ assertEqual msg res (eval comm)
  where
    msg = "Error on case primera condicion"
    res = fromList [("x", 5), ("y", 10)]

testCaseSegundaCondicion :: Comm -> Test
testCaseSegundaCondicion comm = TestCase $ assertEqual msg res (eval comm)
  where
    msg = "Error on case segunda condicion"
    res = fromList [("x", 6), ("y", 100)]

testCaseTerceraCondicion :: Comm -> Test
testCaseTerceraCondicion comm = TestCase $ assertEqual msg res (eval comm)
  where
    msg = "Error on case tercera condicion"
    res = fromList [("x", 15), ("y", 1000)]

testCaseSinCondiciones :: Comm -> Test
testCaseSinCondiciones comm = TestCase $ assertEqual msg res (eval comm)
  where
    msg = "Error on case sin condiciones"
    res = fromList [("x", 8), ("y", 0)] -- y se mantiene 0

testCaseVacio :: Comm -> Test
testCaseVacio comm = TestCase $ assertEqual msg res (eval comm)
  where
    msg = "Error on case vacio"
    res = fromList [("x", 5), ("y", 0)]

testCaseEfectosSecundarios :: Comm -> Test
testCaseEfectosSecundarios comm = TestCase $ assertEqual msg res (eval comm)
  where
    msg = "Error on case efectos secundarios"
    res = fromList [("x", 5), ("y", 0)]

testCaseAnidados :: Comm -> Test
testCaseAnidados comm = TestCase $ assertEqual msg res (eval comm)
  where
    msg = "Error on case anidados"
    res = fromList [("x", 5), ("y", 3), ("z", 100)]

-- Programas complejos
testFactorial :: Comm -> Test
testFactorial comm = TestCase $ assertEqual msg res (eval comm)
  where
    msg = "Error on factorial"
    res = fromList [("n", 5), ("fact", 120), ("i", 6)] -- 5! = 120

testSumaPares :: Comm -> Test
testSumaPares comm = TestCase $ assertEqual msg res (eval comm)
  where
    msg = "Error on suma pares"
    res = fromList [("n", 10), ("sum", 30), ("i", 12)] -- 2+4+6+8+10 = 30

testEuclidesMcd :: Comm -> Test
testEuclidesMcd comm = TestCase $ assertEqual msg res (eval comm)
  where
    msg = "Error on euclides mcd"
    res = fromList [("a", 6), ("b", 6), ("mcd", 6)] -- MCD(48, 18) = 6

-- Casos de regresión
testRepeatUnaVez :: Comm -> Test
testRepeatUnaVez comm = TestCase $ assertEqual msg res (eval comm)
  where
    msg = "Error on repeat una vez"
    res = fromList [("x", 1)]

testAsignacionCircular :: Comm -> Test
testAsignacionCircular comm = TestCase $ assertEqual msg res (eval comm)
  where
    msg = "Error on asignacion circular"
    res = fromList [("x", 2), ("y", 1), ("temp", 1)] -- x y y intercambiados

-- | Tests cases

tests :: [(Comm -> Test, FilePath)]
tests =
    [
    -- Casos originales
      (testSkip, "ejemplos/skip.lis")
    , (testSqrt, "ejemplos/sqrt.lis")
    
    -- Expresiones aritméticas básicas
    , (testConstante, "ejemplos/test001_constante.lis")
    , (testVariablesSimples, "ejemplos/test002_variables_simples.lis")
    , (testSuma, "ejemplos/test003_suma.lis")
    , (testResta, "ejemplos/test004_resta.lis")
    , (testMultiplicacion, "ejemplos/test005_multiplicacion.lis")
    , (testDivision, "ejemplos/test006_division.lis")
    , (testMenosUnario, "ejemplos/test007_menos_unario.lis")
    
    -- Precedencia y asociatividad
    , (testPrecedenciaMultSuma, "ejemplos/test008_precedencia_mult_suma.lis")
    , (testPrecedenciaDivResta, "ejemplos/test009_precedencia_div_resta.lis")
    , (testAsociatividadSuma, "ejemplos/test010_asociatividad_suma.lis")
    , (testAsociatividadMult, "ejemplos/test011_asociatividad_mult.lis")
    , (testParentesisPrecedencia, "ejemplos/test012_parentesis_precedencia.lis")
    , (testMenosUnarioPrecedencia, "ejemplos/test013_menos_unario_precedencia.lis")
    , (testExpresionCompleja, "ejemplos/test014_expresion_compleja.lis")
    
    -- Expresiones booleanas
    , (testConstantesBool, "ejemplos/test015_constantes_bool.lis")
    , (testComparaciones, "ejemplos/test016_comparaciones.lis")
    , (testNegacion, "ejemplos/test017_negacion.lis")
    , (testAndLogico, "ejemplos/test018_and_logico.lis")
    , (testOrLogico, "ejemplos/test019_or_logico.lis")
    
    -- Comandos básicos
    , (testAsignacionMultiple, "ejemplos/test024_asignacion_multiple.lis")
    , (testAsignacionExpresiones, "ejemplos/test025_asignacion_expresiones.lis")
    , (testSecuenciaSimple, "ejemplos/test026_secuencia_simple.lis")
    , (testSecuenciasAnidadas, "ejemplos/test027_secuencias_anidadas.lis")
    
    -- If-then-else
    , (testIfSimpleTrue, "ejemplos/test028_if_simple_true.lis")
    , (testIfSimpleFalse, "ejemplos/test029_if_simple_false.lis")
    , (testIfElseThen, "ejemplos/test030_if_else_then.lis")
    , (testIfElseElse, "ejemplos/test031_if_else_else.lis")
    , (testIfAnidados, "ejemplos/test032_if_anidados.lis")
    , (testIfElseIf, "ejemplos/test033_if_else_if.lis")
    
    -- Repeat-until
    , (testRepeatSimple, "ejemplos/test034_repeat_simple.lis")
    , (testRepeatNoEntra, "ejemplos/test035_repeat_no_entra.lis")
    , (testRepeatMultiplesVars, "ejemplos/test036_repeat_multiples_vars.lis")
    , (testRepeatAnidados, "ejemplos/test037_repeat_anidados.lis")
    
    -- Operador ++
    , (testIncrementoSimple, "ejemplos/test038_incremento_simple.lis")
    , (testIncrementoExpresion, "ejemplos/test039_incremento_expresion.lis")
    , (testIncrementoIf, "ejemplos/test041_incremento_if.lis")
    , (testIncrementoRepeat, "ejemplos/test042_incremento_repeat.lis")
    
    -- Case
    , (testCasePrimeraCondicion, "ejemplos/test044_case_primera_condicion.lis")
    , (testCaseSegundaCondicion, "ejemplos/test045_case_segunda_condicion.lis")
    , (testCaseTerceraCondicion, "ejemplos/test046_case_tercera_condicion.lis")
    , (testCaseSinCondiciones, "ejemplos/test047_case_sin_condiciones.lis")
    , (testCaseVacio, "ejemplos/test048_case_vacio.lis")
    , (testCaseEfectosSecundarios, "ejemplos/test049_case_efectos_secundarios.lis")
    , (testCaseAnidados, "ejemplos/test050_case_anidados.lis")
    
    -- Programas complejos
    , (testFactorial, "ejemplos/test059_factorial.lis")
    , (testSumaPares, "ejemplos/test060_suma_pares.lis")
    , (testEuclidesMcd, "ejemplos/test062_euclides_mcd.lis")
    
    -- Casos de regresión
    , (testRepeatUnaVez, "ejemplos/test077_repeat_una_vez.lis")
    , (testAsignacionCircular, "ejemplos/test079_asignacion_circular.lis")
    ]

-- | Run tests

eval1Tests :: IO Counts
eval1Tests = do
    ts <- mapM buildTest tests
    runTestTT $ TestList ts

buildTest :: (Comm -> Test, FilePath) -> IO Test
buildTest (testSpec, fp) = do
    content <- readFile fp
    case parseComm fp content of
        Left err   -> error ("Parse error: " ++ show err)
        Right comm -> return (testSpec comm)