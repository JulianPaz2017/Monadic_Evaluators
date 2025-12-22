module ParserTests where

import Test.HUnit

import AST
import Parser
import Text.Parsec (ParseError)

-- | Tests cases templates

okTest :: FilePath -> String -> String -> Either ParseError Comm -> Test
okTest filePath cont msg expected =
  TestCase $ assertEqual msg expected (parseComm filePath cont)

errorTest :: FilePath -> String -> String -> Test
errorTest filePath cont msg =
  TestCase $ assertBool msg (isLeft (parseComm filePath cont))
  where
    isLeft (Left _) = True
    isLeft (Right _) = False

-- | Tests cases definition

-- Casos originales
testSkip :: FilePath -> String -> Test
testSkip filePath cont = okTest filePath cont "Error on skip" (Right Skip)

testSqrt :: FilePath -> String -> Test
testSqrt filePath cont = okTest filePath cont msg (Right prog)
  where
    msg = "Error on parser for sqrt program"
    prog = Seq (Seq (Let "n" (Const 25)) 
                    (Let "i" (UMinus (Const 1)))) 
               (RepeatUntil (Seq (Let "i" (Plus (Var "i") (Const 1))) (Let "t" (Times (Var "i") (Var "i")))) 
                            (Or (Gt (Var "t") (Var "n")) (Eq (Var "t") (Var "n"))))

-- Expresiones aritméticas básicas
testConstante :: FilePath -> String -> Test
testConstante filePath cont = okTest filePath cont "Error on constante" (Right expected)
  where
    expected = Let "x" (Const 42)

testVariablesSimples :: FilePath -> String -> Test
testVariablesSimples filePath cont = okTest filePath cont "Error on variables simples" (Right expected)
  where
    expected = Seq (Let "x" (Const 5)) (Let "y" (Var "x"))

testSuma :: FilePath -> String -> Test
testSuma filePath cont = okTest filePath cont "Error on suma" (Right expected)
  where
    expected = Let "x" (Plus (Const 3) (Const 4))

testResta :: FilePath -> String -> Test
testResta filePath cont = okTest filePath cont "Error on resta" (Right expected)
  where
    expected = Let "x" (Minus (Const 10) (Const 3))

testMultiplicacion :: FilePath -> String -> Test
testMultiplicacion filePath cont = okTest filePath cont "Error on multiplicacion" (Right expected)
  where
    expected = Let "x" (Times (Const 6) (Const 7))

testDivision :: FilePath -> String -> Test
testDivision filePath cont = okTest filePath cont "Error on division" (Right expected)
  where
    expected = Let "x" (Div (Const 15) (Const 3))

testMenosUnario :: FilePath -> String -> Test
testMenosUnario filePath cont = okTest filePath cont "Error on menos unario" (Right expected)
  where
    expected = Seq (Let "x" (UMinus (Const 5))) (Let "y" (UMinus (Var "x")))

-- Precedencia y asociatividad
testPrecedenciaMultSuma :: FilePath -> String -> Test
testPrecedenciaMultSuma filePath cont = okTest filePath cont "Error on precedencia mult suma" (Right expected)
  where
    expected = Let "x" (Plus (Const 2) (Times (Const 3) (Const 4)))

testPrecedenciaDivResta :: FilePath -> String -> Test
testPrecedenciaDivResta filePath cont = okTest filePath cont "Error on precedencia div resta" (Right expected)
  where
    expected = Let "x" (Minus (Const 10) (Div (Const 8) (Const 2)))

testAsociatividadSuma :: FilePath -> String -> Test
testAsociatividadSuma filePath cont = okTest filePath cont "Error on asociatividad suma" (Right expected)
  where
    expected = Let "x" (Minus (Minus (Const 10) (Const 3)) (Const 2))

testAsociatividadMult :: FilePath -> String -> Test
testAsociatividadMult filePath cont = okTest filePath cont "Error on asociatividad mult" (Right expected)
  where
    expected = Let "x" (Div (Div (Const 8) (Const 4)) (Const 2))

testParentesisPrecedencia :: FilePath -> String -> Test
testParentesisPrecedencia filePath cont = okTest filePath cont "Error on parentesis precedencia" (Right expected)
  where
    expected = Let "x" (Times (Plus (Const 2) (Const 3)) (Const 4))

testMenosUnarioPrecedencia :: FilePath -> String -> Test
testMenosUnarioPrecedencia filePath cont = okTest filePath cont "Error on menos unario precedencia" (Right expected)
  where
    expected = Let "x" (Plus (UMinus (Const 2)) (Const 3))

testExpresionCompleja :: FilePath -> String -> Test
testExpresionCompleja filePath cont = okTest filePath cont "Error on expresion compleja" (Right expected)
  where
    expected = Let "x" (Minus (Times (UMinus (Plus (Const 3) (Const 4))) (Const 2)) (Const 1))

-- Expresiones booleanas
testConstantesBool :: FilePath -> String -> Test
testConstantesBool filePath cont = okTest filePath cont "Error on constantes bool" (Right expected)
  where
    expected = Seq (IfThenElse BTrue (Let "x" (Const 1)) Skip)
                   (IfThenElse BFalse (Let "x" (Const 2)) Skip)

testNegacion :: FilePath -> String -> Test
testNegacion filePath cont = okTest filePath cont "Error on negacion" (Right expected)
  where
    expected = Seq (Let "x" (Const 5))
                   (IfThenElse (Not (Eq (Var "x") (Const 3))) (Let "y" (Const 1)) Skip)

testAndLogico :: FilePath -> String -> Test
testAndLogico filePath cont = okTest filePath cont "Error on and logico" (Right expected)
  where
    expected = Seq (Let "x" (Const 5))
                   (IfThenElse (And (Gt (Var "x") (Const 2)) (Lt (Var "x") (Const 10))) (Let "y" (Const 1)) Skip)

testOrLogico :: FilePath -> String -> Test
testOrLogico filePath cont = okTest filePath cont "Error on or logico" (Right expected)
  where
    expected = Seq (Let "x" (Const 1))
                   (IfThenElse (Or (Lt (Var "x") (Const 0)) (Gt (Var "x") (Const 0))) (Let "y" (Const 1)) Skip)

-- Precedencia booleana
testPrecedenciaNotAnd :: FilePath -> String -> Test
testPrecedenciaNotAnd filePath cont = okTest filePath cont "Error on precedencia not and" (Right expected)
  where
    expected = Seq (Let "x" (Const 5))
                   (IfThenElse (And (Not BFalse) BTrue) (Let "y" (Const 1)) Skip)

testPrecedenciaAndOr :: FilePath -> String -> Test
testPrecedenciaAndOr filePath cont = okTest filePath cont "Error on precedencia and or" (Right expected)
  where
    expected = IfThenElse (Or BFalse (And BTrue BFalse)) (Let "y" (Const 1)) Skip

testParentesisBool :: FilePath -> String -> Test
testParentesisBool filePath cont = okTest filePath cont "Error on parentesis bool" (Right expected)
  where
    expected = IfThenElse (And (Or BFalse BTrue) BFalse) (Let "y" (Const 1)) (Let "y" (Const 2))

-- Comandos básicos
testAsignacionMultiple :: FilePath -> String -> Test
testAsignacionMultiple filePath cont = okTest filePath cont "Error on asignacion multiple" (Right expected)
  where
    expected = Seq (Seq (Let "x" (Const 1)) (Let "y" (Const 2))) (Let "z" (Const 3))

testAsignacionExpresiones :: FilePath -> String -> Test
testAsignacionExpresiones filePath cont = okTest filePath cont "Error on asignacion expresiones" (Right expected)
  where
    expected = Seq (Seq (Let "x" (Const 5)) (Let "y" (Plus (Var "x") (Const 3)))) 
                   (Let "z" (Times (Var "y") (Const 2)))

testSecuenciaSimple :: FilePath -> String -> Test
testSecuenciaSimple filePath cont = okTest filePath cont "Error on secuencia simple" (Right expected)
  where
    expected = Seq (Seq (Let "x" (Const 1)) (Let "x" (Plus (Var "x") (Const 1))))
                   (Let "x" (Times (Var "x") (Const 2)))

-- If-then-else
testIfSimpleTrue :: FilePath -> String -> Test
testIfSimpleTrue filePath cont = okTest filePath cont "Error on if simple true" (Right expected)
  where
    expected = Seq (Let "x" (Const 5))
                   (IfThenElse (Eq (Var "x") (Const 5)) (Let "y" (Const 10)) Skip)

testIfSimpleFalse :: FilePath -> String -> Test
testIfSimpleFalse filePath cont = okTest filePath cont "Error on if simple false" (Right expected)
  where
    expected = Seq (Let "x" (Const 3))
                   (IfThenElse (Eq (Var "x") (Const 5)) (Let "y" (Const 10)) Skip)

testIfElseThen :: FilePath -> String -> Test
testIfElseThen filePath cont = okTest filePath cont "Error on if else then" (Right expected)
  where
    expected = Seq (Let "x" (Const 5))
                   (IfThenElse (Eq (Var "x") (Const 5)) (Let "y" (Const 1)) (Let "y" (Const 2)))

testIfElseElse :: FilePath -> String -> Test
testIfElseElse filePath cont = okTest filePath cont "Error on if else else" (Right expected)
  where
    expected = Seq (Let "x" (Const 3))
                   (IfThenElse (Eq (Var "x") (Const 5)) (Let "y" (Const 1)) (Let "y" (Const 2)))

testIfAnidados :: FilePath -> String -> Test
testIfAnidados filePath cont = okTest filePath cont "Error on if anidados" (Right expected)
  where
    expected = Seq (Seq (Let "x" (Const 5)) (Let "y" (Const 3)))
                   (IfThenElse (Gt (Var "x") (Const 0))
                               (IfThenElse (Gt (Var "y") (Const 0))
                                           (Let "z" (Plus (Var "x") (Var "y")))
                                           (Let "z" (Minus (Var "x") (Var "y"))))
                               (Let "z" (Const 0)))

-- Repeat-until
testRepeatSimple :: FilePath -> String -> Test
testRepeatSimple filePath cont = okTest filePath cont "Error on repeat simple" (Right expected)
  where
    expected = Seq (Let "x" (Const 0))
                   (RepeatUntil (Let "x" (Plus (Var "x") (Const 1))) 
                                (Eq (Var "x") (Const 3)))

testRepeatNoEntra :: FilePath -> String -> Test
testRepeatNoEntra filePath cont = okTest filePath cont "Error on repeat no entra" (Right expected)
  where
    expected = Seq (Let "x" (Const 5))
                   (RepeatUntil (Let "x" (Plus (Var "x") (Const 1))) 
                                (Gt (Var "x") (Const 4)))

testRepeatMultiplesVars :: FilePath -> String -> Test
testRepeatMultiplesVars filePath cont = okTest filePath cont "Error on repeat multiples vars" (Right expected)
  where
    expected = Seq (Seq (Let "x" (Const 0)) (Let "y" (Const 10)))
                   (RepeatUntil (Seq (Let "x" (Plus (Var "x") (Const 1)))
                                     (Let "y" (Minus (Var "y") (Const 1))))
                                (Gt (Var "x") (Var "y")))

-- Operador ++
testIncrementoSimple :: FilePath -> String -> Test
testIncrementoSimple filePath cont = okTest filePath cont "Error on incremento simple" (Right expected)
  where
    expected = Seq (Let "x" (Const 5)) (Let "y" (VarInc "x"))

testIncrementoExpresion :: FilePath -> String -> Test
testIncrementoExpresion filePath cont = okTest filePath cont "Error on incremento expresion" (Right expected)
  where
    expected = Seq (Let "x" (Const 10)) (Let "y" (Plus (VarInc "x") (Const 5)))

testIncrementoIf :: FilePath -> String -> Test
testIncrementoIf filePath cont = okTest filePath cont "Error on incremento if" (Right expected)
  where
    expected = Seq (Let "x" (Const 4))
                   (IfThenElse (Eq (VarInc "x") (Const 4)) (Let "y" (Const 1)) (Let "y" (Const 2)))

testIncrementoRepeat :: FilePath -> String -> Test
testIncrementoRepeat filePath cont = okTest filePath cont "Error on incremento repeat" (Right expected)
  where
    expected = Seq (Seq (Let "x" (Const 0)) (Let "sum" (Const 0)))
                   (RepeatUntil (Let "sum" (Plus (Var "sum") (VarInc "x")))
                                (Eq (Var "x") (Const 5)))

-- Case
testCasePrimeraCondicion :: FilePath -> String -> Test
testCasePrimeraCondicion filePath cont = okTest filePath cont "Error on case primera condicion" (Right expected)
  where
    expected = Seq (Let "x" (Const 5))
                   (Case [(Eq (Var "x") (Const 5), Let "y" (Const 10)),
                          (Lt (Var "x") (Const 7), Let "y" (Const 100)),
                          (Gt (Var "x") (Const 10), Let "y" (Const 1000))])

testCaseSegundaCondicion :: FilePath -> String -> Test
testCaseSegundaCondicion filePath cont = okTest filePath cont "Error on case segunda condicion" (Right expected)
  where
    expected = Seq (Let "x" (Const 6))
                   (Case [(Eq (Var "x") (Const 5), Let "y" (Const 10)),
                          (Lt (Var "x") (Const 7), Let "y" (Const 100)),
                          (Gt (Var "x") (Const 10), Let "y" (Const 1000))])

testCaseTerceraCondicion :: FilePath -> String -> Test
testCaseTerceraCondicion filePath cont = okTest filePath cont "Error on case tercera condicion" (Right expected)
  where
    expected = Seq (Let "x" (Const 15))
                   (Case [(Eq (Var "x") (Const 5), Let "y" (Const 10)),
                          (Lt (Var "x") (Const 7), Let "y" (Const 100)),
                          (Gt (Var "x") (Const 10), Let "y" (Const 1000))])

testCaseSinCondiciones :: FilePath -> String -> Test
testCaseSinCondiciones filePath cont = okTest filePath cont "Error on case sin condiciones" (Right expected)
  where
    expected = Seq (Seq (Let "x" (Const 8)) (Let "y" (Const 0)))
                   (Case [(Eq (Var "x") (Const 5), Let "y" (Const 10)),
                          (Lt (Var "x") (Const 7), Let "y" (Const 100)),
                          (Gt (Var "x") (Const 10), Let "y" (Const 1000))])

testCaseVacio :: FilePath -> String -> Test
testCaseVacio filePath cont = okTest filePath cont "Error on case vacio" (Right expected)
  where
    expected = Seq (Seq (Let "x" (Const 5)) (Let "y" (Const 0)))
                   (Case [])

testCaseEfectosSecundarios :: FilePath -> String -> Test
testCaseEfectosSecundarios filePath cont = okTest filePath cont "Error on case efectos secundarios" (Right expected)
  where
    expected = Seq (Seq (Let "x" (Const 4)) (Let "y" (Const 0)))
                   (Case [(Eq (VarInc "x") (Const 4), Let "y" (Var "x")),
                          (Gt (Var "x") (Const 10), Let "y" (Const 1000))])

testCaseAnidados :: FilePath -> String -> Test
testCaseAnidados filePath cont = okTest filePath cont "Error on case anidados" (Right expected)
  where
    expected = Seq (Seq (Let "x" (Const 5)) (Let "y" (Const 3)))
                   (Case [(Gt (Var "x") (Const 0), 
                           Case [(Eq (Var "y") (Const 3), Let "z" (Const 100)),
                                 (Eq (Var "y") (Const 4), Let "z" (Const 200))]),
                          (Lt (Var "x") (Const 0), Let "z" (Const 0))])

-- Programas complejos
testFactorial :: FilePath -> String -> Test
testFactorial filePath cont = okTest filePath cont "Error on factorial" (Right expected)
  where
    expected = Seq (Seq (Seq (Let "n" (Const 5)) (Let "fact" (Const 1))) (Let "i" (Const 1)))
                   (RepeatUntil (Seq (Let "fact" (Times (Var "fact") (Var "i")))
                                     (Let "i" (Plus (Var "i") (Const 1))))
                                (Gt (Var "i") (Var "n")))

-- Casos de regresión
testRepeatUnaVez :: FilePath -> String -> Test
testRepeatUnaVez filePath cont = okTest filePath cont "Error on repeat una vez" (Right expected)
  where
    expected = Seq (Let "x" (Const 0))
                   (RepeatUntil (Let "x" (Const 1)) BTrue)

testAsignacionCircular :: FilePath -> String -> Test
testAsignacionCircular filePath cont = okTest filePath cont "Error on asignacion circular" (Right expected)
  where
    expected = Seq (Seq (Seq (Seq (Let "x" (Const 1)) (Let "y" (Const 2)))
                               (Let "temp" (Var "x")))
                            (Let "x" (Var "y")))
                       (Let "y" (Var "temp"))

-- Casos de parser con espacios y comentarios
testEspaciosVariados :: FilePath -> String -> Test
testEspaciosVariados filePath cont = okTest filePath cont "Error on espacios variados" (Right expected)
  where
    expected = Seq (Let "x" (Plus (Const 5) (Const 3)))
                   (Let "y" (Times (Var "x") (Const 2)))

testComentariosLinea :: FilePath -> String -> Test
testComentariosLinea filePath cont = okTest filePath cont "Error on comentarios linea" (Right expected)
  where
    expected = Seq (Let "x" (Const 5))
                   (Let "z" (Plus (Var "x") (Const 1)))

testComentariosBloque :: FilePath -> String -> Test
testComentariosBloque filePath cont = okTest filePath cont "Error on comentarios bloque" (Right expected)
  where
    expected = Seq (Let "x" (Const 5))
                   (Let "y" (Const 10))

-- Casos límite
testParentesisAnidados :: FilePath -> String -> Test
testParentesisAnidados filePath cont = okTest filePath cont "Error on parentesis anidados" (Right expected)
  where
    expected = Let "x" (Times (Plus (Div (Minus (Times (Plus (Const 1) (Const 2)) (Const 3)) (Const 4)) (Const 5)) (Const 6)) (Const 7))

testNombresLargos :: FilePath -> String -> Test
testNombresLargos filePath cont = okTest filePath cont "Error on nombres largos" (Right expected)
  where
    expected = Seq (Let "variableConNombreMuyLargo" (Const 42))
                   (Let "otraVariableLarga" (Plus (Var "variableConNombreMuyLargo") (Const 1)))

testNumerosGrandes :: FilePath -> String -> Test
testNumerosGrandes filePath cont = okTest filePath cont "Error on numeros grandes" (Right expected)
  where
    expected = Seq (Let "x" (Const 999999999))
                   (Let "y" (Plus (Var "x") (Const 1)))

-- Casos de error sintáctico
testParentesisNoBalanceados :: FilePath -> String -> Test
testParentesisNoBalanceados filePath cont = errorTest filePath cont "Should fail on unbalanced parentheses"

testOperadorIncompleto :: FilePath -> String -> Test
testOperadorIncompleto filePath cont = errorTest filePath cont "Should fail on incomplete operator"

testPalabraClaveMalEscrita :: FilePath -> String -> Test
testPalabraClaveMalEscrita filePath cont = errorTest filePath cont "Should fail on misspelled keyword"

testLlavesNoBalanceadas :: FilePath -> String -> Test
testLlavesNoBalanceadas filePath cont = errorTest filePath cont "Should fail on unbalanced braces"

-- | Tests cases - Lista completa

tests :: [(FilePath -> String -> Test, FilePath)]
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
    , (testNegacion, "ejemplos/test017_negacion.lis")
    , (testAndLogico, "ejemplos/test018_and_logico.lis")
    , (testOrLogico, "ejemplos/test019_or_logico.lis")
    , (testPrecedenciaNotAnd, "ejemplos/test020_precedencia_not_and.lis")
    , (testPrecedenciaAndOr, "ejemplos/test021_precedencia_and_or.lis")
    , (testParentesisBool, "ejemplos/test022_parentesis_bool.lis")
    
    -- Comandos básicos
    , (testAsignacionMultiple, "ejemplos/test024_asignacion_multiple.lis")
    , (testAsignacionExpresiones, "ejemplos/test025_asignacion_expresiones.lis")
    , (testSecuenciaSimple, "ejemplos/test026_secuencia_simple.lis")
    
    -- If-then-else
    , (testIfSimpleTrue, "ejemplos/test028_if_simple_true.lis")
    , (testIfSimpleFalse, "ejemplos/test029_if_simple_false.lis")
    , (testIfElseThen, "ejemplos/test030_if_else_then.lis")
    , (testIfElseElse, "ejemplos/test031_if_else_else.lis")
    , (testIfAnidados, "ejemplos/test032_if_anidados.lis")
    
    -- Repeat-until
    , (testRepeatSimple, "ejemplos/test034_repeat_simple.lis")
    , (testRepeatNoEntra, "ejemplos/test035_repeat_no_entra.lis")
    , (testRepeatMultiplesVars, "ejemplos/test036_repeat_multiples_vars.lis")

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
    -- Casos de regresión
    , (testRepeatUnaVez, "ejemplos/test077_repeat_una_vez.lis")
    , (testAsignacionCircular, "ejemplos/test079_asignacion_circular.lis")
    
    -- Parser con espacios y comentarios
    , (testEspaciosVariados, "ejemplos/test066_espacios_variados.lis")
    , (testComentariosLinea, "ejemplos/test067_comentarios_linea.lis")
    , (testComentariosBloque, "ejemplos/test068_comentarios_bloque.lis")
    
    -- Casos límite
    , (testParentesisAnidados, "ejemplos/test070_parentesis_anidados.lis")
    , (testNombresLargos, "ejemplos/test071_nombres_largos.lis")
    , (testNumerosGrandes, "ejemplos/test072_numeros_grandes.lis")
    
    -- Casos de error sintáctico
    , (testParentesisNoBalanceados, "ejemplos/test081_parentesis_no_balanceados.lis")
    , (testOperadorIncompleto, "ejemplos/test082_operador_incompleto.lis")
    , (testPalabraClaveMalEscrita, "ejemplos/test083_palabra_clave_mal_escrita.lis")
    , (testLlavesNoBalanceadas, "ejemplos/test085_llaves_no_balanceadas.lis")
    ]

-- | Run tests

parserTests :: IO Counts
parserTests = do
    ts <- mapM buildTest tests
    runTestTT $ TestList ts

buildTest :: (FilePath -> String -> Test, FilePath) -> IO Test
buildTest (testSpec, fp) = do
    content <- readFile fp
    return (testSpec fp content)