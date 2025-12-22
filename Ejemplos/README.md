# Tests para LIS (Lenguaje Imperativo Simple)

Este directorio contiene casos de test organizados para el trabajo práctico de Análisis de Lenguajes de Programación.

## Organización de los tests:

- **test001-test014**: Expresiones aritméticas básicas y precedencia
- **test015-test022**: Expresiones booleanas y precedencia
- **test023-test027**: Comandos básicos (skip, asignación, composición)
- **test028-test033**: Estructuras if-then-else
- **test034-test037**: Bucles repeat-until
- **test038-test043**: Operador ++ (extensión)
- **test044-test050**: Comando case (extensión)
- **test051-test058**: Manejo de errores (división por cero, variables no definidas)
- **test059-test065**: Programas complejos y combinados
- **test066-test073**: Casos para el parser (espacios, comentarios, casos límite)
- **test074-test076**: Precedencia y asociatividad completa
- **test077-test080**: Casos de regresión
- **test081-test085**: Errores sintácticos (para probar manejo de errores del parser)

## Casos especiales:
- Los tests que incluyan el operando >= o <= (como es el caso de 061) deberian dar error ya que no son operadores válidos en nuestro lenguaje.
- Los tests 075 y 080 deben dar error en el parseo ya que nuestra gramática no admite variables booleanas
- Los tests 051-058 deberían generar errores en tiempo de ejecución
- Los tests 081-085 deberían generar errores de parsing
- Los tests con operador ++ pueden tener comportamientos sutiles según el orden de evaluación
- Los tests de case son azúcar sintáctico para if-then-else anidados

## Resultados esperados:

Cada archivo incluye comentarios que indican qué debería hacer el programa o qué resultado se espera.
