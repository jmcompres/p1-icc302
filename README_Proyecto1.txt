Evaluador de Expresiones Aritméticas en Haskell

1. Introducción

Este proyecto implementa un evaluador de expresiones aritméticas utilizando programación funcional en Haskell. 
El sistema modela las expresiones como un tipo algebraico recursivo y las evalúa de manera segura usando el tipo Maybe para el manejo de errores.

2. Modelo de Datos

Se define el tipo:
         data Expr a


Este tipo permite representar:
   - Literales
   - Suma
   - Resta
   - Multiplicación
   - División
   - Potencia
   - Logaritmo

Las expresiones forman un árbol sintáctico.

Ejemplo:
      Mul (Add (Lit 2) (Lit 3)) (Pow (Lit 2) (Lit 3))

Representa:
      (2 + 3) * (2^3)

3. Evaluación Recursiva

La función:
      eval :: Expr a -> Maybe a


Evalúa la expresión de forma recursiva:
 - Evalúa las subexpresiones.
 - Combina los resultados.
 - Si ocurre un error matemático, devuelve Nothing.

4. Manejo de Errores

Se utilizan funciones auxiliares seguras:
 - sopgen → operaciones generales
 - sdiv → evita división por cero
 - slog → valida dominio del logaritmo

Errores controlados:
 - División por cero
 - Logaritmo con base ≤ 0
 - Logaritmo con base = 1
 - Logaritmo con argumento ≤ 0

 5. Interfaz de Usuario

El usuario puede ingresar expresiones en formato estructurado.

Ejemplo válido: Add (Lit 2) (Lit 3)

Si la sintaxis es incorrecta: Error de sintaxis en la expresion.

Si la operación es inválida: Operacion matematica invalida.

6. Conceptos Aplicados
   Tipos algebraicos
   Recursión estructural
   Programación funcional pura
   Manejo seguro de errores con Maybe
   Separación entre lógica pura e IO

7. Conclusión

El proyecto demuestra cómo modelar estructuras matemáticas como árboles de datos y evaluarlas de manera segura en un entorno funcional.
El uso de Maybe evita excepciones en tiempo de ejecución y permite un manejo explícito de errores matemáticos.