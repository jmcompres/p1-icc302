{-
===============================================================================
EVALUADOR DE EXPRESIONES ARITMETICAS
===============================================================================
José Miguel Comprés - 1015
María José Cruz - 10154963
Almy Ventura - 10153712

Descripcion general:

Este programa define un modelo algebraico para representar expresiones
aritmeticas y las evalua de manera segura utilizando el tipo Maybe.

El uso de Maybe permite manejar errores matematicos sin que el programa
se detenga por excepciones en tiempo de ejecucion.

Las operaciones invalidas (division por cero, logaritmos fuera de dominio,
etc.) devuelven Nothing.
-}


-- DEFINICION DEL TIPO DE DATO
{-
El tipo Expr a representa una expresion aritmetica parametrica.

Es un tipo algebraico recursivo:
- Cada constructor puede contener otras expresiones.
- Esto permite formar arboles de operaciones matematicas.
-}
data Expr a
    = Lit a                             -- Literal numerico (caso base)
          | Add (Expr a) (Expr a)       -- Suma
          | Sub (Expr a) (Expr a)       -- Resta
          | Mul (Expr a) (Expr a)       -- Multiplicacion
          | Div (Expr a) (Expr a)       -- Division        
          | Pow (Expr a) (Expr a)       -- Potencia
          | Log (Expr a) (Expr a)       -- Lgaritmo base e1 de e2
    deriving (Show, Eq, Read)

    -- Show -> Permite imprimir la expresion.
    -- Eq -> Permite evaluar expresiones.
    -- Read -> Permite convertir texto en Expr automaticamente.

-- FUNCION DE EVALUACION (RECURSIVA)
{-
eval toma una expresion y devuelve:

    Just resultado  -> si la operacion es valida
    Nothing         -> si ocurre un error matematico

La evaluacion es recursiva estructural:
- Primero evalua las subexpresiones.
- Luego aplica la operacion correspondiente.
-}

eval :: (Floating a, Eq a, Ord a) => Expr a -> Maybe a
eval (Lit x) = Just x                                       -- Caso base: un literal simplemente devuelve su valor.
eval (Add e1 e2) = sopgen (+) (eval e1) (eval e2)           -- Para cada operacion binaria:
eval (Sub e1 e2) = sopgen (-) (eval e1) (eval e2)           -- 1. Se evalua recursivamente cada subexpresion.
eval (Mul e1 e2) = sopgen (*) (eval e1) (eval e2)           -- 2. Se combinan los resultados con funciones seguras.
eval (Div e1 e2) = sdiv (eval e1) (eval e2)                 -- Division necesita validacion especial.
eval (Pow e1 e2) = sopgen (**) (eval e1) (eval e2)
eval (Log e1 e2) = slog (eval e1) (eval e2)                 -- Logaritmo necesita validacion de dominio.

-- eval (Log (Div (Lit 90) (Mul (Pow (Lit 3) (Lit 2)) (Lit 5))) (Lit 8)) -> Ejemplo

-- FUNCIONES AUXILIARES (MANEJO SEGURO DE ERRORES) 
-- (No se pueden sumar/restar/dividir/etc. dos justs en las eval)
-- (s de safe)

{-
Estas funciones evitan combinar directamente valores Maybe.

Ejemplo incorrecto:
    Just 3 + Just 4   -- No es valido

En su lugar, verificamos que ambos valores sean Just.
-}

-- safeOperacionGeneral (aquí se condensan suma, resta, multiplicación y pow, que tienen las mismas reglas para sus operaciones)
sopgen :: (Floating a, Eq a, Ord a) => (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a  
sopgen oper (Just x) (Just y) = Just (oper x y)         -- Si ambos valores existen, aplica la operacion.
sopgen _ _ _ = Nothing                                  -- Si alguno es Nothing, la operacion falla.

-- Division segura: evita division por cero.
sdiv :: (Floating a, Eq a, Ord a) => Maybe a -> Maybe a -> Maybe a
sdiv (Just _) (Just 0) = Nothing
sdiv (Just x) (Just y) = Just (x/y)
sdiv _ _ = Nothing

-- Logaritmo seguro:
-- Reglas matematicas:
--  base > 0
--  base ≠ 1
--  argumento > 0
slog :: (Floating a, Eq a, Ord a) => Maybe a -> Maybe a -> Maybe a
slog (Just x) (Just y)
    | (x<=0) = Nothing
    | (x==1) = Nothing
    | (y<=0) = Nothing
    | otherwise = Just (logBase x y)
slog _ _ = Nothing


-- FUNCION PARA MOSTRAR RESULTADO

-- Función para mensajes
evaluar :: (Show a) => Maybe a -> String
evaluar (Just x) = "Resultado: "++(show x)
evaluar Nothing = "Operacion matematica invalida"

-- INTERFAZ CON EL USUARIO (IO)
main :: IO ()
main = do
    putStrLn "======================================"
    putStrLn "     EVALUADOR DE EXPRESIONES"
    putStrLn "======================================"
    putStrLn ""
    putStrLn "Este programa evalua expresiones aritmeticas"
    putStrLn "usando los siguientes constructores:"
    putStrLn ""
    putStrLn "Lit x        -> Literal (numero)"
    putStrLn "Add e1 e2    -> Suma"
    putStrLn "Sub e1 e2    -> Resta"
    putStrLn "Mul e1 e2    -> Multiplicacion"
    putStrLn "Div e1 e2    -> Division (segura, no permite dividir por 0)"
    putStrLn "Pow e1 e2    -> Potencia (e1 elevado a e2)"
    putStrLn "Log e1 e2    -> Logaritmo base e1 de e2"
    putStrLn ""
    putStrLn "--------------------------------------"
    putStrLn "SINTAXIS CORRECTA:"
    putStrLn ""
    putStrLn "Las expresiones deben escribirse usando parentesis."
    putStrLn ""
    putStrLn "Ejemplos validos:"
    putStrLn "  Add (Lit 2) (Lit 3)"
    putStrLn "  Mul (Lit 4) (Add (Lit 1) (Lit 2))"
    putStrLn "  Div (Lit 10) (Lit 2)"
    putStrLn "  Pow (Lit 2) (Lit 3)"
    putStrLn "  Log (Lit 2) (Lit 8)"
    putStrLn ""
    putStrLn "Presiona Enter sin escribir nada para salir."
    putStrLn "--------------------------------------"
    loop

-- Bucle interactivo
-- 1) Muestra un mensaje para que el usuario escriba una expresion.
-- 2) Lee la linea ingresada como String.
-- 3) Si la linea esta vacia, termina el programa.
-- 4) Si no esta vacia, intenta parsear la entrada con `reads` como `Expr Double`.
--    - `[(expr, "")]` significa: parseo exitoso y sin texto sobrante.
--    - cualquier otro caso significa error de sintaxis.
-- 5) Si parsea bien, evalua la expresion y muestra el resultado.
-- 6) Vuelve a llamarse a si misma para seguir pidiendo expresiones.
loop :: IO ()
loop = do
    putStrLn "\nIngresa una expresion:"
    input <- getLine

    if null input
        then putStrLn "Saliendo del evaluador..."
        else do
            case reads input :: [(Expr Double, String)] of
                [(expr, "")] -> putStrLn (evaluar (eval expr))
                _            -> putStrLn "Error de sintaxis en la expresion."
            loop
