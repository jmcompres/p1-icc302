-- Modelo de datos para expresiones aritméticas (con tipo de dato genérico).
data Expr a = Lit a                                -- Lit de literal (cambié de Num por si acaso, porque ya hay una clase Num)
          | Add (Expr a) (Expr a)
          | Sub (Expr a) (Expr a)
          | Mul (Expr a) (Expr a)
          | Div (Expr a) (Expr a)
          | Pow (Expr a) (Expr a)
          | Log (Expr a) (Expr a)
          deriving (Show, Eq)



-- Función de evaluación recursiva
eval :: (Floating a, Eq a, Ord a) => Expr a -> Maybe a
eval (Lit x) = Just x
eval (Add e1 e2) = sopgen (+) (eval e1) (eval e2)
eval (Sub e1 e2) = sopgen (-) (eval e1) (eval e2)
eval (Mul e1 e2) = sopgen (*) (eval e1) (eval e2)
eval (Div e1 e2) = sdiv (eval e1) (eval e2)
eval (Pow e1 e2) = sopgen (**) (eval e1) (eval e2)          -- ** Es para elevar flotantes
eval (Log e1 e2) = slog (eval e1) (eval e2)

-- eval (Log (Div (Lit 90) (Mul (Pow (Lit 3) (Lit 2)) (Lit 5))) (Lit 8)) -> Ejemplo



-- Funciones auxiliares (no recursivas) para el manejo de errores (No se pueden sumar/restar/dividir/etc. dos justs en las eval)
-- (s de safe)

sopgen :: (Floating a, Eq a, Ord a) => (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a  -- safeOperacionGeneral (aquí se condensan suma, resta, multiplicación y pow, que tienen las mismas reglas para sus operaciones)
sopgen oper (Just x) (Just y) = Just (oper x y)
sopgen _ _ _ = Nothing

sdiv :: (Floating a, Eq a, Ord a) => Maybe a -> Maybe a -> Maybe a
sdiv (Just _) (Just 0) = Nothing
sdiv (Just x) (Just y) = Just (x/y)
sdiv _ _ = Nothing

slog :: (Floating a, Eq a, Ord a) => Maybe a -> Maybe a -> Maybe a
slog (Just x) (Just y)
    | (x<=0) = Nothing
    | (x==1) = Nothing
    | (y<=0) = Nothing
    | otherwise = Just (logBase x y)
slog _ _ = Nothing



-- Función para mensajes
evaluar :: (Show a) => Maybe a -> String
evaluar (Just x) = "Resultado: "++(show x)
evaluar Nothing = "Operacion matematica invalida"