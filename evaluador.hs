-- Modelo de datos para expresiones aritméticas.
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
eval (Add e1 e2) = sadd (eval e1) (eval e2)
eval (Sub e1 e2) = ssub (eval e1) (eval e2)
eval (Mul e1 e2) = smul (eval e1) (eval e2)
eval (Div e1 e2) = sdiv (eval e1) (eval e2)
eval (Pow e1 e2) = spow (eval e1) (eval e2)          -- ** Es para elevar flotantes
eval (Log e1 e2) = slog (eval e1) (eval e2)

-- eval (Log (Div (Lit 90) (Mul (Pow (Lit 3) (Lit 2)) (Lit 5))) (Lit 8)) -> Ejemplo



-- Funciones auxiliares (no recursivas) para el manejo de errores (No se pueden sumar/restar/dividir/etc. dos justs en las eval)
-- (s de safe)

sadd :: (Floating a, Eq a, Ord a) => Maybe a -> Maybe a -> Maybe a
sadd (Just x) (Just y) = Just (x+y)
sadd _ _ = Nothing

ssub :: (Floating a, Eq a, Ord a) => Maybe a -> Maybe a -> Maybe a
ssub (Just x) (Just y) = Just (x-y)
ssub _ _ = Nothing

smul :: (Floating a, Eq a, Ord a) => Maybe a -> Maybe a -> Maybe a
smul (Just x) (Just y) = Just (x*y)
smul _ _ = Nothing

sdiv :: (Floating a, Eq a, Ord a) => Maybe a -> Maybe a -> Maybe a
sdiv (Just _) (Just 0) = Nothing
sdiv (Just x) (Just y) = Just (x/y)
sdiv _ _ = Nothing

spow :: (Floating a, Eq a, Ord a) => Maybe a -> Maybe a -> Maybe a
spow (Just x) (Just y) = Just (x**y)
spow _ _ = Nothing

slog :: (Floating a, Eq a, Ord a) => Maybe a -> Maybe a -> Maybe a
slog (Just x) (Just y)
    | (x<=0) = Nothing
    | (x==1) = Nothing
    | (y<=0) = Nothing
    | otherwise = Just (logBase x y)
slog _ _ = Nothing