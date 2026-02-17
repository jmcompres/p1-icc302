-- Modelo de datos para expresiones aritméticas.
data Expr = Num Double
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr

-- Ejemplo de expresión para probar.
ejemplo1 :: Expr
ejemplo1 = Add (Num 5) (Mul (Num 3) (Num 2))

--ghci> eval ejemplo1
--Just 11.0
--ghci> ejemplo1
--Add (Num 5.0) (Mul (Num 3.0) (Num 2.0))

ejemplo2 :: Expr
ejemplo2 = Div (Num 10) (Num 0)
--ghci> eval ejemplo2
--Nothing
--ghci> ejemplo2     
--Div (Num 10.0) (Num 0.0)