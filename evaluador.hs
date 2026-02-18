-- Modelo de datos para expresiones aritméticas.
data Expr a = Lit a                                -- Lit de literal (cambié de Num por si acaso, porque ya hay una clase Num)
          | Add (Expr a) (Expr a)
          | Sub (Expr a) (Expr a)
          | Mul (Expr a) (Expr a)
          | Div (Expr a) (Expr a)
          | Exp (Expr a) (Expr a)
          | Log (Expr a) (Expr a)
          deriving (Show, Eq)

eval :: (Floating a) => Expr a -> a
eval (Lit x) = x
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Sub e1 e2) = (eval e1) - (eval e2)
eval (Mul e1 e2) = (eval e1) * (eval e2)
eval (Div e1 e2) = (eval e1) / (eval e2)
eval (Exp e1 e2) = (eval e1) ** (eval e2)          -- ** Es para elevar flotantes
eval (Log e1 e2) = logBase (eval e1) (eval e2)

-- eval (Log (Div (Lit 90) (Mul (Exp (Lit 3) (Lit 2)) (Lit 5))) (Lit 8)) -> Ejemplo