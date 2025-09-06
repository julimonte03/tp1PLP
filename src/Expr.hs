module Expr
  ( Expr (..),
    recrExpr,
    foldExpr,
    eval,
    armarHistograma,
    evalHistograma,
    mostrar,
  )
where

import Generador
import Histograma

-- | Expresiones aritméticas con rangos
data Expr
  = Const Float
  | Rango Float Float
  | Suma Expr Expr
  | Resta Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
  deriving (Show, Eq)

-- recrExpr :: ... anotar el tipo ...
recrExpr :: (Float -> a)                      -- Const
         -> (Float -> Float -> a)             -- Rango
         -> (Expr -> a -> Expr -> a -> a)     -- Suma
         -> (Expr -> a -> Expr -> a -> a)     -- Resta
         -> (Expr -> a -> Expr -> a -> a)     -- Mult
         -> (Expr -> a -> Expr -> a -> a)     -- Div
         -> Expr
         -> a
recrExpr fConst fRango fSuma fResta fMult fDiv expr =
  case expr of
    Const x   -> fConst x
    Rango a b -> fRango a b
    Suma e1 e2  -> fSuma e1 evalIzq e2 evalDer
    Resta e1 e2 -> fResta e1 evalIzq e2 evalDer
    Mult e1 e2  -> fMult e1 evalIzq e2 evalDer
    Div e1 e2   -> fDiv e1 evalIzq e2 evalDer
  where
    evalIzq = recrExpr fConst fRango fSuma fResta fMult fDiv e1
    evalDer = recrExpr fConst fRango fSuma fResta fMult fDiv e2

-- foldExpr :: ... anotar el tipo ...
foldExpr :: (Float -> a)      -- Const
         -> (Float -> Float -> a) -- Rango
         -> (a -> a -> a)         -- Suma
         -> (a -> a -> a)         -- Resta
         -> (a -> a -> a)         -- Mult
         -> (a -> a -> a)         -- Div
         -> Expr
         -> a
foldExpr fConst fRango fSuma fResta fMult fDiv expr =
  case expr of
    Const x   -> fConst x
    Rango a b -> fRango a b
    Suma e1 e2  -> fSuma evalIzq evalDer
    Resta e1 e2 -> fResta evalIzq evalDer
    Mult e1 e2  -> fMult evalIzq evalDer
    Div e1 e2   -> fDiv evalIzq evalDer
  where
    evalIzq = foldExpr fConst fRango fSuma fResta fMult fDiv e1
    evalDer = foldExpr fConst fRango fSuma fResta fMult fDiv e2

-- | Evaluar expresiones dado un generador de números aleatorios
eval :: Expr -> Gen -> (Float, Gen)
eval expr gen = foldExpr
    (\x g -> (x, g))                 -- Const
    (\a b g -> dameUno (a, b) g)     -- Rango
    (\(expr1,g1) (expr2,g2) -> (expr1+expr2, g2)) -- Suma
    (\(expr1,g1) (expr2,g2) -> (expr1-expr2, g2)) -- Resta
    (\(expr1,g1) (expr2,g2) -> (expr1*expr2, g2)) -- Mult
    (\(expr1,g1) (expr2,g2) -> (expr1/expr2, g2)) -- Div
    expr gen


-- | @armarHistograma m n f g@ arma un histograma con @m@ casilleros
-- a partir del resultado de tomar @n@ muestras de @f@ usando el generador @g@.
armarHistograma :: Int -> Int -> G Float -> G Histograma
armarHistograma m n f g = error "COMPLETAR EJERCICIO 9"

-- | @evalHistograma m n e g@ evalúa la expresión @e@ usando el generador @g@ @n@ veces
-- devuelve un histograma con @m@ casilleros y rango calculado con @rango95@ para abarcar el 95% de confianza de los valores.
-- @n@ debe ser mayor que 0.
evalHistograma :: Int -> Int -> Expr -> G Histograma
evalHistograma m n expr = error "COMPLETAR EJERCICIO 10"

-- Podemos armar histogramas que muestren las n evaluaciones en m casilleros.
-- >>> evalHistograma 11 10 (Suma (Rango 1 5) (Rango 100 105)) (genNormalConSemilla 0)
-- (Histograma 102.005486 0.6733038 [1,0,0,0,1,3,1,2,0,0,1,1,0],<Gen>)

-- >>> evalHistograma 11 10000 (Suma (Rango 1 5) (Rango 100 105)) (genNormalConSemilla 0)
-- (Histograma 102.273895 0.5878462 [239,288,522,810,1110,1389,1394,1295,1076,793,520,310,254],<Gen>)

-- | Mostrar las expresiones, pero evitando algunos paréntesis innecesarios.
-- En particular queremos evitar paréntesis en sumas y productos anidados.
mostrar :: Expr -> String
mostrar = error "COMPLETAR EJERCICIO 11"

data ConstructorExpr = CEConst | CERango | CESuma | CEResta | CEMult | CEDiv
  deriving (Show, Eq)

-- | Indica qué constructor fue usado para crear la expresión.
constructor :: Expr -> ConstructorExpr
constructor (Const _) = CEConst
constructor (Rango _ _) = CERango
constructor (Suma _ _) = CESuma
constructor (Resta _ _) = CEResta
constructor (Mult _ _) = CEMult
constructor (Div _ _) = CEDiv

-- | Agrega paréntesis antes y después del string si el Bool es True.
maybeParen :: Bool -> String -> String
maybeParen True s = "(" ++ s ++ ")"
maybeParen False s = s
