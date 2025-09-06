-- | Un `Histograma` es una estructura de datos que permite contar cuántos valores hay en cada rango.
-- @vacio n (a, b)@ devuelve un histograma vacío con n+2 casilleros:
--
-- * @(-inf, a)@
-- * @[a, a + tamIntervalo)@
-- * @[a + tamIntervalo, a + 2*tamIntervalo)@
-- * ...
-- * @[b - tamIntervalo, b)@
-- * @[b, +inf)@
--
-- `vacio`, `agregar` e `histograma` se usan para construir un histograma.
module Histograma
  ( Histograma, -- No se exportan los constructores
    vacio,
    agregar,
    histograma,
    Casillero (..),
    casMinimo,
    casMaximo,
    casCantidad,
    casPorcentaje,
    casilleros,
  )
where

import Util
import Data.List (zipWith4)


data Histograma = Histograma Float Float [Int]
  deriving (Show, Eq)

-- | Inicializa un histograma vacío con @n@ casilleros para representar
-- valores en el rango y 2 casilleros adicionales para los valores fuera del rango.
-- Require que @l < u@ y @n >= 1@.
vacio :: Int -> (Float, Float) -> Histograma
vacio n (l,u) = Histograma l ((u - l) / fromIntegral n) (replicate (n+2) 0)

-- | Agrega un valor al histograma.
agregar :: Float -> Histograma -> Histograma
agregar x (Histograma i t cs)
  | x < i     = Histograma i t (actualizarElem 0 (+1) cs) -- x < i → entra en el primer casillero (-∞, i)
  | x >= limite = Histograma i t (actualizarElem (length cs - 1) (+1) cs) -- x >= limite → entra en el último casillero [limite, ∞)
  | otherwise  = Histograma i t (actualizarElem pos (+1) cs) -- otherwise → entra en alguno de los casilleros intermedios, calculando con floor
  where
    limite = i + t * fromIntegral (length cs - 2) -- marca donde empieza el ultimo casillero del histograma
    pos    = floor ((x - i) / t) + 1

-- | Arma un histograma a partir de una lista de números reales con la cantidad de casilleros y rango indicados.
histograma :: Int -> (Float, Float) -> [Float] -> Histograma
histograma n r xs = foldr(\x rec -> agregar x rec) (vacio n r) xs

-- | Un `Casillero` representa un casillero del histograma con sus límites, cantidad y porcentaje.
-- Invariante: Sea @Casillero m1 m2 c p@ entonces @m1 < m2@, @c >= 0@, @0 <= p <= 100@
data Casillero = Casillero Float Float Int Float
  deriving (Show, Eq)

-- | Mínimo valor del casillero (el límite inferior puede ser @-inf@)
casMinimo :: Casillero -> Float
casMinimo (Casillero m _ _ _) = m

-- | Máximo valor del casillero (el límite superior puede ser @+inf@)
casMaximo :: Casillero -> Float
casMaximo (Casillero _ m _ _) = m

-- | Cantidad de valores en el casillero. Es un entero @>= 0@.
casCantidad :: Casillero -> Int
casCantidad (Casillero _ _ c _) = c

-- | Porcentaje de valores en el casillero respecto al total de valores en el histograma. Va de 0 a 100.
casPorcentaje :: Casillero -> Float
casPorcentaje (Casillero _ _ _ p) = p

-- | Dado un histograma, devuelve la lista de casilleros con sus límites, cantidad y porcentaje.
-- casilleros :: Histograma -> [Casillero]
-- casilleros (Histograma i t lista) = zipWith4 Casillero mins maxs lista porcentajes
--   where
--     n = length lista - 2
--     total = fromIntegral (sum lista)
--     -- funcion para calcular el porcentaje de c/ casillero
--     porcentaje :: Int -> Float
--     porcentaje cantidad = if total == 0 then 0 else (fromIntegral cantidad / total) * 100

--     -- límites inferiores
--     mins = infinitoNegativo : [ i + fromIntegral k * t | k <- [0..n-1] ] ++ [i + fromIntegral n * t]

--     -- límites superiores
--     maxs = i : [ i + fromIntegral (k+1) * t | k <- [0..n-1] ] ++ [infinitoPositivo]

--     porcentajes = map porcentaje lista

casilleros :: Histograma -> [Casillero]
casilleros (Histograma i t cs) = zipWith4 Casillero (comienzosDeIntervalos) (finesDeIntervalos) cs (porcentajes)
  where 
    porcentaje :: Int -> Float
    porcentaje cantidad = fromIntegral cantidad / (fromIntegral (sum cs)) * 100
    n = fromIntegral (length cs)
    comienzosDeIntervalos = infinitoNegativo : (map  (\x -> i + x*t ) [0 .. (n - 2)])
    finesDeIntervalos = (map  (\x -> i + x*t - 1) [0 .. (n - 2)]) ++ [infinitoPositivo]
    porcentajes = map porcentaje cs