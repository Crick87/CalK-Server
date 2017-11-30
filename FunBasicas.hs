module FunBasicas where

-- FACTORIAL
-- Params: 
--    x = Numero a calcular factorial.
factorial :: Double -> Double
factorial 0 = 1
factorial x = x * factorial (x - 1)

-- POTENCIA
-- Params: 
--    x=base, 
--    n=exponente.
potencia :: Double -> Double -> Double
potencia x 0 = 1
potencia x n = x*potencia x (n-1)

suma :: Double -> Double -> Double
suma a b = a + b

resta :: Double -> Double -> Double
resta a b = a - b

multiplicacion :: Double -> Double -> Double
multiplicacion a b = a * b

division :: Double -> Double -> Double
division a b = a/b