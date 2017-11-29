module FunExponenciales where

import FunBasicas

-- EXPONENCIAL
-- Params: 
--    x=numero.
exponencial :: Double -> Double
exponencial x = exponencialTaylor x 0 20
-- Serie de Taylor para coseno
-- Params: 
--    x=numero a calcular coseno, 
--    y=inicio de serie, 
--    z=fin de serie (aproximación).
exponencialTaylor :: Double -> Double -> Double -> Double
exponencialTaylor x y z = 
  if y < z
  then
      (potencia x y)/(factorial y)
      + (exponencialTaylor x (y+1) z)
  else
      0

