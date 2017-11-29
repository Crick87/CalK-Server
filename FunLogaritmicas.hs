module FunLogaritmicas where

import FunBasicas

-- LOGARITMO NATURAL (ln)
-- Params: 
--    x = Numero a calcular ln.
ln :: Double -> Double
ln x = lnTaylor x 0 200

lnTaylor :: Double -> Double -> Double -> Double
lnTaylor x y z = 
  if y < z
  then
      ( 1 / (2*y+1) ) 
      * potencia ( ( ( potencia x 2 ) -1 ) / ( ( potencia x 2 ) +1 ) ) (2*y+1) 
      + (lnTaylor x (y+1) z)
  else
      0

-- LOGARITMO BASE X
-- Params: 
--    b = Base del loratimo.
--    x = Numero a calcular loratimo.
logaritmo :: Double -> Double -> Double
logaritmo b x = (ln x)/(ln b)