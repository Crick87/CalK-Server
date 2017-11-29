module FunTrigonometricas where

import FunBasicas

-- COSENO
-- Params: 
--    x=numero a calcular coseno.
coseno :: Double -> Double
coseno x = cosenoTaylor x 0 20
-- Serie de Taylor para coseno
-- Params: 
--    x=numero a calcular coseno, 
--    y=inicio de serie, 
--    z=fin de serie (aproximación).
cosenoTaylor :: Double -> Double -> Double -> Double
cosenoTaylor x y z = 
  if y < z
  then
      (potencia (-1.0)  y ) 
      * (( potencia x (2*y) ) / (factorial (2*y) ))
      + (cosenoTaylor x (y+1) z)
  else
      0

-- SENO
-- Params: 
--    x=numero a calcular seno.
seno :: Double -> Double
seno x = senoTaylor x 0 20
-- Serie de Taylor para seno
-- Params: 
--    x=numero a calcular seno, 
--    y=inicio de serie, 
--    z=fin de serie (aproximación).
senoTaylor :: Double -> Double -> Double -> Double
senoTaylor x y z = 
  if y < 100
  then 
      (potencia (-1.0)  y ) 
      * (( potencia x (2*y + 1) ) / (factorial (2*y + 1) ))
      + (senoTaylor x (y+1) z)
  else
      0

-- TANGENTE
-- Params: 
--    x=numero a calcular tangente.
tangente :: Double -> Double
tangente x = seno x / coseno x

-- COTANGENTE
-- Params: 
--    x=numero a calcular cotangente.
cotangente :: Double -> Double
cotangente x = coseno x / seno x 
cotangente2 x = 1 / tangente x

-- SECANTE
-- Params: 
--    x = Numero a calcular secante.
secante :: Double -> Double
secante x = 1 / coseno x 

-- COSECANTE
-- Params: 
--    x = Numero a calcular cosecante.
cosecante :: Double -> Double
cosecante x = 1 / seno x 

--CONVERSION DE GRADOS A RADIANES
-- Params: 
--    x = Grados a convertir en radianes.
grad2rad :: Double -> Double
grad2rad x = x*(3.1416/180)

--CONVERSION DE RADIANES A GRADOS
-- Params: 
--    x = Radianes a convertir en grados.
rad2grad :: Double -> Double
rad2grad x = x*180/3.14