module FunEstadisticas where

import FunBasicas

--NUMERO ALEATORIO
--GenerarNumeroAleatorio
--Params:
-- xn = Primer numero aleatorio
-- m = numero mayor del rango
numAleatorio :: Integer -> Integer -> Integer
numAleatorio xn m = ((81*xn+89) `mod` m)

--Sumar
-- xs es una lista que contiene todos los valores que se desean sumar
sumar::[Double]->Double
sumar [ ] = 0
sumar (x:xs) = x + sumar(xs)

-- Devuelve el número de elementos de una lista
-- xs es una lista con valores de tipo Double
-- devuelve un valor de tipo Double
long :: [a] -> Double
long []     = 0
long (x:xs) = 1 + long xs

-- Devuelve el número de elementos de una lista
-- Devuelve un valor de tipo Int
longInt :: [a] -> Int
longInt []     = 0
longInt (x:xs) = 1 + longInt xs

-- PROMEDIO
-- xs es una lista de los que se obtendrá la media aritmetica
-- El tamaño de la lista se obtiene con la funcion long
media :: [Double] -> Double
media  xs = sumar xs/long xs

--CONTAR
-- Cuenta cuantas veces se repite un numero de la lista
-- xs es una lista 
-- n es el elemento que se buscara en la lista
repn :: [Double] -> Double -> Double 
repn [] n=0 
repn (x:xs) n|x==n=1 + repn(xs) n 
     |otherwise=repn(xs) n

--Obtener elemento de una lista
--xs es la lista
--n es el tamaño de la lista
--n comienza en 0
obtenerelemento :: [Double] -> Int -> Double
obtenerelemento xs n= xs !! n

--NUMERO PAR
numpar :: Int -> Bool
numpar n =
 if n `mod` 2 ==  0
   then
     True
 else
     False

-- METODO DE ORDENAMIENTO QUICKSORT
-- Ordena una lista de menor a mayor
qsort [] = []
qsort (x:xs) = qsort small ++ mid ++ qsort large
    where
     small = [y | y<-xs, y<x]
     mid   = [y | y<-xs, y==x] ++ [x]
     large = [y | y<-xs, y>x]

--Combinaciones 
--n Total de elementos
--k Numero de elementos de los grupos que se desean formar
combinaciones :: Double -> Double -> Double
combinaciones n k = (factorial n/( factorial (n-k) * factorial k ))

--Permutaciones sin repetición de elementos 
--n Total de elementos
--k Numero de elementos de los grupos que se desean formar
permutacion1 :: Double -> Double -> Double
permutacion1 n k = factorial n/factorial (n-k)




                     
 




