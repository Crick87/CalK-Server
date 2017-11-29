-- Pragmas necesarios para Yesod
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
-- Importaciones Yesod
import Data.Text.Read
import Data.Text as T (Text, append, cons, head, isPrefixOf, null, dropWhile, drop)
import Yesod
-- Importaciones Módulos Calculadora
import FunBasicas
import FunTrigonometricas
import FunLogaritmicas
import FunExponenciales

-- Declaración de la estructura de respuesta
data Resultado = Resultado
    { 
        result :: Double
    }
-- Instanciación de la respuesta con formato JSON
instance ToJSON Resultado where
    toJSON Resultado {..} = object
        [ 
            "resultado" .= result
        ]

-- Instanciación de la aplicación
data App = App
instance Yesod App

-- Rutas de la API, variables de entrada, métodos de rutas y tipo de petición
mkYesod "App" [parseRoutes|
/potencia/#Text/#Text PotenciaR GET
/factorial/#Text FactorialR GET
/seno/#Text SenoR GET
/coseno/#Text CosenoR GET
/tangente/#Text TangenteR GET
/cotangente/#Text CotangenteR GET
/secante/#Text SecanteR GET
/cosecante/#Text CosecanteR GET
/ln/#Text LnR GET
/logaritmo/#Text/#Text LogaritmoR GET
/exponencial/#Text ExponencialR GET
|]

------------------------Métodos controladores en la API Web-----------------------------
----------------------------------------------------------------------------------------

---- BÁSICAS:
-- Potencia
getPotenciaR :: Text -> Text -> Handler TypedContent
getPotenciaR base exponente = selectRep $ do provideJson res where
    res@Resultado {..} = Resultado (potencia (text2double base) (text2double exponente))
-- Factorial
getFactorialR :: Text -> Handler TypedContent
getFactorialR number = selectRep $ do provideJson res where
    res@Resultado {..} = Resultado (factorial(text2double number))

---- TRIGONOMÉTRICAS:
-- Seno
getSenoR :: Text -> Handler TypedContent
getSenoR number = selectRep $ do provideJson res where
    res@Resultado {..} = Resultado (seno (text2double number))
-- Coseno
getCosenoR :: Text -> Handler TypedContent
getCosenoR number = selectRep $ do provideJson res where
    res@Resultado {..} = Resultado (coseno (text2double number))
-- Tangente
getTangenteR :: Text -> Handler TypedContent
getTangenteR number = selectRep $ do provideJson res where
    res@Resultado {..} = Resultado (tangente (text2double number))
-- Cotangente
getCotangenteR :: Text -> Handler TypedContent
getCotangenteR number = selectRep $ do provideJson res where
    res@Resultado {..} = Resultado (cotangente (text2double number))
-- Secante
getSecanteR :: Text -> Handler TypedContent
getSecanteR number = selectRep $ do provideJson res where
    res@Resultado {..} = Resultado (secante (text2double number))
-- Cosecante
getCosecanteR :: Text -> Handler TypedContent
getCosecanteR number = selectRep $ do provideJson res where
    res@Resultado {..} = Resultado (cosecante (text2double number))

---- LOGARÍTMICAS:
-- Logaritmo natural
getLnR :: Text -> Handler TypedContent
getLnR number = selectRep $ do provideJson res where
    res@Resultado {..} = Resultado (ln (text2double number))
-- Logaritmo base X
getLogaritmoR :: Text -> Text -> Handler TypedContent
getLogaritmoR base number = selectRep $ do provideJson res where
    res@Resultado {..} = Resultado (logaritmo (text2double base) (text2double number))

---- EXPONECIAL:
getExponencialR :: Text -> Handler TypedContent
getExponencialR number = selectRep $ do provideJson res where
    res@Resultado {..} = Resultado (exponencial (text2double number))

----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------

-- Convierte Texto a Double
text2double :: Text -> Double
text2double t = 
    case Data.Text.Read.double (precederCero t) of Right (a, "") ->  a
-- Precede un 0 a valores como '.5' o '-.5' para evitar errores en la conversión a double.
precederCero :: Text -> Text
precederCero t0 = 
    if T.null t1
    then t1
    else if T.head t1 == '.'
        then '0' `T.cons` t1
        else if "-." `T.isPrefixOf` t1
            then "-0." `T.append` (T.drop 2 t1)
            else t1
  where t1 = T.dropWhile ((==) ' ') t0

-- Método principal, manda al puerto 3000 de localhost el resultado de las peticiones
main :: IO ()
main = warp 3000 App
