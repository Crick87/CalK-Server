-- Pragmas necesarios para Yesod
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
-- Importaciones Yesod
import Data.Text.Read
import Data.Text as T                (Text, append, cons, head, isPrefixOf, null, dropWhile, drop)
import Yesod
-- Convertir a Yesod a Wai
import Network.HTTP.Types            (status200)
import Network.Wai                   (responseBuilder)
import Network.Wai.Handler.Warp      (run)
import Network.Wai.Middleware.Cors
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
/grad2rad/#Text Grad2RadR GET
/rad2grad/#Text Rad2GradR GET
/ln/#Text LnR GET
/logaritmo/#Text/#Text LogaritmoR GET
/exponencial/#Text ExponencialR GET
/suma/#Text/#Text SumaR GET
/resta/#Text/#Text RestaR GET
/multiplicacion/#Text/#Text MultiplicacionR GET
/division/#Text/#Text DivisionR GET
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
-- Suma
getSumaR :: Text -> Text -> Handler TypedContent
getSumaR number1 number2 = selectRep $ do provideJson res where
    res@Resultado {..} = Resultado (suma (text2double number1) (text2double number2))
-- Resta
getRestaR :: Text -> Text -> Handler TypedContent
getRestaR number1 number2 = selectRep $ do provideJson res where
    res@Resultado {..} = Resultado (resta (text2double number1) (text2double number2))
-- Multiplicacion
getMultiplicacionR :: Text -> Text -> Handler TypedContent
getMultiplicacionR number1 number2 = selectRep $ do provideJson res where
    res@Resultado {..} = Resultado (multiplicacion (text2double number1) (text2double number2))
-- Divicion
getDivisionR :: Text -> Text -> Handler TypedContent
getDivisionR number1 number2 = selectRep $ do provideJson res where
    res@Resultado {..} = Resultado (division (text2double number1) (text2double number2))

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
-- Convertir grados a radianes
getGrad2RadR :: Text -> Handler TypedContent
getGrad2RadR number = selectRep $ do provideJson res where
    res@Resultado {..} = Resultado (grad2rad (text2double number))
-- Convertir radianes a grados
getRad2GradR :: Text -> Handler TypedContent
getRad2GradR number = selectRep $ do provideJson res where
    res@Resultado {..} = Resultado (rad2grad (text2double number))

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
main = do
    waiApp <- toWaiApp App
    run 3000 $ simpleCors waiApp