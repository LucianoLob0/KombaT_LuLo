module Library where
import PdePreludat

type Horas = Number
type Objetivo = String
type Presion = Number
type Golpe = Objetivo -> Number

poderDeGolpe :: Horas -> Number
poderDeGolpe  = (15*)

fortalezaDelObjetivo :: Objetivo -> Number
fortalezaDelObjetivo = (2*) . length 

presionDelGolpe :: Horas -> Objetivo -> Presion
presionDelGolpe horas objetivo = poderDeGolpe horas / fortalezaDelObjetivo objetivo

gomuGomu :: Golpe
gomuGomu = presionDelGolpe 180

golpesNormales :: Golpe
golpesNormales = presionDelGolpe 240

objetivoDificil :: Objetivo -> Bool
objetivoDificil objetivo = gomuGomu objetivo < 100

focalizarObjetivo :: Objetivo -> String
focalizarObjetivo = take 7

accesibilidad :: Objetivo -> Presion
accesibilidad = golpesNormales . focalizarObjetivo

objetivoAccesible :: Objetivo -> Bool
objetivoAccesible objetivo = 200 < accesibilidad objetivo && 400 > accesibilidad objetivo
