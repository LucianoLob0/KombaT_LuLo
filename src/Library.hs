module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

poderDeGolpe :: Number -> Number

poderDeGolpe horas = 15 * horas

fortalezaDelObjetivo :: String -> Number

fortalezaDelObjetivo nombre = 2 * length nombre 

presionDelGolpe :: Number -> String -> Number

presionDelGolpe a b = poderDeGolpe a / fortalezaDelObjetivo b