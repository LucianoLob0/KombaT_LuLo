module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "poderDeGolpe" $ do
    it "15 veces las horas de entrenamiento" $ do
      poderDeGolpe 3 `shouldBe` 45
      poderDeGolpe 2.5 `shouldBe` 37.5

  describe "fortalezaDelObjetivo" $ do
    it "Doble de la cantidad de letras y espacios del nombre" $ do
      fortalezaDelObjetivo "Bolsa" `shouldBe` 10
      fortalezaDelObjetivo "Bolsa de boxeo" `shouldBe` 28
  
  describe "presionDelGolpe" $ do
    it "cociente de poderDeGolpe y fortalezaDelObjetivo" $ do
      presionDelGolpe 4 "Bolsa" `shouldBe` 6
      presionDelGolpe 2.1 "Bolsa de boxeo" `shouldBe` 1.125

  describe "gomuGomu" $ do
    it "presionDelGolpe de 180 horas dividido la fortalezaDelObjetivo" $ do
      gomuGomu "Bolsa" `shouldBe` 270
      gomuGomu "Res Entera" `shouldBe` 135

  describe "golpesNormales" $ do
    it "presionDelGolpe de 240 horas dividido la fortalezaDelObjetivo" $ do
      golpesNormales "Bolsa" `shouldBe` 360
      golpesNormales "Res Entera" `shouldBe` 180

  describe "objetivoDificil" $ do
    it "gomuGomu sobre un objetivo dando una presión menor a 100" $ do
      objetivoDificil "Bolsa de Entrenamiento" `shouldBe` True
      objetivoDificil "Puf" `shouldBe` False

  describe "focalizarObjetivo" $ do
    it "Obtiene hasta 7 primeros caracteres y/o espacios del nombre de un objetivo" $ do
      focalizarObjetivo "Bolsa de Entrenamiento" `shouldBe` "Bolsa d"
      focalizarObjetivo "Puf" `shouldBe` "Puf"

  describe "accesibilidad" $ do
    it "Presión de los golpes normales sobre un objetivo focalizado" $ do
      accesibilidad "Bolsa de Entrenamiento" `shouldBe` 257.142857143 
      accesibilidad "Puf" `shouldBe` 600

  describe "objetivoAccesible" $ do
    it "Accesibilidad entre 200 y 400 de presión" $ do
      objetivoAccesible "Bolsa de Entrenamiento" `shouldBe` True 
      objetivoAccesible "Puf" `shouldBe` False

