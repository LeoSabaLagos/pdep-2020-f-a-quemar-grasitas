module Lib where
import Text.Show.Functions

laVerdad = True

-----------------------------------------------------------------------------------------------
-- Gimnasta
data Gimnasta = Gimnasta{
    nombreGimnasta :: String,
    edad :: Float,
    peso :: Float,
    coefTonificacion :: Float
    } deriving(Show)


-- Gimnastas de ejemplo
pancho = Gimnasta "Francisco" 40.0 120.0 1.0
andres = Gimnasta "Andy" 22.0 80.0 6.0
--                nombre edad peso coef.tonificación 
-----------------------------------------------------------------------------------------------
-- Punto 1

--Saber si alguien está saludable, 
--lo cual se cumple si no está obeso y tiene una tonificación mayor a 5. 
--Alguien es obeso si pesa más de 100 kilos. 

saludable :: Gimnasta -> Bool
saludable gimnasta = (not.esObeso) gimnasta && estaTonificado gimnasta

--Alguien es obeso si pesa más de 100 kilos. 
esObeso :: Gimnasta -> Bool
esObeso = (>100).peso
-- esObeso gimnasta = peso gimnasta > 100

-- Alguien esta tonificado si tiene una tonificación mayor a 5
estaTonificado :: Gimnasta -> Bool
estaTonificado = (>5).coefTonificacion
--estaTonificado gimnasta = coefTonificacion gimnasta > 5
-----------------------------------------------------------------------------------------------
-- Punto 2

--Hacer que el gimnasta queme una cantidad de calorías, lo que produce que baje de peso.

--Si el gimnasta es obeso, baja 1 kilo cada 150 calorías quemadas.

--Si no es obeso pero tiene más de 30 años y las calorías quemadas son más de 200, baja siempre un kilo.

--En cualquier otro caso se baja la cantidad de calorías 
--quemadas dividido por el producto entre el peso y la edad del gimnasta. 

quemarCalorias :: Float -> Gimnasta -> Gimnasta 
quemarCalorias calorias gimnasta 
    | esObeso gimnasta = gimnasta{peso = (peso gimnasta) - (calorias / 150.0)}
    | masDeTreintaAnios gimnasta && calorias > 200 = gimnasta{peso = (peso gimnasta) - 1}
    | otherwise = gimnasta{peso = (peso gimnasta) - (calorias / (peso gimnasta * edad gimnasta))}

masDeTreintaAnios :: Gimnasta -> Bool
masDeTreintaAnios = (>30).edad
-----------------------------------------------------------------------------------------------
-- Punto 3
-- Desarrollar las funciones para los ejercicios 
-- caminataEnCinta, entrenamientoEnCinta, pesas, colina y montania sabiendo que:

{- data Caminata = UnaCaminata{
    caminar :: Float -> Gimnasta -> Gimnasta
}

data Tonificacion = UnaTonificacion{
    tonificacion :: Float -> Float -> Gimnasta -> Gimnasta
} 
--No es necesario creoooo

relax minutos gimnasta = gimnasta
-}

type Ejercicio =  Float -> Gimnasta -> Gimnasta

-- Parte a

--La cinta quema calorías en función de la velocidad promedio alcanzada durante el ejercicio, 
--quemando 1 caloría por la velocidad promedio por minuto.

--La caminata es un ejercicio en cinta con velocidad constante de 5 km/h. 

-- > caminataEnCinta 40 pancho 
--Gimnasta "Francisco" 40.0 118.6 1.0 ­­­ --quema 200 calorías (1*5*40) 

caminataEnCinta :: Ejercicio
--caminataEnCinta minutos gimnasta = quemarCalorias (calcularCalorias minutos 5.0) gimnasta -- VelocidadPromedio == 5
caminataEnCinta minutos = quemarCalorias (calcularCalorias minutos 5.0)

calcularCalorias :: Float -> Float -> Float
calcularCalorias minutos = (*minutos)
--calcularCalorias minutos velocidadPromedio = minutos * velocidadPromedio -}

-- El entrenamiento en cinta arranca en 6 km/h y 
-- cada 5 minutos incrementa la velocidad en 1 km/h,
-- con lo cual la velocidad máxima dependerá de los minutos de entrenamiento.

-- > entrenamientoEnCinta 40 pancho 
-- Gimnasta "Francisco" 40.0 117.3 1.0 ­­­ 

-- quema 400 calorías (1* ((6+14)/2) * 40) 
-- siendo 14 la velocidad máxima alcanzada por los 8 incrementos durante los 40 minutos

entrenamientoEnCinta :: Ejercicio
-- entrenamientoEnCinta minutos gimnasta = 
    -- quemarCalorias (calcularCalorias minutos (velocidadPromedio minutos 6.0)) gimnasta
entrenamientoEnCinta minutos = 
    quemarCalorias (calcularCalorias minutos (velocidadPromedio minutos 6.0))


velocidadPromedio :: Float -> Float -> Float
--velocidadPromedio minutos velocidadInicial = (velocidadInicial + (velocidadMaxima velocidadInicial minutos)) / 2.0
velocidadPromedio velocidadInicial = (/2.0).(+velocidadInicial).(velocidadMaxima velocidadInicial)


velocidadMaxima :: Float -> Float -> Float
--velocidadMaxima velocidadInicial minutos = velocidadInicial + (minutos / 5.0) 
velocidadMaxima velocidadInicial = (velocidadInicial+).(/5.0) 

-- Parte b
-- Las pesas tonifican la décima parte de los kilos a levantar si se realiza por más de 10 minutos, sino nada.

pesas :: Float -> Ejercicio
pesas kilos minutos gimnasta 
    | minutos > 10 = tonificar kilos gimnasta
    | otherwise = gimnasta 

tonificar :: Float -> Gimnasta -> Gimnasta 
tonificar kilos gimnasta = 
    gimnasta{coefTonificacion = (coefTonificacion gimnasta + (kilos/5.0))}

-- Parte c
-- La colina quema 2 calorías por minuto multiplicado por la inclinación de la colina. 

colina :: Float -> Ejercicio
-- colina inclinacion minutos gimnasta = quemarCalorias (2*minutos*inclinacion) gimnasta
colina inclinacion minutos= quemarCalorias (2*minutos*inclinacion)

-- Parte d
{-
La montaña son 2 colinas sucesivas 
(cada una con la mitad de duración respecto de los minutos totales indicados), 
donde la segunda colina tiene una inclinación de 3 más que la inclinación inicial elegida. 

Además de hacer perder peso por las calorías quemadas por las colinas, 
este ejercicio incrementa en una unidad la tonificación de la gimnasta. 
Resolver usando composición y aplicación parcial. 
-}

montania :: Float -> Float -> Gimnasta -> Gimnasta
montania inclinacion minutos = (tonificar 5).(colina (masTres inclinacion) (mitadDe minutos)).(colina inclinacion (mitadDe minutos)) 
--montania inclinacion minutos gimnasta = tonificar 5 (colina (inclinacion+3) (minutos/2.0) (colina inclinacion (minutos/2.0) gimnasta))

masTres :: Float -> Float
masTres = (+3)

mitadDe :: Float -> Float 
mitadDe = (/2.0)
-----------------------------------------------------------------------------------------------
-- Punto 4

{-
Dada una Rutina (es un Data con un nombre, duración total y lista de ejercicios específicos)
y un gimnasta, obtener al gimnasta luego de realizar la rutina. 
La cantidad de minutos dedicada a cada ejercicio es la misma. 
-}

data Rutina = UnaRutina{
    nombreRutina :: String,
    duracionTotal :: Float,
    ejercicios :: [Ejercicio]
}

-- Parte a
{-
i.Mostrar un ejemplo de uso usando todos los ejercicios del punto anterior. 
ii.Resolverlo usando recursividad.
iii.Hacer otra solución usando fold.
-}

-- i
ejerciciosPiolas :: [Ejercicio]
ejerciciosPiolas = [caminataEnCinta , entrenamientoEnCinta , pesas 50.0 , colina 5.0 , montania 5.0]

rutinaPiola = UnaRutina{
    nombreRutina = "Rutina Piola",
    duracionTotal = 40.0,
    ejercicios = [caminataEnCinta , entrenamientoEnCinta , pesas 50.0 , colina 5.0 , montania 5.0]
}

rutinaCorta = UnaRutina{
    nombreRutina = "Rutina Corta",
    duracionTotal = 5.0,
    ejercicios = [caminataEnCinta , pesas 10.0]
}

rutinaZarpada = UnaRutina{
    nombreRutina = "Rutina Zarpada",
    duracionTotal = 80.0,
    ejercicios = [caminataEnCinta , entrenamientoEnCinta , pesas 200.0 , colina 15.0 , montania 20.0]
}
-- ii
realizarRutina :: Rutina -> Gimnasta -> Gimnasta
--realizarRutina rutina gimnasta = arrancarRutina (duracionTotal rutina) (ejercicios rutina) gimnasta
realizarRutina rutina =
     arrancarRutina (duracionTotal rutina) (ejercicios rutina) 

arrancarRutina :: Float -> [Ejercicio] -> Gimnasta -> Gimnasta
arrancarRutina _ [] gimnasta = gimnasta
arrancarRutina minutos (cabezaEjercicio : colaEjercicio) gimnasta = arrancarRutina minutos colaEjercicio (cabezaEjercicio minutos gimnasta)
-- No me deja hacer point-free por la cantidad de argumentos :(

--iii
realizarRutinaFold :: Rutina -> Gimnasta -> Gimnasta
realizarRutinaFold rutina gimnasta = 
    foldr (hacerEjercicio (duracionTotal rutina)) gimnasta (ejercicios rutina)

hacerEjercicio :: Float -> Ejercicio -> Gimnasta -> Gimnasta
--hacerEjercicio minutos ejercicio gimnasta = ejercicio minutos gimnasta
hacerEjercicio minutos ejercicio = ejercicio minutos

--type Ejercicio =  Float -> Gimnasta -> Gimnasta
-----------------------------------------------------------------------------------------------
-- Punto 5

--Dada una lista de rutinas, obtener un resumen de todas las que (individualmente) 
--pueden llevar a un gimnasta dado a estar saludable

llegarASaludable :: Gimnasta -> [Rutina] -> [String]
--llegarASaludable gimnasta rutinas =  map nombreRutina (filter (rutinaSaludable gimnasta) rutinas)
llegarASaludable gimnasta = (map nombreRutina).(filter (`rutinaSaludable` gimnasta))

rutinaSaludable :: Rutina -> Gimnasta -> Bool
--rutinaSaludable gimnasta rutina = saludable (realizarRutina gimnasta rutina)
rutinaSaludable rutina = saludable.(realizarRutina rutina)

-- realizarRutina :: Rutina -> Gimnasta -> Gimnasta

rutinasDisponibles = [rutinaPiola,rutinaCorta,rutinaZarpada]

{- saludable :: Gimnasta -> Bool
saludable gimnasta = (not.esObeso) gimnasta && estaTonificado gimnasta -}
------------------------------------------------------------------------------------------------
