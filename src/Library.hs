module Library where
import PdePreludat


data Persona = Persona {
    edad :: Number,
    suenios :: [Suenio],
    nombre :: String,
    felicidonios :: Number,
    habilidades :: [String]
}

--Punto 1.a
coeficienteDeSatisfaccion persona 
    | felicidonios persona > 100 = felicidonios persona * edad persona
    | felicidonios persona > 50  = felicidonios persona * cantidadDeSuenios persona
    | otherwise = felicidonios persona `div` 2

cantidadDeSuenios = length . suenios

--Punto 1.b
gradoDeAmbicion persona 
    | felicidonios persona > 100 = felicidonios persona * cantidadDeSuenios persona
    | felicidonios persona > 50  = cantidadDeSuenios persona * edad persona
    | otherwise = cantidadDeSuenios persona * 2

--Punto 2.a
tieneNombreLargo = (>10) . length . nombre

--Punto 2.b
esSuertuda = even . coeficienteDeSatisfaccion

--Punto 2.c
tieneNombreLindo = (=='a') . last . nombre

--Punto 3.a
type Suenio = Persona -> Persona
recibirse :: String -> Suenio
recibirse carrera = 
    aumentarFelicidonios (15 * length carrera) . agregarHabilidad carrera
viajarA :: [String] -> Suenio
viajarA ciudades = aumentarFelicidonios (100 * length ciudades) . cumplirAnios
enamorarseDe :: Persona -> Suenio
enamorarseDe otraPersona = aumentarFelicidonios (felicidonios otraPersona)

aumentarFelicidonios cantidad persona = persona {
    felicidonios = felicidonios persona + cantidad
} 
cumplirAnios persona = persona { edad = edad persona + 1 }
agregarHabilidad habilidad persona = 
    persona { habilidades = habilidad : habilidades persona }

queTodoSigaIgual :: Suenio
queTodoSigaIgual = id

comboPerfecto = 
    viajarA ["Watcher's Hills", "París"] . 
    recibirse "Medicina" . 
    aumentarFelicidonios 100
 
--Punto 4
type Fuente = Persona -> Persona
fuenteMinimalista :: Fuente
fuenteMinimalista persona = suenioCumplido . (head.suenios) persona $ persona
suenioCumplido persona = persona { suenios = tail . suenios $ persona }

fuenteCopada :: Fuente
fuenteCopada persona = foldl (\p s ->suenioCumplido $ s p) persona (suenios persona)

