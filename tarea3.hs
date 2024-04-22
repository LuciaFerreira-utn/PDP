type Pelicula = String 

data Persona = UnaPersona {
 nombre :: String,
 recibioOscar :: Bool,
 actuaciones :: [Actuacion]
 } deriving Show 

data Actuacion = UnaActuacion {
    pelicula :: Pelicula,
    valoracion :: Int
} deriving Show

--auxiliar
peliculasPremiadas :: [Pelicula]
peliculasPremiadas = ["Titanic", "El Padrino", "El Padrino II", "El Padrino III", "El Padrino IV"]

--funciones
primerActuacion :: Persona -> Actuacion
primerActuacion actor = head (actuaciones actor)

ultimaActuacion :: Persona -> Actuacion 
ultimaActuacion actor = last (actuaciones actor) 

valoracionPeliculaExito ::  Int -> Actuacion ->  Bool
valoracionPeliculaExito numExito actuacion = valoracion actuacion <= numExito

peliculasActor :: Persona -> [Actuacion]
peliculasActor actor = actuaciones actor

cantPeliculas :: [Actuacion] -> Int --revisar
cantPeliculas peliculas = length peliculas 

esPeliculaPremiada :: Actuacion -> Bool
esPeliculaPremiada actuacion = (pelicula actuacion) `elem` peliculasPremiadas
 
--primera fila
actorValoracion :: Persona -> Bool --revisar para mejorar
actorValoracion actor =  valoracionPeliculaExito 3 ( ultimaActuacion actor ) && cantPeliculas ( actuaciones actor ) > 1

recibiOscar :: Persona -> Bool
recibiOscar actor = ( recibioOscar actor == True ) || esPeliculaPremiada (primerActuacion actor) 

--seguir, lo hice con ayuda de apuntes
