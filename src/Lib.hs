module Lib where
import Text.Show.Functions

-- PARTE A --
data Cancion = Cancion {
    titulo :: String,
    genero :: String,
    duracion :: Int
} deriving Show

data Artista = Artista {
    nombre :: String,
    canciones :: [Cancion],
    efecto :: Efecto
} deriving Show

type Efecto = Cancion -> Cancion

mapDuracion :: (Int -> Int) -> Cancion -> Cancion
mapDuracion modificar unaCancion = unaCancion{duracion = (modificar.duracion) unaCancion}

mapGenero :: (String -> String) -> Cancion -> Cancion
mapGenero modificar unaCancion = unaCancion{genero = (modificar.genero) unaCancion}

mapTitulo :: (String -> String) -> Cancion -> Cancion
mapTitulo modificar unaCancion = unaCancion{titulo = (modificar.titulo) unaCancion}

cambiarGenero :: String -> Cancion -> Cancion
cambiarGenero generoNuevo unaCancion = unaCancion{genero = generoNuevo}

cambiarDuracion :: Int -> Cancion -> Cancion
cambiarDuracion duracionNueva unaCancion = unaCancion{duracion = duracionNueva}

esDelGenero :: String -> Cancion -> Bool
esDelGenero unGenero = (== unGenero).genero

acortar :: Efecto
acortar = mapDuracion (max 0).(-60) -- WTF HASKELL JUST WORK LIKE YOU ARE SUPPOSED TO

remixar :: Efecto
remixar = mapTitulo (++"remix").mapDuracion (*2).cambiarGenero "remixado"

acustizar :: Int -> Efecto
acustizar unaDuracion unaCancion
    | (not.(esDelGenero "acustico")) unaCancion = (cambiarGenero "acustico".cambiarDuracion unaDuracion) unaCancion
    | otherwise = unaCancion

efectoSupremo :: [Efecto] -> Efecto
efectoSupremo efectos = foldl (.) id efectos

cafeParaDos = Cancion "Cafe para dos" "rock melancolico" 146
fuiHastaAhi = Cancion "Fui hasta ahi" "rock" 279
-- losEscarabajos = Artista "Los Escarabajos" [rocketRaccoon, mientrasMiBateriaFesteja, tomateDeMadera] acortar
-- adela = Artista "Adela" [teAcordas, unPibeComoVos, daleMechaALaLluvia] remixar
-- elTigreJoaco = Artista "El Tigre Joaco" [] (acustizar 360)

-- PARTE B --
esCancionCorta :: Cancion -> Bool
esCancionCorta = (<150).duracion

cancionesCortas :: [Cancion] -> [Cancion]
cancionesCortas = filter esCancionCorta

vistazo :: Artista -> [Cancion]
vistazo = (take 3).cancionesCortas.canciones

cancionesDelGenero :: String -> [Cancion] -> [Cancion]
cancionesDelGenero unGenero = filter (esDelGenero unGenero)

playlist :: String -> [Artista] -> [Cancion]
playlist unGenero = concatMap (cancionesDelGenero unGenero.canciones)

-- PARTE C --
mapCanciones :: ([Cancion] -> [Cancion]) -> Artista -> Artista
mapCanciones modificar unArtista = unArtista{canciones = (modificar.canciones) unArtista}

hacerseDJ :: Artista -> Artista
hacerseDJ unArtista = mapCanciones (map (efecto unArtista)) unArtista

tieneMismoGeneroQue :: Cancion -> Cancion -> Bool
tieneMismoGeneroQue unaCancion otraCancion = esDelGenero (genero unaCancion) otraCancion

todasSonDelMismoGenero :: [Cancion] -> Bool
todasSonDelMismoGenero canciones = all (tieneMismoGeneroQue (head canciones)) canciones

tieneGustoHomogeneo :: Artista -> Bool
tieneGustoHomogeneo = todasSonDelMismoGenero.canciones

todosLosEfectos :: [Artista] -> Efecto
todosLosEfectos = efectoSupremo.(map efecto)

cancionesDeIntegrantes :: [Artista] -> [Cancion]
cancionesDeIntegrantes = concatMap canciones

formarBanda :: String -> [Artista] -> Artista
formarBanda nombre integrantes = Artista nombre (cancionesDeIntegrantes integrantes) (todosLosEfectos integrantes)

mapCancionesDeArtistaSegun :: ([Cancion] -> a) -> Artista -> a
mapCancionesDeArtistaSegun accion = accion.canciones

titulos :: [Cancion] -> [String]
titulos = map titulo

duraciones :: [Cancion] -> [Int]
duraciones = map duracion

generos :: [Cancion] -> [String]
generos = map genero

crearGranTitulo :: [Cancion] -> String
crearGranTitulo = concat.titulos

sumaDeDuraciones :: [Cancion] -> Int
sumaDeDuraciones = sum.duraciones

generoSuperador :: Artista -> String
generoSuperador = foldl1 mejorGenero.generos.canciones

obraMaestraProgresiva :: Artista -> Cancion
obraMaestraProgresiva unArtista = Cancion (mapCancionesDeArtistaSegun crearGranTitulo unArtista) (generoSuperador unArtista) (mapCancionesDeArtistaSegun sumaDeDuraciones unArtista)

mejorGenero :: String -> String -> String
mejorGenero "rock" _ = "rock"
mejorGenero _ "rock" = "rock"
mejorGenero unGenero "reggaeton" = unGenero
mejorGenero "reggaeton" otroGenero = otroGenero
mejorGenero unGenero otroGenero = mejorGeneroPorLetras unGenero otroGenero

mejorGeneroPorLetras :: String -> String -> String
mejorGeneroPorLetras unGenero otroGenero
    | length unGenero > length otroGenero = unGenero
    | otherwise = otroGenero

{-
PARTE 4
a) No, ya que estará transformando a todas las canciones de una lista infinita y no terminará de evaluar.
b) Si, toma las primeras 3 canciones cortas que encuentre.
c) No, necesita evaluar toda la lista para, por ejemplo, sumar todas sus duraciones. 
-}
