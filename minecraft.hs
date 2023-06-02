data Personaje = UnPersonaje {

        nombre:: String,

        puntaje:: Int,

        inventario:: [Material]

} deriving Show

type Material = String

data Receta = UnaReceta {
    materiales :: [Material],
    tiempo :: Int,
    resultado :: Material
} deriving Show


fogata,fosforo, madera,polloAsado,pollo,sueter,hielo,lobos,iglues :: Material
fogata = "fogata"
fosforo = "fosforo"
madera = "madera"
pollo = "pollo"
polloAsado = "pollo asado"
sueter = "sueter"
hielo = "hielo"
iglues = "iglues"
lobos = "lobos"

intentarCrafteo :: Receta -> Personaje -> Personaje
intentarCrafteo receta jugador 
    | tieneMateriales (materiales receta) jugador = craftear receta jugador
    | otherwise = modificarPuntaje (-100) jugador

craftear :: Receta -> Personaje -> Personaje
craftear receta  = modificarPuntaje (tiempo receta * 10) . agregarMaterial (resultado receta) . quitarMateriales (materiales receta)

modificarPuntaje :: Int -> Personaje -> Personaje
modificarPuntaje n jugador = jugador {puntaje = puntaje jugador + n}

agregarMaterial :: Material -> Personaje -> Personaje
agregarMaterial material jugador = jugador {inventario = material : inventario jugador}

quitarMateriales :: [Material] -> Personaje -> Personaje
quitarMateriales materiales jugador = jugador {inventario = foldr quitarUnaVez (inventario jugador) materiales}

quitarUnaVez :: Material -> [Material] -> [Material]
quitarUnaVez _ [] = []
quitarUnaVez material (m:ms)
    | material == m = ms
    | otherwise = m:quitarUnaVez material ms

-- Auxiliares --
tieneMateriales :: [Material] -> Personaje -> Bool
tieneMateriales materiales jugador = all (tieneMaterial jugador) materiales

tieneMaterial :: Personaje -> Material -> Bool
tieneMaterial jugador material = elem material (inventario jugador)

recetaFogata :: Receta
recetaFogata = UnaReceta [madera, fosforo] 10 fogata
recetaPollo :: Receta
recetaPollo = UnaReceta [fogata, madera] 10 polloAsado

objetosCrafteables :: Personaje -> [Receta] -> [Material]
objetosCrafteables jugador recetas = map resultado (filter (puntajeDuplicable jugador) recetas)

puntajeDuplicable :: Personaje -> Receta -> Bool
puntajeDuplicable jugador receta = puntaje (intentarCrafteo receta jugador) > 2*(puntaje jugador)

craftearSucesivamente :: Personaje -> [Receta] -> Personaje
craftearSucesivamente = foldr intentarCrafteo 

mayorPuntaje :: Personaje -> [Receta] -> Bool
mayorPuntaje jugador recetas = puntaje (craftearSucesivamente jugador recetas) > puntaje (craftearSucesivamente jugador (reverse recetas))

--------------------------------------------------------------------

data Bioma = UnBioma {
    materialesBioma :: [Material],
    materialNecesario :: Material
} deriving Show

type Herramienta = [Material] -> Material

hacha, espada :: Herramienta
hacha = last
espada = head
pico :: Int -> Herramienta
pico = flip (!!)

minar :: Herramienta -> Personaje -> Bioma -> Personaje
minar herramienta jugador bioma 
    | tieneMaterial jugador (materialNecesario bioma) = (modificarPuntaje 50 . agregarMaterial (herramienta (materialesBioma bioma))) jugador
    | otherwise = id jugador

infinitosMateriales :: [String]
infinitosMateriales = pollo : infinitosMateriales
juan :: Personaje
juan = UnPersonaje {nombre = "juan", puntaje = 1050, inventario = ["pollo","madera","fosforo","pollo crudo","sueter", fogata]}
