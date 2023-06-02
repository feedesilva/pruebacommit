data Pais = UnPais{
    ipc :: Float,
    pobPublica :: Int,
    pobPrivada :: Int,
    recursos :: [String],
    deuda :: Float
} deriving (Show, Eq)

type Receta = Pais -> Pais

prestamos :: Float -> Receta
prestamos millones pais 
    = pais {deuda = deuda pais + millones * 1.5}
reducirPublico :: Int -> Receta
reducirPublico x pais      
    | x > 100 = pais {ipc = ipc pais * 0.8, pobPublica = pobPublica pais - x}
    | otherwise = pais {ipc = ipc pais * 0.85, pobPublica = pobPublica pais - x}
explotarRecursos :: String -> Receta
explotarRecursos recurso pais 
    = pais {deuda = deuda pais - 2.0, recursos = filter (/= recurso) (recursos pais)} 
blindar :: Receta
blindar pais
    = (reducirPublico 500 . prestamos ((ipc pais * (fromIntegral(pobPrivada pais + pobPublica pais))/2)))  pais 
nuevaReceta :: Receta
nuevaReceta     
    = prestamos 200 . explotarRecursos "mineria"

aplicarReceta :: [Receta] -> Receta
aplicarReceta recetas pais 
    = foldl (flip ($)) pais recetas

puedeZafar :: [Pais] -> [Pais]
puedeZafar  
    = filter (elem "Petroleo" . recursos)

deudaTot :: [Pais] -> Float
deudaTot  
    = foldr ((+).deuda) 0

recursosNaturalesInfinitos :: [String]
recursosNaturalesInfinitos = "Energia" : recursosNaturalesInfinitos

-- Ejemplos --
namibia :: Pais
namibia = UnPais 4140 400000 650000 ["mineria", "ecoturismo"] 50.0
