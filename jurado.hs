import Text.Show.Functions
data Ley = UnaLey {
    tema :: String,
    presupuesto :: Int,
    sectores :: [String]
} deriving (Show)

agenda = ["Cannabis en Medicina", "Educacion Superior", "Tenis de Mesa", "Tenis"]

usoMedicinal, educacionSuperior, profesionalizaciónTenistaMesa, tenis    :: Ley
usoMedicinal = UnaLey "Cannabis en Medicina" 5 ["partido cambio de todos", "sector financiero"]
educacionSuperior = UnaLey "Educacion Superior" 30 ["docentes universitarios", "partido centro federal"]
profesionalizaciónTenistaMesa = UnaLey "Tenis de Mesa" 1 ["partido centro federal", "liga deportistas autonomos", "club paleta veloz"]
tenis = UnaLey "Tenis" 2 ["liga deportistas autonomos"]

sonCompatibles :: Ley -> Ley -> Bool
sonCompatibles l1 l2 
    =  any (`elem` (sectores l1))  (sectores l2) && (substring (tema l1) (tema l2  ) || substring (tema l2) (tema l1))


substring string1 "" = False 
substring palabra1 (letra2:palabra2) = palabra1 == (take (length palabra1) palabra2) || substring palabra1 palabra2 

type Juez = Ley -> Bool
type CorteSuprema = [Juez]
supremaCorte = [juezPublica, juezFinanciero, juezConservador, juezPresupuesto 10, juezPresupuesto 20]

juezPublica :: Juez
juezPublica ley = elem (tema ley) agenda
juezFinanciero, juezConservador :: Juez
juezFinanciero ley = elSectorApoya "sector financiero" ley
juezConservador ley = elSectorApoya "partido conservador" ley
juezPresupuesto :: Int -> Juez
juezPresupuesto valor ley = presupuesto ley <= valor 

elSectorApoya :: String -> Juez
elSectorApoya sector ley = elem sector (sectores ley)

juezPositivo :: Juez
juezPositivo _ = True

juezInventado :: Juez
juezInventado ley = elSectorApoya "Universidad Tecnologica Nacional" ley && juezPresupuesto 5 ley

juezUltimo :: Juez
juezUltimo = juezPresupuesto 15
-----------------------------------------------------

votosCorte :: [Ley] -> CorteSuprema -> [Ley]
votosCorte leyes jueces = filter (sonAprobadas jueces) leyes

sonAprobadas :: CorteSuprema ->Ley -> Bool
sonAprobadas jueces ley = (votosPositivos ley jueces) >= mayoria jueces

votosPositivos :: Ley -> CorteSuprema -> Int
votosPositivos ley jueces = length (filter (votar ley) jueces)
    where votar ley juez = juez ley
mayoria :: CorteSuprema -> Int
mayoria jueces = length jueces `div` 2
{-
votar :: Ley -> Juez -> Bool
votar ley juez = juez ley
-}
nuevaCorte = supremaCorte ++ nuevosJueces
nuevosJueces = [juezPositivo, juezInventado, juezUltimo]

antesNoPeroAhoraSi :: [Ley] -> CorteSuprema -> CorteSuprema -> [Ley]
antesNoPeroAhoraSi leyes corte nuevos = filter (ahoraSi corte nuevos) leyes

ahoraSi :: CorteSuprema -> CorteSuprema -> Ley -> Bool
ahoraSi corte nuevos ley = (not (sonAprobadas corte ley)) && (sonAprobadas nuevos ley)

borocotizar :: CorteSuprema -> CorteSuprema
borocotizar  = map (not.) 

posicionSocial :: String -> Juez -> [Ley] -> Bool
posicionSocial sector juez leyes = all (elSectorApoya sector) (leyesVota juez leyes)

leyesVota :: Juez -> [Ley] -> [Ley]
leyesVota juez leyes = filter (\ley -> juez ley) leyes