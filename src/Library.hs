module Library where
import PdePreludat

{-------------------------------------PUNTO 1-------------------------------------}
data Pais = UnPais{
    ingresoPerCapita :: Number
,   poblacionSectorPublico :: Number
,   poblacionSectorPrivado :: Number
,   recursosNaturales :: [String]
,   deuda :: Number
} deriving (Eq,Show)

namibia = UnPais 4140 400000 650000 ["Mineria","Ecoturismo"] 50000000
usa = UnPais 15250 500000 30000000 ["Mineria","Ecoturismo"] 1000000

{-------------------------------------PUNTO 2-------------------------------------}
type Estrategia = Pais -> Pais

aplicarEstrategia ::  Pais -> Estrategia -> Pais
aplicarEstrategia pais estrategia = estrategia pais

prestarNMillones :: Number -> Estrategia
prestarNMillones n = endeudarse (n*1000000*1.5)

endeudarse :: Number -> Pais -> Pais
endeudarse cantidad pais = pais {deuda=deuda pais + cantidad}

reducirXPuestosPublicos :: Number -> Estrategia
reducirXPuestosPublicos x | x > 100 = reducirPuestosPublicos x.modificarIngresoPerCapita 0.8 
                          | otherwise = reducirPuestosPublicos x.modificarIngresoPerCapita 0.85

modificarIngresoPerCapita :: Number -> Pais -> Pais
modificarIngresoPerCapita porcentaje pais = pais {ingresoPerCapita = ingresoPerCapita pais * porcentaje}

darAEmpresa :: String -> Estrategia
darAEmpresa recurso pais = (perderRecursoNatural recurso.endeudarse (-2000000)) pais

perderRecursoNatural :: String -> Pais -> Pais
perderRecursoNatural recurso pais = pais {recursosNaturales=filter (not.(==recurso)) (recursosNaturales pais)}

establecerBlindaje :: Estrategia
establecerBlindaje pais = (reducirPuestosPublicos 500.endeudarse (0.5*(calcularPBI pais))) pais

reducirPuestosPublicos :: Number -> Pais -> Pais
reducirPuestosPublicos cantidad pais = pais {poblacionSectorPublico=poblacionSectorPublico pais - cantidad}

calcularPBI :: Pais -> Number
calcularPBI pais = (poblacionSectorPublico pais + poblacionSectorPrivado pais) * (ingresoPerCapita pais)

{-------------------------------------PUNTO 3-------------------------------------}
type Receta = [Estrategia]
recetaA = [prestarNMillones 200,darAEmpresa "Mineria"]
recetaB = [reducirXPuestosPublicos 20000,darAEmpresa "Mineria"]
recetaC = [establecerBlindaje]
recetas = [recetaA,recetaB,recetaC]

aplicarReceta :: Receta -> Pais -> Pais
aplicarReceta receta pais = foldl aplicarEstrategia pais receta

--aplicarReceta recetaA namibia

{-------------------------------------PUNTO 4-------------------------------------}
puedenZafar :: [Pais] -> [Pais]
puedenZafar = filter zafa

zafa :: Pais -> Bool
zafa = elem "Petroleo".recursosNaturales

totalDeuda :: [Pais] -> Number
totalDeuda paises = foldl (\x y -> x+deuda y) 0 paises

{-------------------------------------PUNTO 5-------------------------------------}
estaOrdenada :: Pais -> [Receta] -> Bool
estaOrdenada _ (receta1:[]) = True
estaOrdenada pais (receta1:(receta2:recetas)) | deMenorAMayor pais receta1 receta2 = estaOrdenada pais (receta2:recetas)
                                              | otherwise = False

deMenorAMayor :: Pais -> Receta -> Receta -> Bool
deMenorAMayor pais receta1 receta2 = calcularPBI (aplicarReceta receta1 pais) < calcularPBI (aplicarReceta receta2 pais)

{-------------------------------------PUNTO 6-------------------------------------}

{-
(1 punto) Si un país tiene infinitos recursos naturales, modelado con esta función
recursosNaturalesInfinitos :: [String]
recursosNaturalesInfinitos = "Energia" : recursosNaturalesInfinitos
¿qué sucede evaluamos la función 4a con ese país? 
¿y con la 4b?
Justifique ambos puntos relacionándolos con algún concepto.
--Si evaluamos la 4a, va a quedar en un bucle infinito intentando buscar paises que tengan petróleo, pero nunca va a encontrar y
la lista que devuelve seguirá sin existir.
--En el 4b comenzará a sumar infinitamente la deuda de los países y no terminará nunca.
-}




