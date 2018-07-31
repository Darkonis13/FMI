--PUNTO 1
data Pais = Pais{ingresoPerCapita::Int, poblacionActivaSectorPublico::Int, poblacionActivaSectorPrivado::Int, recursosNaturales::[String], deudaConFMI::Int}

namibia = Pais{ingresoPerCapita=4140, poblacionActivaSectorPublico= 400000, poblacionActivaSectorPrivado= 650000, recursosNaturales=["Minería","Ecoturismo"], deudaConFMI=50000000}

--PUNTO 2
prestarDinero :: Int->Pais->Pais
prestarDinero cantidadDeDinero = modificarDeuda (+ (cantidadDeDinero * 1.5))

modificarDeuda:: (Int->Int)->Pais->Pais
modificarDeuda funcionModificadora unPais = unPais{deudaConFMI = funcionModificadora.deudaConFMI $ unPais}


reducirPuestosDeTrabajoPublico unPais cantidadDePuestos

    | poblacionActivaSectorPublico unPais > 100 = disminuirPuestosYIngreso unPais cantidadDePuestos 0.2
    | otherwise = disminuirPuestosYIngreso unPais cantidadDePuestos 0.15
reducirPuestosDeTrabajoPublico :: Pais->Int->Pais

disminuirPuestosYIngreso :: (Ord a) => Pais->Int->a->Pais
disminuirPuestosYIngreso unPais cantidadDePuestos unPorcentaje = unPais{poblacionActivaSectorPublico = poblacionActivaSectorPublico unPais - cantidadDePuestos, ingresoPerCapita= ingresoPerCapita unPais - ingresoPerCapita unPais * unPorcentaje}

asignarExplotacionDeRecursosAUnaEmpresa :: String->Pais->Pais
asignarExplotacionDeRecursosAUnaEmpresa recurso = dejarSinRecurso recurso . modificarDeuda ((-)2000000)

dejarSinRecurso::String->Pais->Pais
dejarSinRecurso recurso unPais = unPais{recursosNaturales = filter (/=recurso) (recursosNaturales unPais)}

establecerBlindaje :: Pais->Pais
establecerBlindaje unPais = flip reducirPuestosDeTrabajoPublico 500 . modificarDeuda ((+) $ (pBI unPais)*0.5) $ unPais

pBI:: Pais->Int
pBI unPais = ingresoPerCapita unPais * (poblacionActivaSectorPublico unPais + poblacionActivaSectorPrivado unPais)

--PUNTO 3
recetaFMI = modificarDeuda ((+) 200000000) . asignarExplotacionDeRecursosAUnaEmpresa "Minería"