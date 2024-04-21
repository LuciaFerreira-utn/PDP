type Altura = Int
type Peso = Int
type Utilidad = Bool
type Costo = Int

pesoPino :: Altura -> Peso
pesoPino altura 
    | 0 < altura && altura <= 300 = altura * 3
    | 300 < altura = 300 * 3 + (altura - 300) * 2
    | otherwise = 0 -- no me acuerdo el mensaje de error

pesoUtil :: Peso -> Utilidad
pesoUtil peso = 400 <= peso && peso <= 1000 

sirvePino :: Altura -> Utilidad
sirvePino altura = pesoUtil(pesoPino altura)

costoTransporte :: Altura -> Costo
costoTransporte altura
    | pesoPino altura <= 600 = 5000
    | 600 < pesoPino altura &&  pesoPino altura <= 800 = 10 * pesoPino altura
    | pesoPino altura > 800 = (10 * pesoPino altura) + altura
