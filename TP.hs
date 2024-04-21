sueldoBasico :: String -> Float --Funcion para comparar strings, si no esta dentro de los 3 primeros valdra 0.
sueldoBasico "Titular" = 149000
sueldoBasico "Adjunto" = 116000
sueldoBasico "Ayudante" = 66000
sueldoBasico cargo = 0

incrementoAntiguedad :: Float -> Float -- Se ingresa la antiguedad y lo que se devuelve es el porcentaje total del incremento.
incrementoAntiguedad antiguedad 
    | 3 <= antiguedad && antiguedad < 5 = 120 --Se devuelve un 20%, 120% es el sueldo total mas ese porcentaje.
    | 5 <= antiguedad && antiguedad < 10 = 130 
    | 10 <= antiguedad && antiguedad < 24 = 150 
    | 24 <= antiguedad = 220 --Se devuelve un 120%, 220% es el sueldo mas ese porcentaje.
    | otherwise = 0

incrementoTotalSueldo :: String -> Float -> Float --Funcion que calcula el sueldo total con el cargo y el incremento de antiguedad.
incrementoTotalSueldo cargo antiguedad = (sueldoBasico cargo * incrementoAntiguedad antiguedad ) / 100

parteFraccionaria :: Float -> Float -- Funcion que nos retorna el valor decimal de un numero. 
parteFraccionaria numero = numero - fromIntegral (floor numero) --Floor nos redondea el numero siempre hacia abajo.

redondeoFraccionario :: Float -> Int --Funcion que decide como redondear los numeros.
redondeoFraccionario numero 
    | parteFraccionaria numero >= 0.5 = ceiling numero --Si es mayor o igual a 0,5 lo redondea para arriba
    | otherwise = floor numero --Si es menor, lo redondea para abajo.

cantidadHoras :: Float -> Int --Funcion para calcular el incremento de horas que se daran.
cantidadHoras horas
    | 5 <= horas && horas <= 50 = redondeoFraccionario (horas/10) --Si estan entre las 5 y 50 horas, se divide por 10 para obtener esas horas en su valor decimal  y se redondea.
    | otherwise = 0

sueldoTotal :: String -> Float -> Float -> Float --Funcion final para el calculo del sueldo con el cargo, antiguedad y las horas trabajadas
sueldoTotal cargo antiguedad horas = incrementoTotalSueldo cargo antiguedad * fromIntegral (cantidadHoras horas)
