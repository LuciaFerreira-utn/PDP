letrasIntermedias :: String -> Int
letrasIntermedias palabra = length ( init (tail palabra) )

esDiv :: String -> Int -> Bool
esDiv empresa divisor = mod (length empresa) divisor == 0

nombreAEmpleados :: String -> Int
nombreAEmpleados nombre
    | nombre == "Acme" = 10
    | last nombre < head nombre = letrasIntermedias nombre
    | nombre == reverse nombre && esDiv nombre 2 = 2 * letrasIntermedias nombre
    | esDiv nombre 3 || esDiv nombre 7 = 3
    | otherwise = 0
