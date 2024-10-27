{-Ejercicio 1-}

promedio x y z =( x + y + z ) / 3

{-Ejercicio 2-}

sumaMonedas m1 m2 m3 m4 m5 =  ((m1 * 1) + (m2 * 2) + (m3 * 5) + (m4 * 10) + (m5 * 20))

{-Ejercicio 3-}

volumenEsfera r =  (4/3) * pi * r**3

{-Ejercicio 4-}

areaCorona r1 r2 = pi * (r2**2 - r1**2)

{-Ejercicio 5-}

ultimaCifra n = rem n 10


{-Ejercicio 6-}

maxTres n1 n2 n3 = max n1 (max n2 n3)

{-Ejercicio 7-}
 
rotar1 xs = tail xs ++ [head xs]

{-Ejercicio 8-}

rotar2 n xs = drop n xs ++ take n xs

{-Ejercicio 9-}

rango xs = [minimum xs , maximum xs]

{-Ejercicio 10-}

palindromo xs = xs == reverse xs

{-Ejercicio 11-}

interior xs = tail(init xs)

{-Ejercicio 13-}

segmento m n xs = take (n - m) (drop m xs)

{-Ejercicio 14-}

extremo n xs = take n xs ++ drop (length xs - n) xs

{------------------------------------------------------------------------------------}

{-Ejercicio 15-}

mediano x y z = (x + y + z) - (min x (min y z)) - (max x (max y z))

{-Ejercicio 16-}

tresIguales x y z = (x == y) && (y == z)

{-Ejercicio 17-}

tresDiferentes x y z = (x /= y) && (x /= z) && (y /= z)

{-Ejercicio 18-}

cuatroIguales x y z u = (tresIguales x y z) && (x == u)

{---------------------------------GUARGAS Y PATRONES--------------------------------}


{-Ejercicio 1-}

divivisionSegura x y
 |  y == 0 = 9999.0
 | otherwise = (x / y)

{-Ejercicio 2-}

xor1 x y = (x || y ) && not (x && y)

{-Ejercicio 3-}

mayorRectangulo (a1 , h1) (a2 , h2)
 | (a1 * h1) >= (a2 * h2) = (a1 , h1)
 |otherwise               = (a2,h2)

{-Ejercicio 4-}

intercambia (x , y) = (y ,x)


{-Ejercicio 5-}

distancia (x1 , y1) (x2 , y2) = sqrt((x2 - x1)**2 + (y2 - y1)**2)

{-Ejercicio 6-}

ciclo [] = []
ciclo xs = last xs : init xs

{-Ejercicio 7-}

numeroMayor x y = max(10 * x + y)(10 * y + x)

{-Ejercicio 8-}

numeroRaices a b c 
 | discriminante > 0 = 2
 | discriminante  == 0 = 1
 | otherwise       = 0
 where
 discriminante = (b**2) - 4 * a * c

{-Ejercicio 9-}

raices a b c
  | discriminante < 0 = []
  | discriminante == 0 = [-b / (2*a)]
  | otherwise = [(-b + sqrt discriminante) / (2*a), (-b - sqrt discriminante) / (2*a)]
  where
    discriminante = b^2 - 4*a*c
{-Ejercicio 10-}

area a b c = (a + b + c) / 2

{-Ejercicio 11-}

interseccion [] _ = []
interseccion  _ [] = []
interseccion [x1 , x2] [y1 , y2]
 | x2 < y1 = []
 | y2 < x1 = []
 | otherwise = [max x1 y1 , min x2 y2]


{-Ejercicio 12-}

linea n | n <= 0 = error ""
        | n == 1 = [1]
        | otherwise = let
            p = (n * (n - 1)) `div` 2 + 1
            u = n * (n + 1) `div` 2
        in p : linea (n - 1) ++ [u]
{--------------------------RECURSIVIDAD------------------------}


{-Ejercicio 1-}

potencia x n 
 | n == 0 = 1
 | otherwise = x * potencia x (n - 1)


{-Ejercicio 2-}

mcd a b
 | b == 0 = a
 | otherwise = mcd b (a `mod` b)

{-Ejercicio 3-}

pertenece x [] = False
pertenece x (y : ys) = x == y || pertenece x ys

{-Ejercicio 4-}

tomar 0 _ = []
tomar n (x:xs) = x : tomar (n -1) xs

{-Ejercicio 5-}

digitosC 0 = []
digitosC n = (mod n 10) : digitosC (div n 10)


{-Ejercicio 6-}

sumaDigitosR n
 | n < 10 = n 
 | otherwise = (n `mod` 10) + sumaDigitosR (n `div` 10)
{-Ejercicio 7-}

ordenaRapida [] = []
ordenaRapida (x:xs) = 
 let menores = [y | y <- xs , y <= x]
     mayores = [y | y <- xs , y > x]
 in ordenaRapida menores ++ [x] ++ ordenaRapida mayores
{-------------------NUEVOS TIPOS DE DATOS-------------------}


{------------------ARBOL--------------------}


data Arbol a = Hoja | Nodo a (Arbol a) (Arbol a) deriving (Eq, Show)
generarNodo :: a -> Arbol a
generarNodo x  = Nodo x Hoja Hoja

insertar x Hoja = generarNodo x
insertar x (Nodo a izq der)
 | x < a = Nodo a (insertar x izq) der
 | x > a = Nodo a izq (insertar x der)
 | otherwise = Nodo a izq der

insertarLista [] arbol = arbol
insertarLista (x:xs) arbol = insertarLista xs arbolNuevo
 where
  arbolNuevo = insertar x arbol

buscarArbol x Hoja = False
buscarArbol x (Nodo a izq der)
 | x == a = True
 | x < a = buscarArbol x izq
 | x > a = buscarArbol x der

inOrden Hoja = []
inOrden (Nodo a izq der) = inOrden izq ++ [a] ++ inOrden der

preOrden Hoja = []
preOrden (Nodo a izq der) = [a] ++ preOrden izq ++ preOrden der

postOrden Hoja = []
postOrden (Nodo a izq der) = postOrden izq ++ postOrden der ++ [a]
