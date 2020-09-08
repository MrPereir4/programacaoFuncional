--DUPLA : Vinnicius Pereira e Vitor Borges
--1-
triangulo :: (Int, Int, Int) -> String
triangulo (x, y, z)
    | x + y + z > 180 = "Nao_triangulo"
    | x == y && x == z && y == z = "Equilatero"
    | x == 90 || y == 90 || z == 90 = "Retangulo"
    | x > 90 || y > 90 || z > 90 = "Obtuso"
    | otherwise = "Simples"
    
--2-

equacao :: (Float, Float, Float) -> (Float, Float)
equacao (a, b, c)
    | a == 0 = (negate(c) / b, a)
    | otherwise = ((negate(b) + sqrt(b**2 - 4 * a * c)) / 2 * a, (negate(b) - sqrt(b**2 - 4 * a * c)) / 2 * a)
    
--3-

type Data = (Int, Int, Int)

bissexto :: Int -> Bool
bissexto x
    | mod x 100 == 0 = False
    | mod x 4 == 0 = True
    | otherwise = False

valida :: Data -> Bool
valida (x, y, z)
    | x > 30 && y /= 1  && y /= 3 && y /= 5 && y /= 7 && y /= 8 && y /= 10 && y /= 12 = False
    | bissexto z == False && y == 2 && x > 28 = False
    | otherwise = True
    
    
difData :: (Data, Data) -> Int
difData ((x1, y1, z1), (x2, y2, z2))
    | valida (x1, y1, z1) == False || valida (x2, y2, z2) == False = -1
    | y2 > y1 = z1 - z2 - 1
    | y2 < y1 = z1 - z2
    | x2 > x1 = z1 - z2 - 1
    | x2 <= x1 = z1 - z2
    
    
precoOnibus :: (Float, Data, Data) -> Float
precoOnibus (x, y, z)
    | difData(y, z) <= 2 = 0.15 * x
    | difData(y, z) <= 10 = 0.4 * x
    | difData(y, z) >= 70 = 0.5 * x
    | otherwise = x
    
--4-

gera1 :: [Int]
gera1 = [ x*x | x <- [1..15], odd x, x >=4, x<=14]

gera2 :: [(Int, Int)]
gera2 = [(x, y) | x <- [1..4], y <- [x..x*2]]

l1 = [10..15]
gera3 :: [Int]
gera3 = [y | x <- l1, y <- [1..x]]

gera4 :: [(Int, Int)]
gera4 = [ (x, y) | x <- [1..16], y <- [x+1], odd x]

gera5 :: [Int]
gera5 = [ x + y | (x, y) <- gera4]

--5-

    contaNegM2 :: [Int] -> Int
    contaNegM2 [] = 0
    contaNegM2 (x:xs)
        | x < 0 && even x = (contaNegM2 xs) +1
        | otherwise = (contaNegM2 xs)
        

    listaNegM2 :: [Int] -> [Int]
    listaNegM2 xs = [x | x <- xs, x < 0, even x]


--6-

    distancias :: [(Float, Float)] -> [Float]
    distancias lst = [sqrt (x^2 + y^2) | (x, y) <- lst]
    
--7-

    fatores n = [i | i<-[1..n], mod n i == 0]
    primos :: Int -> Int -> [Int]
    primos xs ys = [x | x <- [xs..ys], (fatores x) == [1, x]]

--8-

    mdc :: Int -> Int -> Int
    mdc x y
        | x < y = mdc y x
        | y == 0 = x
        | otherwise = mdc y (mod x y)
        
    mmc2 :: Int -> Int -> Int
    mmc2 x y = (x * y) `div` (mdc x y)
    mmc :: Int -> Int -> Int -> Int
    mmc x y z = mmc2 x (mmc2 y z)
    
--9-
    serie :: Float -> Int -> Float
    serie x n
     | n == 1 = 1 / x
     | even n = (x / fromIntegral(n)) + (serie x (n-1))
     | otherwise = (fromIntegral(n) / x) + (serie x (n-1))
     
--10-

    fb :: Int -> String
    fb x
        | (mod x 3 == 0) && (mod x 5 == 0) = "FizzBuzz"
        | (mod x 3 == 0) = "Fizz"
        | (mod x 5 == 0) = "Buzz"
        | otherwise = "No"

    fizzbuzz :: Int -> [String]
    fizzbuzz x = [fb x | x <- [1..x]]
    
--11-PROBLEMAS ao retornar cmo dentro de dupla

    conta_ocorrencia :: Int -> Int -> [Int] -> (Int, Int)
    conta_ocorrencia _ _ [] = (0, 0)
    conta_ocorrencia x y (z:zs)
        | x == z && y == z = (conta_ocorrencia x y zs) +1
        | x == z = (conta_ocorrencia x y zs) +1
        | y == z = (conta_ocorrencia x y zs) +1
        | otherwise = (conta_ocorrencia x y zs)
--12-

    conta_ocorrencia_unica :: Int -> [Int] -> Int
    conta_ocorrencia_unica _ [] = 0
    conta_ocorrencia_unica x (y: ys) = if (x == y) then (conta_ocorrencia_unica x ys)+1 else (conta_ocorrencia_unica x ys)

    unica_ocorrencia :: Int -> [Int] -> Bool
    unica_ocorrencia x [] = False
    unica_ocorrencia x xs = if (conta_ocorrencia_unica x xs == 1) then True else False

--13-

    intercala :: [Int] -> [Int] -> [Int]
    intercala [] xs = xs
    intercala xs [] = xs
    intercala (x:xs) ys = [x] ++ intercala ys xs

--14-

type Contato = (String, String, String, String)

recNome :: String -> [Contato] -> String
recNome _ [] = "Email desconhecido"
recNome x ((n, _, _, em):xs) = if (x == em) then n else (recNome x xs)

--15-

    tamanho :: [Pessoa] -> Float
    tamanho [] = 0
    tamanho (x:xs) = 1 + (tamanho xs)

    altTotal :: [Pessoa] -> Float
    altTotal [] = 0
    altTotal ((_, a, _, _):xs) = (a + altTotal xs)

    altMedia :: [Pessoa] -> Float
    altMedia x = (altTotal x) / tamanho x
    
    maisNova :: [Pessoa] -> Int
    maisNova x = minimum [i | (_, _, i, _) <- x]


    maisVelho :: [Pessoa] -> String
    maisVelho [(n, _, _, e)] = n ++ "(" ++ [e] ++ ")"
    maisVelho ((n1, a1, i1, e1):(n2, a2, i2, e2):xs)
        | i1 > i2 = maisVelho ((n1, a1, i1, e1):xs)
        | otherwise = maisVelho ((n2, a2, i2, e2):xs)
        
        
    maisCinq :: [Pessoa] -> [Pessoa]
    maisCinq [] = []
    maisCinq x = [(n2, a2, i2, e2) | (n2, a2, i2, e2) <- x, i2 >= 50]
    
    casadas :: Int -> [Pessoa] -> [Pessoa]
    casadas _ [] = []
    casadas i x = [(n2, a2, i2, e2) | (n2, a2, i2, e2) <- x, i2 >= i, e2 == 'C']

--16-

    insere_ord :: Ord a => a -> [a] -> [a]
    insere_ord _ [] = []
    insere_ord elmnt (x:xs) = if (elmnt > x) then [x] ++ (insere_ord elmnt xs) else [elmnt] ++ (x:xs)

--17-

    reverte :: [a] -> [a]
    reverte [] = []
    reverte (x:xs) = reverte xs ++ [x]

--18-

    membro :: Eq a => a -> [a] -> Bool
    membro _ [] = False
    membro a (x:xs) = if(a == x) then True else membro a xs

    sem_repetidos :: Eq a => [a] -> [a]
    sem_repetidos [] = []
    sem_repetidos (x:xs) = if (membro x xs) then (sem_repetidos xs) else [x] ++ (sem_repetidos xs)

--19-

    disponiveis :: [Int]
    disponiveis = [1,2,5,10,20,50,100]
    notasTroco :: Int -> [[Int]]
    notasTroco 0 = [[]]
    notasTroco x = [ y:ys | y <- disponiveis, x >= y, ys <- notasTroco(x-y)]
--20-
