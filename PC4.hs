--1-
    --Avalidado.
    
--2-

    quadrados :: Int -> Int -> [Int]
    quadrados x y
        | x > y = []
        | otherwise = x * x: quadrados (x+1) y
        
--3-

    numeros_impares :: [Int] -> [Int]
    numeros_impares xs = [ x | x <- xs, odd x]
    
--4-

    tabuada :: Int -> [Int]
    tabuada x = [a*x | a <- [1..10]]

--5-

    bissexto :: Int -> Bool
    bissexto x
        | mod x 100 == 0 = False
        | mod x 4 == 0 = True
        | otherwise = False
        
    bissextos :: [Int] -> [Int]
    bissextos xs = [x | x <- xs, bissexto x == True]
    

--6-

    sublista :: [[Int]] -> [Int]
    sublista x = [ b | c <- x, b <- c]

--7-

--8-

    npares :: [Int] -> Int
    npares [] = 0
    npares xs
        | (mod (head xs) 2 == 0) = npares (tail xs) +1
        | otherwise = npares (tail xs)
        
--9-

    produtorio :: [Int] -> Int
    produtorio [] = 0
    produtorio [x] = x
    produtorio xs = (head xs)*(produtorio(tail xs))

--10-

    compr:: [[Int]] -> [Int]
    compr [] = []
    compr ([]:t) = comprime t
    compr ((x:y):t) = x : comprime ((y):t)

--11-

    tamanho :: [a] -> Int
    tamanho [] = 0
    tamanho xs = (tamanho (tail xs)) +1

--12-

    uniaoNRec ::[Int] -> [Int] -> [Int]
    uniaoNRec (x:xs) ys = (x:xs) ++ [y | y <- ys, elem y (x:xs) == False]

--13-

    uniaoRec2 :: Eq a => [a] -> [a] -> [a]
    uniaoRec2 y [] = y
    uniaoRec2 y (x:xs) = if elem x y then uniaoRec2 y xs else uniaoRec2 (y ++ [x]) xs
   
        
