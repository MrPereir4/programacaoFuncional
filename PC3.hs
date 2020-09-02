1a-
(|||) :: Bool -> Bool -> Bool

True ||| True = True
True ||| False = True
False ||| True = True
False ||| False = False

False ||| False = False
_ ||| _ = True

False ||| b = b
True ||| _ = True

1b-

a ||| b  = if a == False && b == False then False
else True

a ||| _ = if a == True then True
else False


2-

type Ponto = (Float, Float)
dist :: Ponto -> Ponto -> Float
dist (x1, y1) (x2, y2) = sqrt(((x2 - x1) ^ 2) + ((y2 - y1) ^ 2))


4-

fatorialComGuard :: Int -> Int
fatorialComGuard x
    | x == 0 = 1
    | otherwise = x * fatorialComGuard(x-1)
    
fatorialCasPad :: Int -> Int
fatorialCasPad 0 = 1
fatorialCasPad x = x * fatorialCasPad(x-1)


5-

fibo :: Int -> Int
fibo 0 = 0
fibo 1 = 1
fibo x = fibo(x-2) + fibo(x-1)

6-

n_tri :: Int -> Int
n_tri 0 = 0
n_tri 1 = 1
n_tri x = x + n_tri(x-1)

7-

passo :: (Int, Int) -> (Int, Int)
passo(x, y) = (y, y+x)

fibo :: Int -> (Int,Int)

fibo 0 = (0,1)
fibo n = (fibo (n-1))

8-

potencia2 :: Int -> Int
potencia2 0 = 1
potencia2 x = 2 * potencia2(x-1)


9-

prodIntervalo :: Int -> Int -> Int
prodIntervalo m n
    | m == n = n
    | otherwise = n * prodIntervalo m (n-1)

11-

resto_div :: Int -> Int -> Int
resto_div x y
    | x < y = x
    | otherwise = resto_div(x-y) y
    
    
12-

mdcGuard :: (Int, Int) -> Int
mdcGuard (x, y)
    | y == 0 = x
    | otherwise = mdcGuard(y, (mod x y))
    
mdcCasPad :: (Int, Int) -> Int
mdcCasPad (x, 0) = x
mdcCasPad (x, y) = mdcCasPad(y, (mod x y))

13-

binomialGuard :: (Int, Int) -> Int
binomialGuard (x, y)
    | y == 0 = 1
    | x == y = 1
    | otherwise = binomialGuard (x-1, y) + binomial (x-1, y-1)
    

binomial :: (Int, Int) -> Int
binomial (x, 0) = 1
binomial (x, y) = if (y == x) then 1 else binomial (x-1, y) + binomial (x-1, y-1)


14-
a) [5, 4..1]

b) take 3 ['a', 'c'..'h']

c) [1,4..16]

d) zip (zip [1,1] [-2,5]) [-5,9]



15-

intervalo :: Int -> Int -> [Int]
intervalo x y
    | x < y = [x..y]
    | x == y = [x]
    | otherwise = []
    
impares :: Int -> Int -> [Int]
impares a b = if b > a then [ x| x <- [a..b], x `mod` 2 == 1]
    else []


