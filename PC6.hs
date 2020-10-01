
--1
paridade :: [Int] -> [Bool]
paridade x = map(even) x

--2
prefixos :: [String] -> [String]
prefixos x = map(take 3) x

--3
saudacao :: [String] -> [String]
saudacao x = map("Oi " ++) x

--4
filtrar :: (a -> Bool) -> [a] -> [a]
filtrar (y) x = [ z | z <- x, y z]

--5
pares :: [Int] -> [Int]
pares x = filter even x

--6
solucoes :: [Int] -> [Int]
solucoes x = filter(\z -> (5*z + 6) < (z*z)) x

--7
maior :: [Int] -> Int
maior x = foldr1 (max) x

--8
menor_min10 :: [Int] -> Int
menor_min10 x = foldr min 10 x

--9
junta_silabas_plural :: [String] -> String
junta_silabas_plural x = foldr (++) "s" x



--/////////////////////////////////////////////
lst1 = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
lst2 = [20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1]
lst3 = [11,12,13,14,15,16,17,18,19,20,1,2,3,4,5,6,7,8,9,10]
lst4 = [10,9,8,7,6,5,4,3,2,1,20,19,18,17,16,15,14,13,12,11]
lst5 = [11,12,13,14,15,5,4,3,2,1,16,17,18,19,20,10,9,8,7,6]
lst6 = [1,12,3,14,5,15,4,13,2,11,6,17,8,19,20,10,9,18,7,16]
lst7 = [1..1000]
lst8 = [1000,999..1]
lst9 = lst1++[0]
lst10 = [0]++lst3
lst11 = lst1++[0]++lst3

--10
--bubblesort
bolha [] = []
bolha lista = bolhaOrd lista (length lista)

bolhaOrd lista 0 = lista
bolhaOrd lista n = bolhaOrd (troca lista) (n-1)

troca [x] = [x]
troca (x:y:xs)
    | x > y = y : troca (x:xs)
    | otherwise = x : troca (y: xs)

--selectionsort
selecao :: (Ord a) => [a] -> [a]
selecao [] = []
selecao xs = [x] ++ selecao (remove x xs)
    where x = minimo xs
    
remove :: (Ord a) => a -> [a] -> [a]
remove a [] = []
remove a (x:xs)
    | a == x = xs
    | otherwise = x:(remove a xs)
    
minimo :: (Ord a) => [a]->a
minimo [] = undefined
minimo [x] = x
minimo (x:xs)
    | x <= (minimo xs) = x
    | otherwise = minimo xs
    
--inserionsort
insercao :: (Ord a) => [a] -> [a]
insercao [] = []
insercao (x:xs) = insereOrd x (insercao xs)

insereOrd :: (Ord a) => a -> [a] -> [a]
insereOrd x [] = [x]
insereOrd x (y:ys)
    | x <= y = (x:y:ys)
    | otherwise = y : (insereOrd x ys)
    
--quicksort
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (s:xs) = quicksort [x | x <- xs, x < s]
    ++ [s] ++
    quicksort [x|x <- xs, x >= s]
    
--

--11
--bubblesort

bolhaCnt :: Ord a => [a] -> ([a], Int)
bolhaCnt [] = ([], 0)
bolhaCnt lista = bolhaOrdCnt (lista, 0) (length lista)

bolhaOrdCnt :: (Ord a, Num t, Num b, Eq t) => ([a], b) -> t -> ([a], b)
bolhaOrdCnt (lista, cnt) 0 = (lista, cnt)
bolhaOrdCnt (lista, cnt) n = bolhaOrdCnt ((trocaCnt (lista, cnt))) (n-1)

trocaCnt :: (Ord a, Num b) => ([a], b) -> ([a], b)
trocaCnt ([x], count) = ([x], count)
trocaCnt ((x:y:xs), count)
    | x > y = cnc (trocaCnt((x:xs), count + 1)) y
    | otherwise = cnc (trocaCnt((y:xs), count + 1)) x
    where cnc (lista, count) a = (a : lista, count)
    
--selectionshort
mySelectionsort2 :: Ord a => [a] -> ([a], Int)
mySelectionsort2 lista = selectionAUX lista 0

selectionAUX :: (Ord a) => [a] -> Int -> ([a], Int)
selectionAUX [] n = ([], n)
selectionAUX (x : xs) n =
  let (least, n_num) = minimo2 (x : xs) n

      remove2 _ [] = []
      remove2 n (h : t) =
        if (n == h)
          then t
          else h : (remove2 n t)

      add (lst, n) y = (y : lst, n)
   in add (selectionAUX (remove2 least (x : xs)) n_num) least

minimo2 :: (Ord a) => [a] -> Int -> (a, Int)
minimo2 [] _ = undefined
minimo2 [x] cont = (x, cont)
minimo2 (x : y : xs) cont
  | x > y = minimo2 (y : xs) (cont + 1)
  | otherwise = minimo2 (x : xs) (cont + 1)


--insertionshort
myInsertionsort2 :: (Ord a) => [a] -> ([a], Int)
myInsertionsort2 [] = ([], 0)
myInsertionsort2 [x] = ([x], 0)
myInsertionsort2 (h : t) =
  let (sorted_tail, n) = myInsertionsort2 t

      (lst, n1) = insereOrd2 h sorted_tail n
   in (lst, n1)

insereOrd2 :: (Ord a) => a -> [a] -> Int -> ([a], Int)
insereOrd2 x [] n = ([x], n)
insereOrd2 x (h : t) n =
  if (x <= h)
    then ((x : h : t), n + 1)
    else add (insereOrd2 x t (n + 1)) h
  where
    add (list, n) y = (y : list, n)

--quicksort
quickAux :: [a] -> Int -> (a -> Bool) -> ([a], Int)
quickAux [] n _ = ([], n)
quickAux (x : xs) n cond =
  if (cond x)
    then add (quickAux xs (n + 1) cond) x
    else quickAux xs (n + 1) cond
  where
    add (list, n) y = (y : list, n)

myQuicksort2 :: (Ord a) => [a] -> ([a], Int)
myQuicksort2 [] = ([], 0)
myQuicksort2 (piv : xs) =
  let (left, n_L) = quickAux xs 0 (<= piv)
      (right, n_R) = quickAux xs 0 (> piv)
      (sorted_L, n1_L) = myQuicksort2 left
      (sorted_R, n1_R) = myQuicksort2 right
   in (sorted_L ++ [piv] ++ sorted_R, n_L + n_R + n1_L + n1_R)

--12
--bubblesort
bolhaCntInv :: Ord a => [a] -> ([a], Int)
bolhaCntInv [] = ([], 0)
bolhaCntInv lista = bolhaOrdCntInv (lista, 0) (length lista)

bolhaOrdCntInv :: (Ord a, Num t, Num b, Eq t) => ([a], b) -> t -> ([a], b)
bolhaOrdCntInv (lista, cnt) 0 = (lista, cnt)
bolhaOrdCntInv (lista, cnt) n = bolhaOrdCntInv ((trocaCntInv (lista, cnt))) (n-1)

trocaCntInv :: (Ord a, Num b) => ([a], b) -> ([a], b)
trocaCntInv ([x], count) = ([x], count)
trocaCntInv ((x:y:xs), count)
    | x < y = cnc (trocaCntInv((x:xs), count + 1)) y
    | otherwise = cnc (trocaCntInv((y:xs), count + 1)) x
    where cnc (lista, count) a = (a : lista, count)
    
--selectionsort
selection_sort2 :: Ord a => [a] -> ([a], Int)
selection_sort2 lista = auxiliarSelection lista 0

auxiliarSelection :: (Ord a) => [a] -> Int -> ([a], Int)
auxiliarSelection [] n = ([], n)
auxiliarSelection (x : xs) n =
  let (least, n_num) = minimo2 (x : xs) n

      remove2 _ [] = []
      remove2 n (h : t) =
        if (n == h)
          then t
          else h : (remove2 n t)

      add (lst, n) y = (y : lst, n)
   in add (auxiliarSelection (remove2 least (x : xs)) n_num) least

minimo2 :: (Ord a) => [a] -> Int -> (a, Int)
minimo2 [] _ = undefined
minimo2 [x] cont = (x, cont)
minimo2 (x : y : xs) cont
  | x > y = minimo2 (y : xs) (cont + 1)
  | otherwise = minimo2 (x : xs) (cont + 1)

--insertionsort
insertion_sort2 :: (Ord a) => [a] -> ([a], Int)
insertion_sort2 [] = ([], 0)
insertion_sort2 [x] = ([x], 0)
insertion_sort2 (h : t) =
  let (sorted_tail, n) = insertion_sort2 t

      (lst, n1) = insere_ordenado2 h sorted_tail n
   in (lst, n1)

insere_ordenado2 :: (Ord a) => a -> [a] -> Int -> ([a], Int)
insere_ordenado2 x [] n = ([x], n)
insere_ordenado2 x (h : t) n =
  if (x <= h)
    then ((x : h : t), n + 1)
    else add (insere_ordenado2 x t (n + 1)) h
  where
    add (list, n) y = (y : list, n)
--quicksort
auxiliarQuick :: [a] -> Int -> (a -> Bool) -> ([a], Int)
auxiliarQuick [] n _ = ([], n)
auxiliarQuick (x : xs) n cond =
  if (cond x)
    then add (auxiliarQuick xs (n + 1) cond) x
    else auxiliarQuick xs (n + 1) cond
  where
    add (list, n) y = (y : list, n)

quicksort2 :: (Ord a) => [a] -> ([a], Int)
quicksort2 [] = ([], 0)
quicksort2 (piv : xs) =
  let (left, n_L) = auxiliarQuick xs 0 (<= piv)
      (right, n_R) = auxiliarQuick xs 0 (> piv)
      (sorted_L, n1_L) = quicksort2 left
      (sorted_R, n1_R) = quicksort2 right
   in (sorted_L ++ [piv] ++ sorted_R, n_L + n_R + n1_L + n1_R)

