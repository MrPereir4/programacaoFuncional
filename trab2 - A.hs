--1
--a
selecao :: (Ord a) => [a] -> [a]
selecao [] = []
selecao xs = [x] ++ selecao (remove x xs)
    where x = foldr1 (min) xs
    
remove :: (Ord a) => a -> [a] -> [a]
remove a [] = []
remove a (x:xs)
    | a == x = xs
    | otherwise = x:(remove a xs)
    
--b
insercao1:: (Ord a) => [a] -> [a]
insercao1 [] = []
insercao1 (x:xs) = foldr insereOrd1 [x] (insercao1 xs)

insereOrd1 :: Ord a => a -> [a] -> [a]
insereOrd1 x [] = [x]
insereOrd1 x (y:ys)
 |x <= y = (x:y:ys)
 |otherwise = y: (insereOrd1 x ys)
 
--c
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (s:xs) = quicksort (filter (< s) xs) ++ [s] ++ quicksort (filter (>= s) xs)


--Exercicio 2
--sem contador
--a
troca :: (Ord a) => ([a], Int) -> ([a], Int)
troca ([x], trg) = ([x], trg)
troca ((x:y:xs), trg) =
    if x > y
        then chg (troca ((x:xs), 1)) y
        else chg (troca ((y:xs), trg)) x
    where
        chg (z, k) e = (e : z, k)
        
bolhaAuxiliar :: (Ord a) => ([a], Int) -> Int -> ([a], Int)
bolhaAuxiliar (l, trg) 0 = (l, trg)
bolhaAuxiliar (l, trg) n
    | trg == 0 = (l, trg)
    | otherwise = bolhaAuxiliar (troca (l, 0)) (n-1)
    
bolha :: (Ord a) => [a] -> [a]
bolha [] = []
bolha lista = fst(bolhaAuxiliar (lista, -1) (length lista))

--b
bolhaVar2 :: Ord a => [a] -> [a]
bolhaVar2 [] = []
bolhaVar2 lst = bolhaVar2Ord lst (length lst)

bolhaVar2Ord :: Ord a => [a] -> Int -> [a]
bolhaVar2Ord lst 0 = lst
bolhaVar2Ord lst n = bolhaVar2Ord (troca2 lst) (n-1)

bolhaOrd [x] = [x]
bolhaOrd l = (bolhaOrd body) ++ last
    where
        lst = troca2 l
        last = splitLast lst
        body = splitBody lst

splitBody :: [a] -> [a]
splitBody lst = take (length lst - 1) lst

splitLast :: [a] -> [a]
splitLast lst = drop (length lst - 1) lst

troca2 :: Ord a => [a] -> [a]
troca2 [x] = [x]
troca2 (x:y:zs) = if x > y then
  y:(troca2 (x:zs)) else
  x:(troca2 (y:zs))
  
--c
bolhaVar3 :: Ord a => [a] -> [a]
bolhaVar3 [] = []
bolhaVar3 lst = bolhaVar3Ord lst (length lst)

bolhaVar3Ord :: Ord a => [a] -> Int -> [a]
bolhaVar3Ord lst 0 = lst
bolhaVar3Ord lst n = do
    if count /= 0 then
        (bolhaVar3Ord body (n-1)) ++ last
        else list
        where
            (list, count) = troca3 (lst, 0)
            last = splitLast list
            body = splitBody list

troca3 :: Ord a => ([a], Int) -> ([a], Int)
troca3 ([x], n) = ([x], n)
troca3 ((x:y:zs), n) = if x > y then
  addInitList (troca3 ((x:zs), n+1)) y else
  addInitList (troca3 ((y:zs), n)) x
    where
      addInitList (list, n) a = (a:list, n)
      
--com contador
--a
bubbleSort1Cont :: (Ord a) => [a] -> ([a], Int)
bubbleSort1Cont [] = ([], 0)
bubbleSort1Cont list = format (bubleAuxCont (list, -1, 0) (length list))
  where
    format (l, _, c) = (l, c)

trocaCont :: (Ord a) => ([a], Int, Int) -> ([a], Int, Int)
trocaCont ([x], flag, n) = ([x], flag, n)
trocaCont ((x : y : xs), flag, n) =
  if x > y
    then add (trocaCont ((x : xs), 1, n + 1)) y
    else add (trocaCont ((y : xs), flag, n + 1)) x
  where
    add (l, f, c) e = (e : l, f, c)

bubleAuxCont :: (Ord a) => ([a], Int, Int) -> Int -> ([a], Int, Int)
bubleAuxCont (l, flag, c) 0 = (l, flag, c)
bubleAuxCont (l, flag, c) n
  | flag == 0 = (l, flag, c)
  | otherwise = bubleAuxCont (trocaCont (l, 0, c)) (n -1)

--b
bubbleSort2Cont :: (Ord a) => [a] -> ([a], Int)
bubbleSort2Cont [] = ([], 0)
bubbleSort2Cont lst =
  let add (l, c) e = (e : l, c)

      troca ([x], c) = ([x], c)
      troca ((x : y : xs), c) =
        if x > y
          then add (troca (x : xs, c + 1)) y
          else add (troca (y : xs, c + 1)) x

      split lst = (take (length lst - 1) lst, drop (length lst - 1) lst)

      bubble :: (Ord a) => ([a], Int) -> ([a], Int)
      bubble ([x], c) = ([x], c)
      bubble (l, c) = (proxima_etapa ++ ultimo_elem, rec_c)
        where
          (lista_trocada, c1) = (troca (l, c))
          (parte_a_trocar, ultimo_elem) = split lista_trocada
          (proxima_etapa, rec_c) = bubble (parte_a_trocar, c1)
   in bubble (lst, 0)

--c
bubbleSort3Cont :: (Ord a) => [a] -> ([a], Int)
bubbleSort3Cont [] = ([], 0)
bubbleSort3Cont l =
  let add (l, f, c) y = (y : l, f, c)
      split lst = (take (length lst - 1) lst, drop (length lst - 1) lst)
      format (l, _, c) = (l, c)

      troca ([x], flag, c) = ([x], flag, c)
      troca ((x : y : xs), flag, c) =
        if x > y
          then add (troca ((x : xs), 1, c + 1)) y
          else add (troca ((y : xs), flag, c + 1)) x

      bubble ([x], flag, c) = ([x], flag, c)
      bubble (lst, flag, c)
        | n_flag == 0 = (lst, flag, c)
        | otherwise = (proxima_etapa ++ ultimo_elem, 0, rec_c)
        where
          (lista_trocada, n_flag, c1) = troca (lst, flag, c)
          (parte_a_trocar, ultimo_elem) = split lista_trocada
          (proxima_etapa, _, rec_c) = bubble (parte_a_trocar, 0, c1)
   in format (bubble (l, -1, 0))

--Exercicio 3
--sem contador
--a
selecao1 :: (Ord a) => [a] -> [a]
selecao1 [] = []
selecao1 xs = x:(selecao1 (remove x xs))
    where x = minimum xs
    
remove1 :: (Ord a) => a -> [a] -> [a]
remove1 a [] = []
remove1 a (x:xs)
    | a == x = xs
    | otherwise = x:(remove1 a xs)
    
--b
selecao2 :: (Ord a) => [a] -> [a]
selecao2 [] = []
selecao2 [x] = [x]
selecao2 (x:xs) = e:(selecao2 lst)
    where (e, lst) = remove_menor (x, xs)

remove_menor :: (Ord a) => (a, [a]) -> (a, [a])
remove_menor (m, [x]) = if x < m then (x, [m]) else (m, [x])
remove_menor (menorElem, (x : xs))
    | x < menorElem = add menorElem (remove_menor (x, xs))
    | otherwise = add x (remove_menor (menorElem, xs))
    where
        add a (n, l) = (n, a : l)
 
 
--Com contador
--a
selecaoCnt2 :: (Ord a) => [a] -> ([a], Int)
selecaoCnt2 [] = ([], 0)
selecaoCnt2 [x] = ([x], 0)
selecaoCnt2 (x:xs) =
    let (least, novoUlt, cnt) = remove_menorCnt(x, xs, 0)
        (prox_etapa, nCont) = selecaoCnt2 novoUlt
    in (least: prox_etapa, cnt + nCont)

remove_menorCnt :: (Ord a) => (a, [a], Int) -> (a, [a], Int)
remove_menorCnt (m, [x], c) = if x < m then (x, [m], c+1) else (m, [x], c + 1)
remove_menorCnt (menorElem, (x : xs), c1)
    | x < menorElem = add menorElem (remove_menorCnt (x, xs, c1 + 1))
    | otherwise = add x (remove_menorCnt (menorElem, xs, c1 + 1))
    where
        add a (n, l, c) = (n, a:l, c)

--Exercicio 4
--Sem contador
--a
quicksortVar1 :: (Ord a) => [a] -> [a]
quicksortVar1 [] = []
quicksortVar1 (s:xs) =
    let (left, right) = divide s xs
    in (quicksortVar1 left) ++ [s] ++ (quicksortVar1 right)
    
divide :: (Ord a) => a -> [a] -> ([a], [a])
divide _ [] = ([], [])
divide s [x] = if x < s then ([x], []) else ([], [x])
divide s (x:xs)
    | x < s = addLeft x (divide s xs)
    | otherwise = addRight x (divide s xs)
    where
        addLeft a (b, c) = (a : b, c)
        addRight a (b, c) = (b, a : c)
        
--b
quicksortVar2 :: (Ord a) => [a] -> [a]
quicksortVar2 [] = []
quicksortVar2 lst =
    let firstThree = take 3 lst
        pivo = if length (firstThree) < 3 then firstThree !! 0
            else foldr1 (min) (firstThree)
            
        deletaPrimOcorrencia _ [] = []
        deletaPrimOcorrencia x (y:ys)
            | x == y = ys
            | otherwise = y : deletaPrimOcorrencia x ys
            
        (left, right) = divide2 pivo (deletaPrimOcorrencia pivo lst)
    in (quicksortVar2 left) ++ [pivo] ++ (quicksortVar2 right)
    
divide2 :: (Ord a) => a -> [a] -> ([a], [a])
divide2 _ [] = ([], [])
divide2 s [x] = if x < s then ([x], []) else ([], [x])
divide2 s (x:xs)
    | x < s = addLeft x (divide2 s xs)
    | otherwise = addRight x (divide2 s xs)
    where
        addLeft a (b, c) = (a : b, c)
        addRight a (b, c) = (b, a : c)
        
--com contador
--a
divideCont :: (Ord a) => a -> [a] -> Int -> ([a], [a], Int)
divideCont _ [] n = ([], [], n)
divideCont x [e] n =
  if e < x
    then ([e], [], n + 1)
    else ([], [e], n + 1)
divideCont x (e : es) n
  | e < x = addEsq e (divideCont x es (n + 1))
  | otherwise = addDir e (divideCont x es (n + 1))
  where
    addEsq a (l, r, c) = (a : l, r, c)
    addDir a (l, r, c) = (l, a : r, c)

quickSortVar3 :: (Ord a) => [a] -> ([a], Int)
quickSortVar3 [] = ([], 0)
quickSortVar3 (piv : xs) =
  let (left, right, n) = divideCont piv xs 0

      (sortedL, n_L) = quickSortVar3 left
      (sortedR, n_R) = quickSortVar3 right
   in (sortedL ++ [piv] ++ sortedR, n + n_L + n_R)
   
--b
quickSortVar4 :: (Ord a) => [a] -> ([a], Int)
quickSortVar4 [] = ([], 0)
quickSortVar4 lst =
  let piv = foldr1 (min) (take 3 lst)

      deleteFrstOc :: (Ord a) => a -> [a] -> Int -> ([a], Int)
      deleteFrstOc _ [] n = ([], n)
      deleteFrstOc x (y : ys) n
        | x == y = (ys, n + 1)
        | otherwise = add y (deleteFrstOc x ys (n + 1))
        where
          add e (l, c) = (e : l, c)

      (novoUlt, checks) = deleteFrstOc piv lst 0

      (left, right, n1) = divideCont piv novoUlt 0
      (sortedL, n_L) = quickSortVar4 left
      (sortedR, n_R) = quickSortVar4 right
   in (sortedL ++ [piv] ++ sortedR, n1 + n_L + n_R + checks + 3)
