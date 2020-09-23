
--1-
bissexto :: Int -> Bool
bissexto x
    | mod x 100 == 0 = False
    | mod x 4 == 0 = True
    | otherwise = False

--a-
valida :: Data -> Bool
valida (x, y, z)
    | x > 30 && y /= 1  && y /= 3 && y /= 5 && y /= 7 && y /= 8 && y /= 10 && y /= 12 = False
    | b == False && y == 2 && x > 28 = False
    | otherwise = True
    where
        b = bissexto z
        
--b-
bissextos :: [Int] -> [Int]
bissextos xs = a
    where
        a = [x | x <- xs, bissexto x == True]

--c-
type Emprestimo = (String, String, Data, Data, String)

type Emprestimos = [Emprestimo]

bdEmprestimo :: Emprestimos
bdEmprestimo =
  [ ("H123C9", "BSI945", (12, 9, 2009), (20, 09, 2009), "aberto"),
    ("L433C5", "BCC021", (01, 9, 2009), (10, 09, 2009), "encerrado"),
    ("M654C3", "BCC008", (04, 9, 2009), (15, 09, 2009), "aberto")
  ]

procede :: Data -> Data -> Bool
procede (dia, mes, ano) (dia2, mes2, ano2) = not (a || b || c || d)
  where
    a = not (valida (dia, mes, ano)) || not (valida (dia2, mes2, ano2))
    b = ano > ano2
    c = ano == ano2 && mes > mes2
    d = ano == ano2 && mes == mes && dia > dia2

emprestimoEmDia :: Data -> Emprestimo -> Bool
emprestimoEmDia dataAtual (codLivro, codAluno, dataEmprest, dataDevo, status) = exp1
  where
    exp1 = procede dataAtual dataDevo

atrasados :: Emprestimos -> Data -> Emprestimos
atrasados listaEmprestimos dataAtual = a
  where
    a = [x | x <- listaEmprestimos, not (emprestimoEmDia dataAtual x)]


--d-
passo :: (Int, Int) -> (Int, Int)
passo (x, y) = exp1
  where
    a = (y, x + y)

fibo2 :: Int -> (Int, Int)
fibo2 0 = (0, 1)
fibo2 n = exp1
  where
    a = passo (fibo2 (n -1))
    
--e-
prodIntervalo :: Int -> Int -> Int
prodIntervalo m n = if (m >= n) then a else b
  where
    a = n
    b = (m * (prodIntervalo (m + 1) n))

fatInter :: Int -> Int
fatInter n = a
  where
    a = prodIntervalo 1 n

--2-
--a-
bissextoUsingLet :: Int -> Bool
bissextoUsingLet ano =
  let a = (mod ano 400 == 0)
      b = (mod ano 4 == 0)
      c = (mod ano 100 /= 0)
   in a || (b && c)
   
validaLet :: DataLet -> Bool
validaLet (dia, mes, ano) =
  let a = dia >= 1 && dia <= 31 && (mes == 1 || mes == 3 || mes == 5 || mes == 7 || mes == 8 || mes == 10 || mes == 12)
      b = dia >= 1 && dia <= 30 && (mes == 4 || mes == 6 || mes == 9 || mes == 11)
      c = dia >= 1 && dia <= 28 && mes == 2 && not (bissextoLet ano)
      d = dia >= 1 && dia <= 29 && mes == 2 && (bissextoLet ano)
   in a || b || c || d
   
--b-
bissextoUsingLet :: Int -> Bool
bissextoUsingLet ano =
  let a = (mod ano 400 == 0)
      b = (mod ano 4 == 0)
      c = (mod ano 100 /= 0)
   in a || (b && c)
   

type DataLet = (Int, Int, Int)

validaLet :: DataLet -> Bool
validaLet (dia, mes, ano) =
  let a = dia >= 1 && dia <= 31 && (mes == 1 || mes == 3 || mes == 5 || mes == 7 || mes == 8 || mes == 10 || mes == 12)
      b = dia >= 1 && dia <= 30 && (mes == 4 || mes == 6 || mes == 9 || mes == 11)
      c = dia >= 1 && dia <= 28 && mes == 2 && not (bissextoUsingLet ano)
      d = dia >= 1 && dia <= 29 && mes == 2 && (bissextoUsingLet ano)
   in a || b || c || d
   
--c-
type EmprestimoLet = (String, String, DataLet, DataLet, String)

type EmprestimosLet = [EmprestimoLet]

bdEmprestimoLet :: EmprestimosLet
bdEmprestimoLet =
  [ ("H123C9", "BSI945", (12, 9, 2009), (20, 09, 2009), "aberto"),
    ("L433C5", "BCC021", (01, 9, 2009), (10, 09, 2009), "encerrado"),
    ("M654C3", "BCC008", (04, 9, 2009), (15, 09, 2009), "aberto")
  ]

procedeLet :: DataLet -> DataLet -> Bool
procedeLet (dia, mes, ano) (dia2, mes2, ano2) =
  let a = not (validaLet (dia, mes, ano)) || not (validaLet (dia2, mes2, ano2))
      b = ano > ano2
      c = ano == ano2 && mes > mes2
      d = ano == ano2 && mes == mes && dia > dia2
   in not (a || b || c || d)

emprestimoEmDiaLet :: DataLet -> EmprestimoLet -> Bool
emprestimoEmDiaLet dataAtual (codLivro, codAluno, dataEmprest, dataDevo, status) =
  let a = procedeLet dataAtual dataDevo
   in a

atrasadosLet :: EmprestimosLet -> DataLet -> EmprestimosLet
atrasadosLet listaEmprestimos dataAtual =
  let a = [x | x <- listaEmprestimos, not (emprestimoEmDiaLet dataAtual x)]
   in a

--d-
passoLet :: (Int, Int) -> (Int, Int)
passoLet (x, y) =
  let a = (y, x + y)
   in a

fibo2Let :: Int -> (Int, Int)
fibo2Let 0 = (0, 1)
fibo2Let n =
  let a = passoLet (fibo2Let (n -1))
   in a
   
--e-
prodIntervaloLet :: Int -> Int -> Int
prodIntervaloLet m n =
  let a =
        if (m >= n)
          then n
          else (m * (prodIntervalo (m + 1) n))
   in a

fatInterLet :: Int -> Int
fatInterLet n =
  let a = prodIntervalo 1 n
   in a

--3-
--1-
(\x. 2*x + 1) 3
(2*3 + 1)
(6 + 1)
7

--2-
(\ xy. x-y) 5 7
(5-7)
-2

--3-
(\ yx. x-y) 5 7
(7-5)
2

--4-
(\ yx. x-y) (\ z. z/2)
(\ yx. x-y) z/2
(\ yx. z/2-z/2)

--5-
(\ xy. x-y) ((\ z. z/2) 6) 1
(\ xy. x-y) (6/2) 1
(\ xy. x-y) 3 1
(3-1)
2

--6-
(\x. \y. - x y) 9 4
(\x. - x 4) 9
(- 9 4)
5

--7-
(\ x. * xx) (\y. y)
(\ x. * xx) y
(* yy)

--4-

--a-
8

--b-
17

--c-
48

--d-
48

--e-
[(4,1),(5,2),(6,3)]

--5-

--a-
(λx λy. y)((λz. z)(λz. z))(λw. w) 5
(\x -> \y -> y)((\z -> z)(\z -> z))(\w -> w) 5

--b-
((λf. (λx. f(f x))) (λy. (y * y))) 3
((\f -> (\x -> f(f x))) (\y -> (y * y))) 3

--c-
((λf. (λx. f(f x)))(λy.(+ y y))) 5
((\f -> (\x -> f(f x))) (\y -> (y + y))) 5

--d-
((λx. (λy. + x y) 5) ((λy. - y 3) 7))
(\x -> (\y -> x + y) 5) ((\y -> y - 3) 7)

--e-
(((λf. (λx. f(f(f x)))) (λy. (y * y))) 2)
((\f -> (\x -> f(f(f x)))) (\y -> (y * y))) 2

--f-
(λx.λy.+x ((λx.-x3)y)) 5 6
(\x -> \y -> x + ((\x -> x - 3)y)) 5 6
