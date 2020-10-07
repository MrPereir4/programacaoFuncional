--Dupla: Vinnicius Pereira da Silva(11821BCC046) e Victor Borges(11811BCC034)
--6

data Exp a =
    Val a -- um numero
    | Add (Exp a) (Exp a) -- soma de duas expressoes
    | Sub (Exp a) (Exp a) -- subtração
    | Mul (Exp a) (Exp a) -- Multiplicacao
    | Pot (Exp a) (Exp a) -- Potenciacao
    

--a
avalia :: Integral a => Exp a -> a
avalia (Val x) = x
avalia (Add exp1 exp2) = (avalia exp1) + (avalia exp2)
avalia (Sub exp1 exp2) = (avalia exp1) - (avalia exp2)
avalia (Mul exp1 exp2) = (avalia exp1) * (avalia exp2)
avalia (Pot exp1 exp2) = (avalia exp1) ^ (avalia exp2)

--b
p1, p2 :: Num a => Exp a
p1 = (Mul (Add (Val 3) (Val 12)) (Pot (Sub (Val 15) (Val 5)) (Mul (Val 1) (Val 3))))
p2 = (Mul (Add (Add (Val 6) (Sub (Val 8)(Val 5))) (Val 1)) (Add (Pot (Val 6) (Val 2)) (Val 2)))

-- Valor p1: 15000
-- Valor p2: 380

--Exercicio 7
--a
data Hora = AM Int Int | PM Int Int deriving (Show, Ord, Eq)

horasDecorridas :: Hora -> Int
horasDecorridas (AM h m) = if h > 11 then -1 else h
horasDecorridas (PM h m) = if h > 11 then -1 else h + 12

minutosDecorridos :: Hora -> Int
minutosDecorridos (AM h m) = if h > 11 then -1 else h * 60 + m
minutosDecorridos (PM h m) = if h > 11 then -1 else (h + 12) * 60 + m

segundosDecorridos :: Hora -> Int
segundosDecorridos (AM h m) = if h > 11 then -1 else (h * 60 * 60) + m * 60
segundosDecorridos (PM h m) = if h > 11 then -1 else ((h + 12) * 60 * 60) + m * 60

--b
--Modificada no a, foi retornado o valor -1 para demonstrar o erro

--c
--Foi inserido o Ord e o Eq para efetuar as comparacoes


--Exercicio 8

precede :: Data -> Data -> Bool
precede (d1, m1, y1) (d2, m2, y2)
 | y1 > y2 = False
 | y1 == y2 && m1 > m2 = False
 | y1 == y2 && m1 == m2 && d1 > d2 = False
 | otherwise = True

type Data = (Int, Int, Int)

data Contato = Nome String | Telefone Int deriving (Eq, Show)

data Mensagem = Msg Contato String Data Hora String deriving (Show)


mensagens :: [Mensagem]
mensagens = [
    (Msg (Nome "Vinnicius Pereira") "Eae Vitor" (06, 07, 2020) (AM 10 30) "WhatsApp"),
    (Msg (Nome "Vinnicius Pereira") "Tudo certo" (06, 07, 2020) (AM 10 33) "WhatsApp"),
    (Msg (Nome "Vinnicius Pereira") "Vamos fazer o trabalho" (06, 07, 2020) (AM 10 40) "WhatsApp"),
    (Msg (Nome "Vinnicius Pereira") "Pode ser as 14" (06, 07, 2020) (AM 11 30) "WhatsApp"),
    (Msg (Telefone 33330000) "Blz" (06, 07, 2020) (AM 10 30) "WhatsApp"),
    (Msg (Nome "Vinnicius Pereira") "To aqui ja" (06, 07, 2020) (AM 11 34) "WhatsApp"),
    (Msg (Nome "Vinnicius Pereira") "Cade voce" (06, 07, 2020) (AM 11 50) "WhatsApp"),
    (Msg (Nome "Vinnicius Pereira") "Temos que enviar para o professor" (06, 07, 2020) (PM 0 30) "WhatsApp"),
    (Msg (Nome "Vinnicius Pereira") "Enviei" (06, 07, 2020) (PM 8 30) "WhatsApp"),
    (Msg (Nome "Vinnicius Pereira") "Amanha vamos ver se deu certo" (06, 07, 2020) (PM 10 30) "WhatsApp"),
    (Msg (Nome "Vinnicius Pereira") "Eae Vitor" (07, 07, 2020) (AM 10 30) "WhatsApp"),
    (Msg (Nome "Vinnicius Pereira") "De certo" (07, 07, 2020) (AM 10 33) "WhatsApp"),
    (Msg (Nome "Vinnicius Pereira") "Tiramos uma boa nota" (07, 07, 2020) (AM 10 35) "WhatsApp"),
    (Msg (Nome "Vinnicius Pereira") "19" (07, 07, 2020) (AM 10 40) "WhatsApp"),
    (Msg (Nome "Vinnicius Pereira") "valia 20" (07, 07, 2020) (AM 10 45) "WhatsApp"),
    (Msg (Nome "Irma do Pereira") "Dedin" (05, 07, 2020) (AM 8 30) "WhatsApp"),
    (Msg (Nome "Irma do Pereira") "To gravida" (05, 07, 2020) (AM 8 30) "WhatsApp"),
    (Msg (Nome "Pereira") "ODEDO" (06, 07, 2020) (PM 14 00) "WhatsApp"),
    (Msg (Nome "Dedin") "Bora faze o trab" (06, 07, 2020) (PM 14 00) "WhatsApp"),
    (Msg (Nome "Dedin") "Bo" (06, 07, 2020) (AM 14 01) "WhatsApp"),
    (Msg (Nome "Chefe") "Voce esta demitido" (06, 07, 2020) (AM 10 30) "LinkedIn"),
    (Msg (Nome "Carol Gata37") "Oi gato.." (01,04,2020) (PM 8 0) "Facebook"),
    (Msg (Nome "Carol Gata37") "esolteirou?" (01,04,2020) (PM 8 0) "Facebook"),
    (Msg (Nome "Pereira") "oi." (01,04,2020) (PM 8 1) "Facebook"),
    (Msg (Telefone 994679117) "Fala nigel" (14,09,2020) (PM 7 6) "SMS"),
    (Msg (Telefone 992345679) "Fala erick ,blz?" (14,09,2020) (PM 7 6) "SMS"),
    (Msg (Telefone 994679117) "Blz e ai?" (14,09,2020) (PM 7 6) "SMS"),
    (Msg (Telefone 992345679) "blzz" (14,09,2020) (PM 7 6) "SMS"),
    (Msg (Telefone 992345679) "Ent, to aqui em baixo ja desce ai" (14,09,2020) (PM 7 6) "SMS"),
    (Msg (Telefone 994679117) "to descendo to descendo" (14,09,2020) (PM 7 6) "SMS")
    ]


--b
ordenaContatos :: [Mensagem] -> [Mensagem]
ordenaContatos [] = []
ordenaContatos x = bolhaAux x (length x)

bolhaAux :: [Mensagem] -> Int -> [Mensagem]
bolhaAux x 0 = x
bolhaAux x n = bolhaAux (troca x) (n - 1)

troca :: [Mensagem] -> [Mensagem]
troca [x] = [x]
troca (x:y:xs)
    | compara x y = y:troca(x:xs)
    | otherwise = x:troca(y:xs)
    where
        compara (Msg(Nome _) _ _ _ _) (Msg(Telefone _) _ _ _ _) = True
        compara (Msg(Telefone _) _ _ _ _) (Msg(Nome _) _ _ _ _) = False
        compara (Msg(Nome a) _ _ _ _) (Msg(Nome b) _ _ _ _) = a > b
        compara (Msg(Telefone a) _ _ _ _) (Msg(Telefone b) _ _ _ _) = a > b

--c

msg_precede :: Mensagem -> Mensagem -> Bool
msg_precede (Msg _ _ d1 h1 _) (Msg _ _ d2 h2 _)
    | d1 == d2 = (minutosDecorridos h1) < (minutosDecorridos h2)
    | otherwise = precede d1 d2
    
ordenandoPorDataHora :: [Mensagem] -> [Mensagem]
ordenandoPorDataHora [] = []
ordenandoPorDataHora (pivo:xs) = (ordenandoPorDataHora [x | x<-xs, (msg_precede x pivo) == False]) ++ [pivo] ++  (ordenandoPorDataHora [x | x<-xs, (msg_precede x pivo) == True])

--d

ultimasDuasMensagens :: Contato -> [Mensagem] -> [Mensagem]
ultimasDuasMensagens contato mensagens = take 2 [(Msg c m d h a) | (Msg c m d h a)<-sorted_msgs, c == contato]
    where
        sorted_msgs = ordenandoPorDataHora mensagens

--Exercicio 9

data ArvoreBinInt = Nulo | No Int ArvoreBinInt ArvoreBinInt deriving Show

arvDados :: ArvoreBinInt
arvDados = No 4 (No 2 Nulo Nulo) (No 10 (No 5 Nulo Nulo) (No 15 Nulo Nulo))

--a
internos :: ArvoreBinInt -> [Int]
internos Nulo = []
internos (No _ Nulo Nulo) = []
internos (No x esq dir) = (x : internos esq) ++ internos dir

--b
somaNos :: ArvoreBinInt -> Int
somaNos Nulo = 0
somaNos (No x Nulo Nulo) = x
somaNos (No x esq dir) = x + (somaNos esq) + (somaNos dir)

--c
pertence :: Int -> ArvoreBinInt -> Bool
pertence _ Nulo = False
pertence x (No valorNo esq dir)
    | x == valorNo = True
    | x < valorNo = pertence x esq
    | otherwise = pertence x dir
    
--Exercicio 10

data ArvBinEA a = Vazia | Folha a |
                  NoEA (Char, ArvBinEA a, ArvBinEA a)
                     deriving (Show)


ea::ArvBinEA Float
ea = NoEA ('+', NoEA ('*', Folha 10, Folha 5), Folha 7)


arvoreResolvida :: Floating a => ArvBinEA a -> a
arvoreResolvida Vazia = 0
arvoreResolvida (Folha x) = x
arvoreResolvida (NoEA (ar, esq, dir))
    | ar == '*' = arvoreResolvida esq * arvoreResolvida dir
    | ar == '/' = arvoreResolvida esq / arvoreResolvida dir
    | ar == '+' = arvoreResolvida esq + arvoreResolvida dir
    | ar == '-' = arvoreResolvida esq - arvoreResolvida dir
    | otherwise = undefined
