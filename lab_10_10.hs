-- Matheus Lopes Ciccotti
-- 11811BSI275

-- Lista 10/10


-- Ex. 1

-- primeira versão

separa :: [ Int ] -> [ Int ]
separa (p:s:r) = (s:r)
-- > separa [1 ,2 ,3 ,4 ,5]
-- [2,3,4,5]
-- > separa [1 ,2 ,3]
-- [2,3]
-- > separa [1 ,2]
-- [2]
-- > separa [1]
-- Erro

-- segunda versão

-- separab :: [ Int] -> [ Int ]
-- separab (p:s:r) = (r:s:p)
-- Não pode ser executada, causa um erro no compilador.


-- terceira versão

separac :: [ Int ] -> [ Int ]
separac (p:r) = r
-- > separac [1 ,2 ,3 ,4 ,5]
-- [2,3,4,5]
-- > separac [1 ,2 ,3]
-- [2,3]
-- > separac [1 ,2]
-- [2]
-- > separac [1]
-- []
-- > separac []
-- Erro


--Ex. 2

--a)
comprimento :: [Int] -> Int
comprimento [] = 0
comprimento (x:xs) = 1 + comprimento xs

--b)
somatorio :: [Int] -> Int
somatorio [] = 0
somatorio (x:xs) = x + somatorio xs

--c)
somatorio_impares :: [Int] -> Int
somatorio_impares [] = 0
somatorio_impares (x:xs)
    |(mod x 2) == 1 = x + (somatorio_impares xs)
    |otherwise = (somatorio_impares xs)

--d)
soma_quadrados :: [Int] -> Int
soma_quadrados [] = 0
soma_quadrados (x:xs) = x * x + (soma_quadrados xs)

--e)
soma_mult_3 :: [Int] -> Int
soma_mult_3 [] = 0
soma_mult_3 (x:xs)
    |(mod x 3) == 0 = x + (soma_mult_3 xs)
    |otherwise = (soma_mult_3 xs)

--f)
produtorio :: [Int] -> Int
produtorio [] = 1
produtorio (x:xs) = x * (produtorio xs)

--g)
n_esimo :: Int -> [Int] -> Int
n_esimo 0 (x:xs) = x
n_esimo n (x:xs) = n_esimo (n-1) xs

--h)
ultimo :: [Int] -> Int
ultimo [x] = x
ultimo (x:xs) = ultimo xs

--i)
duplica :: [Int] -> [Int]
duplica [] = []
duplica (x:xs) = [x, x]++duplica xs

--j)
reverso :: [Int] -> [Int]
reverso [] = []
reverso (x:xs) = reverso xs ++ [x] 

--k)
substituir_todos :: Int -> Int -> [Int] -> [Int]
substituir_todos _ _ [] = []
substituir_todos n1 n2 (x:xs)
    |x == n1 = n2:substituir_todos n1 n2 xs
    |otherwise = x:substituir_todos n1 n2 xs

--l)
substituir_primeiro :: Int -> Int -> [Int] -> [Int]
substituir_primeiro _ _ [] = []
substituir_primeiro n1 n2 (x:xs)
    |x == n1 = n2:xs
    |otherwise = x:substituir_primeiro n1 n2 xs

--m)
produto_interno :: [Int] -> [Int] -> Int
produto_interno [] [] = 0
produto_interno (x:xs) (y:ys) = x * y + produto_interno xs ys

--n)
maior :: [Int] -> Int
maior [] = 0
maior (x:xs)
    |x > maior xs = x
    |otherwise = maior xs

--o)
desduplicar :: [Int] -> [Int]
desduplicar [] = []
desduplicar (x:xs) = [x]++(desduplicar (tail xs))

--p)
impares :: [Int] -> Bool
impares [] = True
impares (x:xs)
    |(mod x 2) == 0 = False
    |otherwise = impares xs

--q)
insere :: Int -> [Int] -> [Int]
insere _ [] = []
insere n (x:xs)
    |n <= x = n:x:xs
    |otherwise = x:insere n xs

--r)
quadrado :: [Int] -> [Int]
quadrado [] = []
quadrado (x:xs) = x * x:quadrado xs

--s)
pertence :: Int -> [Int] -> Bool
pertence _ [] = False
pertence n (x:xs)
    |n == x = True
    |otherwise = pertence n xs

--t)
remover_todos :: Int -> [Int] -> [Int]
remover_todos _ [] = []
remover_todos k (x:xs)
    |x == k = remover_todos k xs
    |otherwise = x:remover_todos k xs

--u)
primeiros :: [(a,b)] -> [a]
primeiros [] = []
primeiros l =  map fst l

--v)
concatenar :: [[Int]] -> [Int]
concatenar [] = []
concatenar (x:xs) = x++concatenar xs

--w)
diferenca :: [Int] -> [Int] -> [Int]
diferenca [] [] = []
diferenca (x:xs) (y:ys) = y - x:diferenca xs ys

--x)
iguais :: [Int] -> [Int] -> Bool
iguais [] [] = True
iguais _ [] = False
iguais [] _ = False
iguais (x:xs) (y:ys)
    |x == y = iguais xs ys
    |otherwise = False

--y)
somatorio2 :: [Float] -> Float
somatorio2 [] = 0
somatorio2 (x:xs) = x + somatorio2 xs

comprimento2 :: [ Float ] -> Float
comprimento2 [] = 0
comprimento2 (_:xs) = 1 + (comprimento2 xs)

media :: [Float] -> Float
media [] = 0
media l1 = somatorio2(l1) / comprimento2(l1)

--z)
lista_impares :: Int -> [Int]
lista_impares 0 = []
lista_impares n
    | mod (n-1) 2 == 0 =  lista_impares(n-1)++[n]
    | otherwise = lista_impares (n - 1)


--Ex. 3

--a)
pertence' :: Int -> [Int] -> Bool
pertence' _ [] = False
pertence' n (x:xs)
    |n == x = True
    |otherwise = pertence' n xs

--b)
uniao :: [Int] -> [Int] -> [Int]
uniao [] [] = []
uniao (x:xs) (y:ys)
    |x == y = [x]++(uniao xs ys)
    |otherwise = [x]++[y]++(uniao xs ys)

--c)
inter :: [Int] -> [Int] -> [Int]
inter [] [] = []
inter _ [] = []
inter [] _ = []
inter (x:xs) (y:ys)
    |elem x (y:ys) = [x]++(inter xs ys)
    |otherwise = (inter xs ys)

--d)
diff :: [Int] -> [Int] -> [Int]
diff [] [] = []
diff (x:xs)(y:ys) 
    | pertence y (x:xs) == False = y:tail(ys)++xs 
    | otherwise = diff (xs) (ys)

--e)
sub_conjunto :: [Int] -> [Int] -> Bool
sub_conjunto [] [] = True
subconjunto  [] = False
subconjunto [] = True
sub_conjunto (x:xs) (y:ys)
    |x == y = sub_conjunto xs ys
    |otherwise = sub_conjunto (x:xs) ys

--f)


--Ex. 4

--a)
maius :: [Char] -> [Char]
maius [] = []
maius (x:xs) = (toUpper x) : maius xs

--b)
maius' :: [Char] -> [Char]
maius' [] = []
maius' (x:xs) 
    | isAlpha x = (toUpper x) : maius' xs
    | otherwise =  maius' xs

--c)
maius_c :: [Char] -> ([Char],[Char])
maius_c l = (l,maius l)


--Ex. 5


--Ex. 6
converte :: Int -> [Int]
converte 0 = [0]
converte num 
    |num `div` 2 == 0 = [mod num 2]
    |otherwise = converte (num `div` 2) ++ [mod num 2]


--Ex. 7
decimal :: [Int] -> Int
decimal [] = 0
decimal (x:xs) = x * 2 ^ (comprimento (x:xs) - 1) + decimal (xs)


--Ex. 8
digitos :: Int -> [Int]
digitos 0 = []
digitos n = reverse((mod n 10):reverse((digitos (div n 10))))
