--1. Escreva uma expressão para gerar uma lista com todos os números de 1 a 100 exceto os múltiplos de 5.
-- resposta: [x | x <- [1..100], mod x 5 /= 0]

-- 2. Escreva uma função que receba uma lista e que devolva os seus elementos ímpares triplicados.
q2 :: [Int] -> [Int]
q2 lista = [x * 3| x <- lista, odd x]

-- 3. Defina uma função que dada uma lista de inteiros retorne quantos da lista são pares.
q3 :: [Int] -> Int
q3 lista = length [x | x <- lista, even x]

-- 4. Escreva uma função que receba um inteiro e retorne a lista com todos seus divisores.
q4 :: Int -> [Int]
q4 n = [x | x <- [1..n], mod n x == 0]

-- 5. Defina uma função que verifique se um dado número é primo.
q5 :: Int -> Bool
q5 n = length (q4 n) == 2

-- 6. Defina uma função que calcule o máximo comum divisor de dois números.
q6 :: Int -> Int -> Int
q6 a b = maximum [x | x <- [1..min (abs a) (abs b)], mod a x == 0, mod b x == 0]

-- 7. Defina uma função que calcule o mínimo múltiplo comum de dois números.
q7 :: Int -> Int -> Int
q7 a b =
    div x  (q6 a b)
    where x = abs (a * b)

-- 8. Escreva uma função que receba uma lista de strings e retorna um único string que quando apresentado mostra os strings da lista em linhas separadas
onSeparateLines :: [String] -> String
onSeparateLines strs = concat [s ++ "\n" | s <- strs]

-- 9. Pesquise o que faz a função pré-definida replicate. Elabore uma definição de uma função que faça o mesmo que replicate
q9 :: Int -> a -> [a]
q9 n m = [m | _ <- [1..n]]

-- 10. Pesquise o que faz a função pré-definida repeat. Dê uma definição de uma função que faça o mesmo que repeat.
q10 :: a -> [a]
q10 n = [n | _ <- [1..]]

{- 11. Defina uma função
formataDirCom :: String -> Int -> String
tal que formataDirCom cs n retorna um string que completa cs com brancos à esquerda de tal forma que o tamanho fique igual a n, 
caso o tamanho de cs seja menor que n. No caso contrário, a função deve retornar cs.
-}

-- 13. Sem usar maximum nem minimum, escrever uma função que verifica se uma lista está formada pela repetição de um único elemento.
q13 :: Eq a => [a] -> Bool
q13 (i:n) = and [i == m | m <- n]
q13 [] = True

--14. Defina uma função que dada uma lista verifique se todos seus elementos são múltiplos de 5
q14 :: [Int] -> Bool
q14 x = and [mod y 5 == 0 | y <- x]

-- 15. Defina uma função que dada uma lista verifique se há algum elemento que seja ímpar.
q15 :: [Int] -> Bool
q15 x = or [odd y | y <- x ]

-- 16. Defina uma função que faça o mesmo que a função elem.
q16 :: Eq a => a -> [a] -> Bool
q16 n x = or [n == m | m <- x]

{- 17. Defina uma função que calcule o valor aproximado de pi utilizando a seguinte série de Leibniz
    1 - 1/3 + 1/5 - 1/7 + 1/9 - ... = pi/4
A função recebe como argumento o número de termos a ser usado para aproximar. -}
q17 :: Int -> Double
q17 x = 4 * sum [(-1)^^k/ fromIntegral (2*k + 1) | k <- [0..x-1] ]