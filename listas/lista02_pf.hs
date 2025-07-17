-- 1. Reescreva algumas das funções da lista de exercícios anterior. Utilize definições locais.
    -- Bom, vou reescrever a função "media" e "positivo", ambas definidas na lista anterior
    -- media será reaproveitada mais adiante
media :: Float -> Float -> Float -> Float
media x y z = soma/3
    where soma = x + y + z

positivo :: Int -> Int
positivo x = resultado
    where
        resultado
            | x > 0     = 1
            | x == 0    = 0
            | otherwise = -1

{- 2. Usando guardas, defina uma função chamada sinal que receba um número Int e que retorne 1, 0 ou -1,
dependendo se o valor é positivo, zero ou negativo, respectivamente.
-}
sinal :: Int -> Int
sinal x
   | x > 0 = 1
   | x < 0 = -1
   | otherwise = 0

-- 3. Considerando a definição do exercício anterior, avalie manualmente as expressões
    {-  sinal (4-2*3)
        sinal (3^2 - 2*4)
        sinal (4*4 - 2^4)
    -}
    -- Questão sem código. É só resolver isso aí no papel, imitando a avaliação preguiçosa do haskell, como consta na página 4 do slide.

{- 4. Na lista de exercícios anterior você deve ter definido a função sinal, mas talvez com outro nome.
Usando sua definição anterior mas, se for caso, alterando o nome da função, avalie as expressões a. b. e c. listadas acima.
-}
-- Questão redundante, porque ambas as definições são idênticas (talvez o prof. não esperava que os alunos usassem guardas naquela). Pulando pra próxima..

{- 5.Usando guardas, defina uma função que, dado um inteiro, retorne um string que informe se o número é positivo, negativo ou nulo.
Caso não for nulo, o string deverá conter também a magnitude do número. Por exemplo, para o valor 15 a função deverá retornar “Positivo 15”, para o valor -15
deverá retornar “Negativo 15” e para 0 deverá retornar “Nulo”.
-}
pos :: Int -> String
pos x
   | x > 0 = "Positivo " ++ show x
   | x < 0 = "Negativo " ++ show (abs x)
   | otherwise = "Nulo"


{- 6. Defina uma função que receba como entradas três números Float correspondentes a três notas e que retorne "Aprovado", "Reprovado" ou
"Vai para a Final". Considera-se aprovado quando a média das três notas for maior ou igual a 7 e reprovado quando a nota for menor que 5.
Se a nota estiver entre 5 e 7, considera-se que vai para a final.
-}
aprov :: Float -> Float -> Float -> String
aprov x y z
   | m >= 7 = "Aprovado"
   | m < 5 = "Reprovado"
   | otherwise = "Vai para a Final"
   where m = media x y z


{- 7.Dê definições diferentes para uma função lógica que implemente o "ou exclusivo".
O ou exclusivo de dois valores é verdadeiro se e somente se um deles for verdadeiro e outro falso.
Uma versão usando a expressão condicional if _ then _ else _.
Uma versão usando casamento de padrões.
Uma versão somente usando operadores lógicos (sem condicional nem casamento de padrões)
Uma versão usando comparação de igualdade (ou comparação de diferente)
-}

-- O "ou exclusivo" é o famoso XOR. Um XOR entre dois valores é verdadeiro se e somente se apenas um deles for verdadeiro.
-- As funções foram definidas conforme a ordem presente no enunciado.

ex1 :: Bool -> Bool -> Bool
ex1 x y = if x /= y then True else False

ex2 :: Bool -> Bool -> Bool
ex2 False True = True
ex2 True False = True
ex2 _ _        = False

ex3 :: Bool -> Bool -> Bool
ex3 x y = (x || y) && not (x && y)

ex4 :: Bool -> Bool -> Bool
ex4 x y = not (x == y)