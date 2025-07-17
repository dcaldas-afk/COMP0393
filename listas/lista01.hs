-- 1. Defina uma função que permita verificar se um número Int é par.
par :: Int -> Bool
par x = mod x 2 == 0

-- 2. Defina uma função que permita verificar se um número Int é ímpar.
impar :: Int -> Bool
impar x = mod x 2 == 1

-- 3. Defina uma função que calcule a média aritmética de três números Double.
media :: Double -> Double -> Double -> Double
media x y z = (x + y + z)/3

-- 4. Defina uma função que calcule o menor de dois números Int.
menor :: Int -> Int -> Int
menor x y = if x < y then x else y

-- 5. Defina uma função que permita verificar se um caractere é uma letra minúscula do alfabeto inglês. 
minuscula :: Char -> Bool
minuscula c = c >= 'a' && c <= 'z'

-- 6. Defina uma função que receba um número Int e que retorne 1, 0 ou -1, dependendo se o valor é positivo, zero ou negativo, respectivamente.
positivo :: Int -> Int
positivo x
    | x > 0     = 1
    | x == 0    = 0
    | otherwise = -1

{- 7.Um supermercado está com oferta na compra de vinhos. Na compra de três garrafas paga-se apenas duas. 
Defina uma função que receba a quantidade de garrafas de vinho compradas e o preço individual de cada garrafa, 
sem descontos, e calcule o valor total a pagar. -}
desconto :: Int -> Double -> Double
desconto qtd preco =
    let trio = div qtd 3
        subtotal = fromIntegral(qtd - trio) * preco
    in subtotal

{- 8. Na última semana do circo em Aracaju os ingressos estão com 50% de desconto para menores de idade e idosos. 
Considere menor de idade até os 18 anos, inclusive, e idoso a partir dos 60 anos. Para o resto da população o desconto é de 10%. 
Defina uma função que receba a idade e o preço normal do ingresso, um Int e um Double, respectivamente. 
A função deve retornar o valor a ser pago. -}
ingresso :: Int -> Double -> Double
ingresso idade preco
    | idade <= 18 || idade >= 60 = preco / 2
    | otherwise                  = preco * 0.9

-- 9. Defina duas funções, uma para transformar graus fahrenheit para centígrados e outra que faz o inverso.
fahrenheitParaCelsius :: Double -> Double
fahrenheitParaCelsius temp = (temp - 32) / 1.8

celsiusParaFahrenheit :: Double -> Double
celsiusParaFahrenheit temp = temp * 1.8 + 32

{- 10. Defina uma função que receba um caractere. Caso o caractere for uma letra maiúscula do alfabeto inglês a função deverá retornar 
a correspondente letra minúscula, caso contrário a função retorna o próprio caractere. 
Na definição, só pode usar as funções pré-definidas toEnum e fromEnum. -}
descapitalizar :: Char -> Char
descapitalizar c
    | c >= 'A' && c <= 'Z' = toEnum(fromEnum c + 32)
    | otherwise            = c

{- 11. Defina uma função que, dado um inteiro, retorne um string que informe se o número é positivo, negativo ou nulo.
Caso não for nulo, o string deverá conter também a magnitude do número. 
Por exemplo, para o valor 15 a função deverá retornar “Positivo 15”, 
para o valor -15 deverá retornar “Negativo 15” e para 0 deverá retornar “Nulo”.-}
ex11 :: Int -> String
ex11 n 
    | n > 0     = "Positivo " ++ show n
    | n < 0     = "Negativo " ++ show (abs n)
    | otherwise = "Nulo"