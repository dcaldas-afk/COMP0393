-- 9. Defina uma função que, dado um inteiro, retorne um string que informe se o número é positivo, negativo ou nulo. Caso não for nulo, o string deverá conter também a magnitude do número. Por exemplo, para o valor 15 a função deverá retornar “Positivo 15”, para o valor -15 deverá retornar “Negativo -15” e para 0 deverá retornar “Nulo”. 
isPositiveStr :: Int -> String
isPositiveStr x 
    |x > 0 = "Positivo " ++ show x
    |x < 0 = "Negativo " ++ show x
    |otherwise = "Nulo"

main :: IO()
main = do
    putStrLn "Digite um número inteiro: "
    a <- getLine
    let x = read a :: Int
    putStrLn(isPositiveStr x)
