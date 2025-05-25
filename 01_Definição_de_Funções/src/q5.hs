-- 5. Defina uma função que receba um número Int e que retorne 1, 0 ou -1, dependendo se o valor é positivo, zero ou negativo, respectivamente.
    
isPositive :: Int -> Int
isPositive x
    | x > 0 = 1
    | x == 0 = 0
    | otherwise = -1

main :: IO()
main = do
    putStrLn "Digite um número:"
    a <- getLine
    let n = read a :: Int
    if n == 0 then putStrLn "Este número não é nem positivo e nem negativo."
    else if n == 1 then putStrLn "Este número é positivo." 
    else putStrLn "Este número é negativo."