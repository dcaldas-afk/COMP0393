-- 2. Defina uma função que permita verificar se um número Int é ímpar. 
isOdd :: Int -> Bool
isOdd n = n `mod` 2 == 1

main :: IO ()
main = do
    putStrLn "Digite um número: "
    a <- getLine
    let num = read a :: Int
    let res = isOdd num
    if res
        then putStrLn "Este número é ímpar"
    else
        putStrLn "Este número não é ímpar"