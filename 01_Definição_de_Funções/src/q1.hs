-- 1. Defina uma função que permita verificar se um número Int é par.
isEven :: Int -> Bool
isEven = even

main :: IO ()
main = do
    putStrLn "Digite um número: "
    a <- getLine
    let num = read a :: Int
    let res = isEven num
    if res
        then putStrLn "Este número é par"
    else
        putStrLn "Este número não é par"