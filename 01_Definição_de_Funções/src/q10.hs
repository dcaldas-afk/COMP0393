-- 10. Defina uma função que receba como entradas três números Float correspondentes a três notas e que retorne "Aprovado", "Reprovado" ou "Vai para a Final". Considera-se aprovado quando a média das três notas for maior ou igual a 7 e reprovado quando a nota for menor que 5. Se a nota estiver entre 5 e 7, considera-se que vai para a final.
mean :: Float -> Float -> Float -> Float
mean x y z = (x + y + z)/3

isApproved :: Float -> Float -> Float -> String
isApproved x y z
    |n >= 7 = "Aprovado"
    |n >= 5 = "Vai para a Final"
    |otherwise = "Reprovado"
    where 
        n = mean x y z

main :: IO()
main = do
    putStrLn "Digite a nota da primeira prova: "
    a <- getLine
    putStrLn "Digite a nota da segunda prova: "
    b <- getLine
    putStrLn "Digite a nota da terceira prova: "
    c <- getLine
    let [x,y,z] = map read [a,b,c] :: [Float]
    putStrLn(isApproved x y z)