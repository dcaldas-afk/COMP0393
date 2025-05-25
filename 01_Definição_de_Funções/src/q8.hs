-- 8. Defina uma função que receba um caractere. Caso o caractere for uma letra maiúscula do alfabeto inglês a função deverá retornar a correspondente letra minúscula, caso contrário a função retorna o próprio caractere. Na definição, só pode usar as funções pré-definidas toEnum e fromEnum.
minusculus :: Char -> Char
minusculus c =
    let x = fromEnum c
    in if x >= fromEnum 'A' && x <= fromEnum 'Z'
        then toEnum(x + 32)
        else c

main :: IO()
main = do
    putStrLn "Digite um caractere"
    c <- getLine
    let x = read c :: Char
    putStrLn $ "Retorno: " ++ show(minusculus x)
   