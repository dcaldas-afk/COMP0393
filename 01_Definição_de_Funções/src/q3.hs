-- 3. Defina uma função que calcule a média aritmética de três números Double.

mean :: Double -> Double -> Double -> Double
mean x y z = (x + y + z)/3

main :: IO ()
main = do
    putStrLn "Digite o primeiro número: "
    a <- getLine
    putStrLn "Digite o segundo número: "
    b <- getLine
    putStrLn "Digite o terceiro número: "
    c <- getLine
    let x = read a :: Double
    let y = read b :: Double
    let z = read c :: Double
    let res = mean x y z
    putStrLn ("A média aritmética destes 3 números é igual a " ++ show res)


{- Forma mais compacta:

main :: IO ()
main = do
    [a, b, c] <- sequence $ replicate 3 getLine
    let [x, y, z] = map read [a, b, c] :: [Double]
    putStrLn $ "A média aritmética destes 3 números é igual a " ++ show (mean x y z)

-}