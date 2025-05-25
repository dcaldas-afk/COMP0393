-- 6. Num supermercado está com oferta na compra de vinhos. Na compra de três garrafas paga-se apenas duas. Defina uma função que receba a quantidade de garrafas de vinho compradas e o preço individual de cada garrafa, sem descontos, e calcule o valor total a pagar.
    
sales :: Int -> Double -> Double
sales qtd price = 
    let trio = qtd `div` 3
        subtotal = fromIntegral(qtd - trio) * price
    in subtotal

main :: IO()
main = do
    putStrLn "Quantas garrafas você está levando?"
    a <- getLine
    putStrLn "Qual o preço individual de cada garrafa?"
    b <- getLine
    let x = read a :: Int
    let y = read b :: Double
    let res = sales x y
    putStrLn $ "Após a promoção, o subtotal ficou " ++ show res