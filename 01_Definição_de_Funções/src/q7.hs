-- 7. Defina duas funções, uma para transformar graus fahrenheit para centígrados e outra que faz o inverso.   
cToFahrenheit :: Double -> Double
cToFahrenheit temp = 32 + 1.8 * temp

fToCelsius :: Double -> Double
fToCelsius temp = (temp - 32) / 1.8

main :: IO()
main = do
    putStrLn "Digite uma temperatura em Celsius:"
    a <- getLine
    let x = read a :: Double
    putStrLn $ "Esta temperatura equivale a " ++ show(cToFahrenheit x) ++ " °F"

{- Main da outra função

main :: IO()
main = do
    putStrLn "Digite uma temperatura em Fahrenheit:"
    a <- getLine
    let x = read a :: Double
    putStrLn $ "Esta temperatura equivale a " ++ show (fToCelsius x) ++ " °C"


-}