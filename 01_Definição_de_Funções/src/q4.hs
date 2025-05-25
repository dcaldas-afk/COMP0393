    
import qualified Control.Monad

-- 4. Defina uma função que calcule o menor de dois números Int.
larger :: Int -> Int -> Int
larger x y = if x > y then x else y

main :: IO ()
main = do
    putStrLn "Digite dois números inteiros: "
    [a, b] <- Control.Monad.replicateM 2 getLine
    let [x, y] = map read [a, b] :: [Int]
    putStrLn $ "O menor destes números é " ++ show(larger x y)