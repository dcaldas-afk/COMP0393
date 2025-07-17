menor :: Double -> Double -> Double
menor x y = if x <= y then x else y

maior :: Double -> Double -> Double
maior x y = if x >= y then x else y

-- 2. Defina uma função que receba três números e determine se todos são diferentes.
diff :: Double -> Double -> Double -> Bool
diff x y z = (x /= y) && (x /= z) && (z /= y)

{- 3. Defina uma função que receba três números e calcule a mediana. 
A mediana é o valor central quando listamos os três números por ordem de grandeza.-}
mediana :: Double -> Double -> Double -> Double
mediana x y z = 
    let m = menor x (menor y z)
        n = maior x (maior y z)
    in (x + y + z) - m - n

{- 4. Anualmente, o MEC avalia os cursos universitários de todo o país e atribui conceitos com base em diversos critérios, 
sendo um deles a quantidade de livros disponíveis. Considerando as regras definidas abaixo, escreva uma função que receba
como entrada a quantidade de livros e a quantidade de alunos de um curso, ambos inteiros, e retorne a letra maiúscula 
correspondente ao conceito concedido pelo MEC.
1 livro para até 8 alunos --> Conceito A
1 livro para entre 9 e 12 alunos --> Conceito B
1 livro para entre 13 e 18 alunos --> Conceito C
1 livro para mais de 18 alunos --> Conceito D -}
mec :: Int -> Int -> Char
mec livros alunos
    | x <= 8             = 'A'
    | x >= 9 && x <= 12  = 'B'
    | x >= 13 && x <= 18 = 'C'
    | otherwise          = 'D'
    where x = div alunos livros

{- 5. Dados a, b e c, os três lados de um triângulo, defina uma função para calcular a área utilizando a fórmula
area= raiz(s(s-a)(s-b)(s-c)) onde s = a+b+c/2 
A função deve retornar zero no caso dos três lados não formarem efetivamente um triângulo. -}
area :: Double -> Double -> Double -> Double
area a b c
    | (a + b > c) && (a + c > b) && (b + c > a) = x ** 0.5
    | otherwise                                 = 0
    where 
        s   = (a + b + c)/2
        x   = (s * (s-a) * (s-b) * (s-c))

{- 6. Chama-se ano bissexto o ano ao qual é acrescentado um dia extra, ficando ele com 366 dias, 
um dia a mais do que os anos normais de 365 dias, ocorrendo a cada quatro anos, exceto anos múltiplos de 100 que 
não são múltiplos de 400. Defina uma função que, dado um ano, indique se este é bissexto.-}
bissexto :: Int -> Bool
bissexto ano = (mod ano 400 == 0) || (mod ano 4 == 0) && (mod ano 100 /= 0)

{- 7. 
-}

{- 8. A Locadora de Veículos Eudora lançou uma grande promoção esse mês: pagando apenas R$ 90 por diária, 
o cliente pode alugar um carro de passeio. Para cada diária, o cliente recebe uma cota de quilometragem de 100 Km. 
Cada quilômetro a mais custará uma taxa extra de R$ 12. Escreva uma função que receba como entrada a quantidade de dias 
e a quilometragem total rodada por um cliente dessa locadora e retorne o valor total a ser pago.-}
eudora :: Int -> Double -> Double
eudora dias km = 90 * d + precoKm
    where precoKm  = maior 0 (km - 100 * d ) * 12
          d        = fromIntegral dias

{- 9. Uma corretora de seguros cobra mais barato se o principal condutor do veículo é mulher e se tem mais de 40 anos. 
Caso contrário, o valor do seguro fica caro. Defina uma função que receba um valor booleano que indica se o condutor
é homem ou mulher (True ou False, respectivamente), outro valor inteiro com a idade em anos do condutor.
A função deve retornar um booleano que indique se o seguro vai ficar barato ou caro (True ou False, respectivamente).
-}
seguro :: Bool -> Int -> Bool
seguro sexo idade = not sexo && (idade > 40)

{- 10. Numa cidade o valor da KWh de energia varia da forma mostrada abaixo.
Até 99 KWh: R$1.35
100 até 299 KWh: R$1.55
300 até 574 KWh: R$1.75
Maior ou igual a 575 KWh: R$2.15
Sabe-se que quando o consumo é maior que 300KWh é cobrado uma taxa de 10% no valor da conta e o preço mínimo de qualquer conta é R$35. 
Escreva uma função para calcular o valor de uma conta elétrica. -}
conta :: Int -> Double
conta consumo = maior 35 subtotal
    where 
    x = fromIntegral consumo
    subtotal
        | x <= 99              = x * 1.35
        | x >= 100 && x <= 299 = x * 1.55
        | x >= 300 && x <= 574 = x * 1.75 * 1.1
        | otherwise            = x * 2.15 * 1.1

{- 11. Redefina a função da questão anterior considerando que a tabela de preços é progressiva. 
Isto quer dizer que, por exemplo, se o consumo for 150 KWh, então deste total de KWh, para os primeiros 99 KWh usa-se a tarifa 
de R$1.35 por KWh e para os restantes 51 KWh usa-se a tarifa de R$1.55.-}
conta2 :: Int -> Double
conta2 consumo = maior 35 total
    where
    x = fromIntegral consumo
    qt1 = menor x 99
    qt2 = menor (maior 0 (x - 99)) (299 - 99)
    qt3 = menor (maior 0 (x - 299)) (574 - 299)
    qt4 = maior 0 (x - 574)
    subtotal = qt1 * 1.35 + qt2 * 1.55 + qt3 * 1.75 + qt4 * 2.15
    total = if x > 300 then subtotal * 1.1 else subtotal

{- 13. Uma estratégia para resolver problemas é primeiro lidar com simplificações do problema original. 
Neste exercício você deve definir uma função para calcular idade em anos, meses e dias. Antes de definir a solução completa, defina:

a. Uma função que calcula a idade somente em anos completos.
b. Uma função que calcula a idade em anos e meses completos.
c. A função requerida.     -}

funcA :: Int -> Int -> Int
funcA anoAtual anoNasc = anoAtual - anoNasc

funcB :: (Int, Int) -> (Int, Int) -> (Int, Int)
funcB (mesAtual, anoAtual) (mesNasc, anoNasc)
    | mesAtual >= mesNasc = (anos, meses)
    | otherwise = (anos - 1, meses + 12)
    where
        anos = funcA anoAtual anoNasc
        meses = mesAtual - mesNasc

funcC :: (Int, Int, Int) -> (Int, Int, Int) -> String
funcC (diaAtual, mesAtual, anoAtual) (diaNasc, mesNasc, anoNasc)
    |
