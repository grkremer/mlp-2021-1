addMe :: Integer -> Integer -> Integer
addMe x y = x + y

invertelst :: [Integer] -> [Integer]
invertelst [] = []
invertelst (h:t) = invertelst(t) ++ [h]

potencia x 0 = 1
potencia x 1 = x
potencia x y = x*potencia x (y-1)

powlist [] y = []
powlist (h:t) y = [potencia h y] ++ powlist t y

somatorio [] = 0
somatorio (h:t) = foldl (+) h t

norma [] = 0
norma (h:t) = sqrt(foldl (+) (potencia h 2) (powlist t 2))

ordena_pares [] [] = []
ordena_pares (h1:t1) (h2:t2) = [(h1, h2)] ++ ordena_pares t1 t2


lista_multiplicada x = map multiplica_tupla x

soma_tudo (h:t) = foldl (+) h t

multiplica_tupla a = (fst(a)*snd(a))

prod_int x y = soma_tudo(lista_multiplicada(zip x y))

cosseno_vetores x y = (prod_int x y) / ((norma x)*(norma y))

main :: IO ()
main = print(cosseno_vetores[2, 2] [0, 2])