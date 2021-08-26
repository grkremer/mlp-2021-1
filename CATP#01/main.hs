addMe :: Integer -> Integer -> Integer
addMe x y = x + y

invertelst :: [Integer] -> [Integer]
invertelst [] = []
invertelst (h:t) = invertelst(t) ++ [h]

potencia :: Integer -> Integer -> Integer
potencia x 0 = 1
potencia x 1 = x
potencia x y = x*potencia x (y-1)

powlist :: [Integer] -> Integer -> [Integer]
powlist [] y = []
powlist (h:t) y = [potencia h y] ++ powlist t y

main :: IO ()
main = print(powlist [1,2,3,4,5,6,7,8] 0)