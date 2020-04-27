-- 1. На типах из класса Show есть также функция shows :: Show a => a -> ShowS.
-- Используя эту функцию, но не используя рекурсию, напишите функцию
-- listToString :: Show a => [a] -> ShowS, которая за один проход по списку
-- возвращает конкатенацию представлений всех элементов списка. Например,
-- listToString [1,2,3,44] "" возвращает "12344".

-- 2. Напишите функцию isPrime :: Integer -> Bool, которая проверяет, является
-- ли неотрицательное целое число простым.

-- Используя isPrime, определите бесконечный список primes :: [Integer],
-- содержащий все простые числа. Вместо isPrime можно использовать
-- решето Эратосфена или другой подходящий метод.

-- 3. Напишите функцию hIndex :: [Int] -> [Int], которая вычисляет
-- индекс Хирша автора.  Входом к этой функции является список, где
-- каждый элемент соответствует одной публикации. Значение элемента
-- равно количеству ссылок в работах других людей на эту
-- публикацию. Например, если автор написал три статьи, из которых
-- первые две упоминались в одной статье каждая, а третья упоминалась
-- в пяти статьях, то список будет [1, 1, 5].  Индекс Хирша равен
-- максимальному числу n, такому что автор имеет по крайней мере n
-- публикаций, на каждую из которых ссылались по крайней мере n
-- раз. Например,
-- hIndex [3,1,2,2,2,3] == 2
-- hIndex [3,1,2,2,2,3,3] == 3
-- Можно использовать сортировку.

-- 4. Напишите функцию primePowers :: Integer -> [Integer], такую что
-- primePowers n возвращает бесконечный список из элементов множества
-- {p^i | p есть простое число, 1 <= i <= n} в возрастающем порядке.

import Data.List

-- task 1
listToString :: Show a => [a] -> ShowS

listToString = flip (foldr shows)

-- task 2
factors :: Integer -> [Integer]

factors n = [x | x <- [1..n], n `mod` x == 0]

isPrime :: Integer -> Bool

isPrime n 
    | n >= 0    = factors n == [1,n]
    | otherwise = False

primes :: [Integer]

primes = filter (\x-> (isPrime x == True)) [2..]

-- task 3

-- [1,2,3,2] --> [(1,1),(2,1),(3,1),(2,1)] --> [[(1,1)], [(2,1),(2,1)], [(3,1)]]
grouptoValueOnePairs :: [Int] -> [[(Int,Int)]]

grouptoValueOnePairs = group . map (\x -> (x,1))

-- [[(1,1)], [(2,1),(2,1)], [(3,1)]] --> [(1,1), (2,2), (3,1)]
countСitation :: [[(Int,Int)]] -> [(Int,Int)]

countСitation = map (foldr (\(k1,v1) (k2,v2)-> (k1,v1+v2)) (0,0))

-- [(1,1), (2,2), (3,1)] --> [(1,1), (2,2)]
filterCorrectCitation :: [(Int,Int)] -> [(Int,Int)]

filterCorrectCitation = filter (\(x,y) -> x<=y)

-- [(1,1), (2,2)] --> 2
getMaxOfCitation :: [(Int,Int)] -> Int

getMaxOfCitation = maximum . map fst

hIndex :: [Int] -> Int

hIndex = getMaxOfCitation . filterCorrectCitation . countСitation . grouptoValueOnePairs

-- task 4
merge :: [Integer] -> [Integer] -> [Integer]
merge [] list = list
merge list [] = list
-- [2:4,8] [3:9,27] --> 2 [4:8] [3:9,27] --> 2 3 [4:8] [9:27] --> 2 3 4 [8] [9,27] --> 2 3 4 8 [] [9,27] --> 2 3 4 8 9 27
merge (x:xs) (y:ys)
  | x <= y    = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

primePowers :: Integer -> [Integer]
primePowers n = foldr merge [] [map (^i) primes | i <- [1..n]]
