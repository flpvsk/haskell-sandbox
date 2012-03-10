fib :: Num i => i -> i
fib 0 = 0
fib 1 = 1
fib i = fib i-1 + fib i-2

replicate' :: Num n => e -> n -> [e]
replicate' e 0 = []
replicate' e n = e:replicate' e (n-1)

take' :: Num n => n -> [e] -> [e]
take' 0 _       = []
take' 1 (x:_)   = [x]
take' _ []      = []
take' n (x:xs)  = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a, b)]
zip' [] []          = []
zip' [] [_]         = []
zip' [_] []         = []
zip' (x:xs) (y:ys)  = (x,y) : (zip' xs ys)

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' e (x:xs)
                | e == x = True
                | otherwise = elem' e xs

--quicksort :: (Ord a) => [a] -> [a]
quicksort []      = []; quicksort (x:xs)  = 
                  quicksort [lt | lt <- xs, lt <= x]
                  ++ [x] ++
                  quicksort [gt | gt <- xs, gt > x]
