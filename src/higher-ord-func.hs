inc :: Num a => a -> a
inc = (1+)

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' _ []     = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ []      = []
filter' f (x:xs)
                | f x       = x : filter' f xs
                | otherwise = filter' f xs
