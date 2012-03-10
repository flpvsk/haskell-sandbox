head1 :: [a] -> a
head1 []    = error "I'm an error, dude!"
head1 (h:_) = h

lt :: Ord a => a -> a -> a
lt a b
      | a < b     = a
      | otherwise = b
