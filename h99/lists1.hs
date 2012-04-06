-- http://www.haskell.org/haskellwiki/99_questions/1_to_10

{-
Prelude> myLast [1,2,3,4]
4
Prelude> myLast ['x','y','z']
'z'
-}
myLast :: [a] -> a
myLast []     = error "Empty List"
myLast (x:[]) = x
myLast (_:xs) = myLast xs

{-
Prelude> myButLast [1,2,3,4]
3
Prelude> myButLast ['a'..'z']
'y'
-}
myButLast :: [a] -> a
myButLast []        = error "Empty List"
myButLast (x:[])    = error "Less than two elements"
myButLast (x:_:[])  = x
myButLast (_:xs)    = myButLast xs

{-
Prelude> elementAt [1,2,3] 2
2
Prelude> elementAt "haskell" 5
'e'
-}
elementAt :: (Integral b) => [a] -> b -> a
elementAt [] _      = error "Empty List"
elementAt (x:xs) i
  | i <= 0    = error "Index should start from 1" 
  | i == 1    = x
  | otherwise = elementAt xs (i-1)
