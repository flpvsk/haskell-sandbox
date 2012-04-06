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

{-
Prelude> myLength [123, 456, 789]
3
Prelude> myLength "Hello, world!"
13
-}
myLength :: (Integral b) => [a] -> b
myLength = foldr (\_ res -> 1 + res) 0

{-
Prelude> reverse "A man, a plan, a canal, panama!"
"!amanap ,lanac a ,nalp a ,nam A"
Prelude> reverse [1,2,3,4]
[4,3,2,1]
-}
myReverse :: [a] -> [a]
myReverse = foldl (\res e -> e:res) []

{-
*Main> isPalindrome [1,2,3]
False
*Main> isPalindrome "madamimadam"
True
*Main> isPalindrome [1,2,4,8,16,8,4,2,1]
True
-}
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome l  = l == myReverse l

{-
*Main> flatten (Elem 5)
[5]
*Main> flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
[1,2,3,4,5]
*Main> flatten (List [])
[]
-}
flatten :: [[a]] -> [a]
{-
flatten []        = []
flatten ([x]:xs)  = x:(flatten xs)
-}
flatten = foldr (\[e] res -> e:res) []
