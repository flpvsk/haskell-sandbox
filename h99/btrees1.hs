-- http://www.haskell.org/haskellwiki/99_questions/54A_to_60

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Eq)

instance Show a => Show (Tree a) where
  show Empty = "Empty"
  show (Branch a b c) = 
    "(" ++ show a ++ 
    " " ++ show b ++
    " " ++ show c ++ ")"
{-
showHeight :: Show a => Tree a -> Int -> String
showHeight Empty i = (take i $ repeat ' ') ++ "Empty"
showHeight (Branch x s1 s2) i = (take i $ repeat ' ')
                 ++ (show x) ++ "\n" ++
                 showHeight s1 (i+2) ++
                 showHeight s2 (i+4) ++ "\n"
-}

{- Problem 55
 - Write a function cbal-tree to construct completely balanced binary trees for a given number of nodes. The predicate should generate all solutions via backtracking. Put the letter 'x' as information into all nodes of the tree.
 -
 -}

leaf :: a -> Tree a
leaf x = Branch x Empty Empty

cbaltree :: (Integral a) => a -> [Tree Char]
cbaltree 0 = []
cbaltree 1 = return $ leaf 'x'
cbaltree 2 = [ Branch 'x' (leaf 'x') Empty
             , Branch 'x' Empty (leaf 'x') ]
{-
cbaltree n = do
  prev1 <- cbaltree $ n-1
  prev2 <- cbaltree $ n-2
  first <- return $ Branch 'x' prev1 prev2
  scnd  <- return $ Branch 'x' prev2 prev1
  [first, scnd]
  -}
cbaltree n =
    cbaltree (n-1) >>= (\x ->
    cbaltree (n-2) >>= (\y ->
    [Branch 'x' x y]))

minHeight :: Tree a -> Int
minHeight Empty = 0
minHeight (Branch _ s1 s2) =
  1 + min (minHeight s1) (minHeight s2)

maxHeight :: Tree a -> Int
maxHeight Empty = 0
maxHeight (Branch _ s1 s2) =
  1 + max (maxHeight s1) (maxHeight s2)

diffHeight :: Tree a -> Int
diffHeight x = maxHeight x - minHeight x

tprint :: Show a => a -> IO()
tprint a = putStr (show a)

lprint :: Show a => [a] -> String -> IO()
lprint x sep =
    putStr $ foldr (\x acc -> (show x) ++ sep ++ acc) "" x


