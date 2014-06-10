-- Solutions to the problems 1-10, at
-- http://www.haskell.org/haskellwiki/99_questions/1_to_10.

myLast [] = error "empty list"
myLast [x] = x
myLast (_:xs) = myLast xs

myButLast [] = error "empty list"
myButLast [x] = error "list of one element"
myButLast [x, _] = x
myButLast (_:xs) = myButLast xs

elementAt [] _ = error "empty list"
elementAt l 1 = head l
elementAt l n = elementAt (tail l) (n - 1)

myLength l = myLength' l 0 where
  myLength' [] n = n
  myLength' (_:xs) n = myLength' xs (n+1)

myReverse = foldl (flip (:)) []

isPalyndrome l = (reverse l) == l

data NestedList a = Elem a | List [NestedList a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = (flatten x) ++ (flatten $ List xs)
-- To remember: concatMap :: (a -> [b]) -> [a] -> [b]

compress (x1:l@(x2:xs))
  | x1 == x2  = compress l
  | otherwise = x1:(compress l)
compress l = l

pack [] = []
pack (x:xs) = reverse $ pack' xs [x] []
  where pack' []     xl xll = xl:xll
        pack' (x:xs) xl xll
          | x == head xl = pack' xs (x:xl) xll
          | otherwise    = pack' xs [x] (xl:xll)
-- To remember: span :: (a -> Bool) -> [a] -> ([a], [a])

encode l = map (\x -> (length x, head x)) (pack l)
