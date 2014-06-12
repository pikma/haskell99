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

-- Solutions to the problems 1-10, at
-- http://www.haskell.org/haskellwiki/99_questions/11_to_20

data Encoding a = Multiple (Int, a) | Single a
  deriving (Show)
encodeModified l = map aux $ encode l
  where aux (n, x) = if n == 1 then Single x else Multiple (n, x)
-- Other solution, that doesn't reuse anything from above:
encodeModified' l = reverse $ aux l []
  where aux []       ll = ll
        aux l@(x:xs) ll =
          let (begin, end) = span (== x) l
              count = length begin
              compressed = if count == 1 then Single x
                                         else Multiple (count, x) in
                aux end (compressed:ll)

decodeModified = reverse . (foldl f [])
  where f res (Single x) = x:res
        f res (Multiple (n, x)) = (replicate n x) ++ res
-- Note: should have used concatMap.

encodeDirect l = reverse $ aux l [] where
  aux []      ll = ll
  aux l@(x:_) ll =
    let (n, rest) = takeWhileAndCount l in
      aux rest (encodeElement (n, x):ll) where
        encodeElement (1, x) = Single x
        encodeElement (n, x) = Multiple (n, x)
        takeWhileAndCount l = aux' 0 l where
          aux' n [] = (n, [])
          aux' n l@(x':xs) = if x' == x then aux' (1+n) xs else (n, l)
