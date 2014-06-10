-- Solutions to the problems 1-10, at
-- http://www.haskell.org/haskellwiki/99_questions/1_to_10.

myLast [] = error "empty list"
myLast [x] = x
myLast (_:xs) = myLast xs
