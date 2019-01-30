{- 
Minimal functions on lists in haskell: head, null, tail
From this functions, let's define the following functions :
-}

--# Function that returns the length of a given list:

-- With built in functions:

len l = if null l then 0 else 1 + len (tail l)

-- With pattern matching:

len' [] = 0
len' (x:xs) = 1 + len' xs

--# Function that returns the last element of a list:

last l = if null (tail l) then head l else last (tail l)

last' (x:[]) = x
last' (x:xs) = last' xs

--# Factorial function

fact n = if n == 1 then 1 else n * fact (n-1)

fact' 1 = 1
fact' n = n * fact' (n-1) 

--# Function that returns all elements of a list without the last

withoutLast l = if null (tail l) then [] else (head l):withoutLast (tail l)

withoutLast' [x] = []
withoutLast' (x:xs) = x:withoutLast' xs

{- #
Function that takes 2 parameters: an integer n and a list ; and returns a list that contains
the first n elements of the input list.
-}

take' n list = if n == 0 || (null list) then []
                 else (head list):take' (n-1) (tail list)

take'' _ [] = []
take'' 0 _ = []
take'' n (x:xs) = x:take'' (n-1) xs

pick n list = if n == 0 || (null list) then []
                   else if n == (length list) || (n > (length list)) then list
                   else pick n (init list)

pick' n list = if n == (length list) || (n > (length list)) then list
                   else pick' n (init list)

-- # The last n elements of the input list

nLast n list = if n == 0 || (null list) then []
                  else (nLast (n-1) (init list))++[last list]

--# Function that concatenates two list (sum)

--somme l s = if (null l) then s else if (null s) then l else 
sumList l s = if (null l) then s else if (null s) then l else sumList l ++ [head s] tail s 

--# Function that reverse the elements of a list:

reverse' list = if (null list) then [] else reverse (tail list) ++ [head list]

reverse'' [] = []
reverse'' (x:xs) = (reverse xs)++[x]