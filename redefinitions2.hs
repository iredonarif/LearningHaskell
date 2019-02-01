--I) Sort the elements of a list in ascending or descending order

--1- Ascending Order
--V1

-- Minimum function (theMin)
theMin :: [Int] -> Int 
theMin (x:[]) = x
theMin (x:xs) = if x < theMin xs then x else theMin xs

-- List without min function
listWithoutMin :: [Int] -> [Int]
listWithoutMin l = [ i | i <- l, i /= theMin l] 

--Function to sort a list in ascending order by using (theMin, listWithoutMin)
ascendingList :: [Int] -> [Int]
ascendingList [] = []
ascendingList l = (theMin l):ascendingList (listWithoutMin l)

-- V2
ascendingList' :: [Int]->[Int]
ascendingList' [] = []
ascendingList' (x:[]) = [x]
ascendingList' (x:xs) = (minimum (x:xs)):ascendingList' [ i | i <- (x:xs), i /= (minimum (x:xs))]

-- V3 : Using let expression

ascendingList'' :: [Int]->[Int]
ascendingList'' [] = []
ascendingList'' (x:[]) = [x]
ascendingList'' (x:xs) =  let min = minimum (x:xs) in min:ascendingList'' [ i | i <- (x:xs), i /= min]

-- V4 With quick sort algorithm
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = let lessOrEqual = [a | a <- xs, a <= x]; greater = [a | a <- xs, a > x] 
                   in quicksort lessOrEqual ++ [x] ++ quicksort greater

--2- Descending order

descendingOrder :: [Int]->[Int]
descendingOrder [] = []
descendingOrder (x:[]) = [x]
descendingOrder (x:xs) =  let max = maximum (x:xs) in max:descendingOrder [ i | i <- (x:xs), i /= max]

--II) Function returning the number of times a number is found in a list

nbTimes :: Int -> [Int] -> Int
nbTimes n [] = 0
nbTimes n (x:xs) = if n==x then 1 + nbTimes n xs else nbTimes n xs

--III) Function returning a list of integers without duplicates (set list)

setList :: [Int] -> [Int]
setList [] = []
setList (x:xs) = if x `elem` xs then setList xs else x:setList xs

--IV) Function returning the union of two lists of integers

union :: [Int] -> [Int] -> [Int]
union [] [] = []
union [] (x:xs) = (x:xs)
union (x:xs) [] = (x:xs)
union (x:xs) (x2:xs2) = setList ((x:xs)++(x2:xs2))

-- Function to remove an element from a list

remove e l = [ i | i <- l, i /= e]

--V) Function returning the intersection of two set lists 

intersection :: [Int] -> [Int] -> [Int]
intersection [] [] = []
intersection [] (x:xs) = []
intersection (x:xs) [] = []
intersection (x:xs) (x':xs') = if x `elem` (x':xs') then x:intersection xs (remove x (x':xs')) else intersection xs (remove x (x':xs'))

{- 
intersection :: [Int] -> [Int] -> [Int]
intersection [] [] = []
intersection [] (x:xs) = []
intersection (x:xs) [] = []
intersection (x:xs) (x':xs') = if x `elem` (x':xs') then x:intersection xs (x':xs') else intersection xs (x':xs')
-}

--VI) And logic && Or logic of booleans list
--1
andLogic :: [Bool] -> Bool
andLogic [] = error "Empty list"
andLogic (x:[]) = x
andLogic (x:xs) = x && andLogic xs 

--2
orLogic :: [Bool] -> Bool
orLogic [] = error "Empty list"
orLogic (x:[]) = x
orLogic (x:xs) = x || orLogic xs 
