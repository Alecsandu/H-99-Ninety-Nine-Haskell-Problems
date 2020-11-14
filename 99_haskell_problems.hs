import Data.Char
import Data.List
import Debug.Trace

-- Problem 1
-- Find the last element from a list.

reverseList :: [a] -> [a]
reverseList [] = []
reverseList (h:t) = reverseList t ++ [h]

myLast :: [a] -> a
myLast [] = error "Can't find the last element of an empty list."
myLast list = head (reverseList list)
--myLast list = last list


-- Problem 2
-- Find the last but one element of a list.

myButLast :: [a] -> a
myButLast [] = error "Can't find the last but one element of an empty list."
myButLast list
    | length list == 1 = error "Can't find the last but one element of a list with only one element."
    | otherwise = last (take ((length list) - 1) list)


-- Problem 3
-- Find the K'th element from a list.

elementAt :: [a] -> Int -> a
elementAt [] _ = error "Can't return K'th element of an empty list."
elementAt list 1 = head list
elementAt list nr
    | nr > (length list) || nr <= (-1) = error "The number K is incorrect."
    | otherwise = elementAt (tail list) (nr-1)


-- Problem 4
-- Find the number of elements of a list.

myLength :: [a] -> Int
myLength [] = 0
myLength (h:t) = 1 + myLength t


-- Problem 5
-- Reverse a list

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (h:t) = myReverse t ++ [h]
myReverse2 :: [a] -> [a]
myReverse2 list = foldl (flip (:)) [] list


-- Problem 6
-- Find out whether a list is a palindrome.

myPalindrome :: (Eq a) => [a] -> Bool
myPalindrome [] = True
myPalindrome [_] = True
myPalindrome list = list == (myReverse2 list)


-- Problem 7
-- Flatten a nested list structure.
-- Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).

data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem e) = [e]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)


-- Problema 8
-- Eliminate consecutive duplicates of list elements.
-- If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.

compress :: (Eq a) => [a] -> [a]
compress [] = []
compress list = map head (group list)

compress2 :: (Eq a) => [a] -> [a]
compress2 list = [fst x| x <- zip (init list) (tail list), (fst x /= snd x)] ++ [last list]


-- Problem 9
-- Pack consecutive duplicates of list elements into sublists.
-- If a list contains repeated elements they should be placed in separate sublists.
pack :: (Eq a) => [a] -> [[a]]
pack list = group list

pack2 :: (Eq a) => [a] -> [[a]]
pack2 [] = []
pack2 [x] = [[x]]
pack2 (h:t) = if h `elem` (head (pack2 t)) 
              then (h:(head  (pack2 t))) : (tail (pack2 t))
              else [h] : (pack2 t)

pack3 :: (Eq a) => [a] -> [[a]]
pack3 [] = []
-- We split elements of a list recursively into those which are equal to the first one,
-- and those that are not. Then do the same for the latter:
pack3 (x:xs) = let (first, rest) = span (==x) xs
              in (x:first) : pack3 rest


-- Problem 10
sumAparitions :: [a] -> Int
sumAparitions [] = 0
sumAparitions (h:t) = 1 + sumAparitions t

encode :: (Eq a) => [a] -> [(Int,a)]
encode [] = []
encode list = [((sumAparitions x),(head x)) | x <- (pack list)]


