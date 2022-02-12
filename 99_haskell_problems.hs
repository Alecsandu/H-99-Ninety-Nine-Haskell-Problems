import Data.Char
import Data.List
import Data.Either
import Debug.Trace

-- Problem 1
-- Find the last element from a list.
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (h:t) = reverseList t ++ [h]

myLast :: [a] -> a
myLast [] = error "Can't find the last element of an empty list."
myLast list = head (reverseList list)
myLast2 :: [a] -> a
myLast2 [] = error "Can't find the last element of an empty list."
myLast2 list = last list


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
-- returns the first half of the list without the middle element
firstHalfOfList :: (Eq a) => [a] -> [a]
firstHalfOfList [] = []
firstHalfOfList list = take ((length list) `div` 2) list
-- returns the second half of the list without the middle element if the list has odd length
secondHalfOfList :: (Eq a) => [a] -> [a]
secondHalfOfList [] = []
secondHalfOfList list = 
    reverse (
        if (length list) `mod` 2 == 0
        then drop ((length list) `div` 2) list
        else drop 1 (drop ((length list) `div` 2) list)
    )
-- checks if a list is a palindrom by cheching if the first and reversed second halfs are equal
myPalindrome2 ::(Eq a) => [a] -> Bool
myPalindrome2 [] = True
myPalindrome2 [_] = True
myPalindrome2 list = (firstHalfOfList list) == (secondHalfOfList list)


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
compress2 [] = []
compress2 list = [fst x | x <- zip (init list) (tail list), (fst x /= snd x)] ++ [last list]


-- Problem 9
-- Pack consecutive duplicates of list elements into sublists.
-- If a list contains repeated elements they should be placed in separate sublists.
pack :: (Eq a) => [a] -> [[a]]
pack list = group list

pack2 :: (Eq a) => [a] -> [[a]]
pack2 [] = []
pack2 [x] = [[x]]
pack2 (h:t) = if h `elem` (head (pack2 t)) 
              then (h:(head (pack2 t))) : (tail (pack2 t))
              else [h] : (pack2 t)

-- We split elements of a list recursively into those which are equal to the first one,
-- and those that are not. Then do the same for the latter:
pack3 :: (Eq a) => [a] -> [[a]]
pack3 [] = []
pack3 (x:xs) = let (first, rest) = span (==x) xs
              in (x:first) : pack3 rest


-- Problem 10
-- Implement the so-called run-length encoding data compression method.
-- Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.
sumAparitions :: [a] -> Int
sumAparitions [] = 0
sumAparitions (h:t) = 1 + sumAparitions t

encode :: (Eq a) => [a] -> [(Int,a)]
encode [] = []
encode list = [((sumAparitions x),(head x)) | x <- (group list)]


-- Problem 11
-- Same as Prolem 10 but when there are no duplicates just put in the list the element without the number
checkIfElementHasDuplicates :: [a] -> Either (Int,a) a
checkIfElementHasDuplicates list = if ((sumAparitions list) == 1)
                                    then Right(head list) 
                                    else Left((sumAparitions list),(head list))

encodeWithoutNonDuplicatesNumbers :: (Eq a) => [a] -> [Either (Int,a) a]
encodeWithoutNonDuplicatesNumbers [] = []
encodeWithoutNonDuplicatesNumbers list = [ checkIfElementHasDuplicates x | x <- (group list)]


-- Used to avoid compile failure with ghc
main :: IO ()
main = return ()
