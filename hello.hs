main :: IO ()
main = putStrLn "Hello, World!"

doubleMe x = x + x

doubleSmallNum x = if x > 100
                     then x
                     else x*2    -- Else is always required

name = "Imagine this is a name!" -- this is a function without params, called a "name" or "defination"

-- LISTS -- 

-- Lists are homogeneous data structs, store several elements of the same type
-- Strings are lists
-- Two lists can be joined by ++
-- But cons can also be used :
  -- useage: 5:[1,2,3] will eval to [5,1,2,3]
-- [1,2,3] is sugar for 1:2:3:[]
-- to access an element of a list at a certain index use !!, indicies start at 0
-- When lists are compared they are compared element by elelement in lexogrpahical order
-- head returns the first element of a list
-- tail returns the everything but the head
-- init takes a list and returns everything expect last element 
-- head tail and init can not be used on empty lists
-- null checks if a list is empty
-- reverse, does what youd expect
-- take extracts that many elements from a list
-- drop drops that many elements from a list
-- maximum returns the biggest ele ent 
-- minimum for the smallest
-- elem tells us if something is an element of the list, written as infix normally
-- [1..20] will return the numbers 1 through 20
-- [2,4..20] will return [2 4 6 8 9 ... 20]
-- floats in ranges can be weird
-- [20, 19 .. 1] will return all the numbers from 20 to 1
-- take 24 [13, 26 ..] will take the first 24 elements of the inf list that is mulitples of 13
-- cycle takes a list and cycles it into an infinit list
-- repeat takes an element and produces an inf list of just that element 
-- replicate exists
-- [x*2 | x <- [1..10]] is a list comprenision it will return 1 * 2, 2*2, 3*2... 10*2
-- [x*2 | x <- [1..10], x*2 >= 12] gives us the x*2 where x>=12

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG" | x <- xs, odd x] -- after the comma is a condition (called a predicate), several predicates are possible 

allNumsNot13or15or19 xs = [x | x <- xs, x /= 13, x /=15, x /= 19]

allProductsOfTwoLists xs ys = [x * y | x <- xs, y <- ys]

len' xs = sum [1 | _ <- xs] -- replaces the list with a 1, before summing up the list

removeNonUpperCase :: [Char] -> [Char] -- Type of the fucntion below
removeNonUpperCase st = [c | c <- st, c `elem` ['A'..'Z']] -- possible because strings are lists

removeEvenFromNested xss = [ [ x | x <- xs, even x] | xs <- xss]  -- takes as input lists of list of numbers, and removes the even numbers (without flattening the list)

-- tuples can contain lists
-- fst, returns first element of a pair
-- snd takes second element of a pair

-- zip takes two lists and then zips then together into one list.  If the lists do not match in size then they will get truncated.  We can zip inf lists with finiate lists


triangle = [(a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10] ] -- creates a list of all trianles with sides equal to or smaller then 10
rightTrinagle = [ (a,b,c)| c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2 ] --TODO: unsure what the [1..c] syntax does

-- :t will print out the type of something when using ghci, we can give functions type defs as so


sumThreeNumbers:: Int -> Int -> Int -> Int -- the last thing is the return type, the first three are inputs to the function.  Functions are expressions so :t works on them to see what the type is. (even without explicit declaration)
sumThreeNumbers a b c = a + b + c

-- Int is bounded, 32 bit.  Integer is not bounded 

-- :t fst will return fst::(a,b)->a.  This means it works with any type


-- Type class -- 
-- Interface that defines some behavor
-- t: (==) will return the following: (==) :: Eq a => a -> a -> a -> Bool
-- Eq is a class constraint, everything before the => is the class constaint 
-- all standards types are part of Eq, EXPECT FOR IO
-- Ord is for types that have ordering, can be GT, LT, EQ (greater then, less then, equal too)
-- Show means can be represted as strings
-- Read is the oppistate of show, read takes a string and returns a type which is a memeber of read.  read requries a type to be known or infered.
  -- read "5" will error, read "5" :: Float will not
-- Enum are sequently ordered types, that means we can use list ranges!
-- Bounded means they have a minBound and a maxBound
-- Num is numeric type class
-- Int and Interger are memebers of Intergral


-- The following will take an int or an interger, and then compute the factorial
factorial::(Integral a) => a -> a 
factorial 0 = 1 -- this pattern match must come above the below one, otherwise the below one would catch everything 
factorial x = x * (factorial (x - 1))

first::(a,b,c) -> a
first (x,_,_) = x
second::(a,b,c) -> b 
second (_,y,_) = y
third::(a,b,c) -> c
third (_,_,z) = z

sumPairsInList:: Num a => [(a,a)] -> [a] -- TODO: Why?
sumPairsInList xs = [a+b | (a,b) <- xs]

-- x:y:z:xs will only match lists with 3 or more elements
head' (x:_) = x -- binding of variables means you need ()

-- From the book: This function is safe because it takes care of the empty list, a singleton list, a list with two elements and a list with more than two elements. Note that (x:[]) and (x:y:[]) could be rewritten as [x] and [x,y] (because its syntactic sugar, we don’t need the parentheses). We can’t rewrite (x:y:_) with square brackets because it matches any list of length 2 or more.

sum' [] = 0 -- sum of an empty list is 0 
sum' (x:xs) =  x + sum' xs -- sum of list is the first element, plus the sum of the rest of the list

capital :: String -> String -- takes as input a string, and returns a string
capital "" = "Empty string, whoops!" -- if there is no string
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x] -- all is the full reference before we chop it up 

-- can't use ++ in pattern matches


----------------Guards--------------------

max' a b           --TODO: how does haskell know :t that this is a Ord
  | a > b = a
  | otherwise = b

a `myCompare` b
  | a < b = LT
  | a > b = GT
  | otherwise = EQ

densityTell :: (RealFloat a) => a -> a -> String  
densityTell mass volume  
    | density < 1.2 = "Wow! You're going for a ride in the sky!"  
    | density <= 1000.0 = "Have fun swimming, but watch out for sharks!"  
    | otherwise   = "If it's sink or swim, you're going to sink."  
    where density = mass / volume  -- defines density in the above guard clause to be mass / volumne.  We could define multiple things

densityTell :: (RealFloat a) => a -> a -> String  
densityTell mass volume  
    | density < air = "Wow! You're going for a ride in the sky!"  
    | density <= water = "Have fun swimming, but watch out for sharks!"  
    | otherwise   = "If it's sink or swim, you're going to sink."  
    where density = mass / volume  
          air = 1.2  
          water = 1000.0  
