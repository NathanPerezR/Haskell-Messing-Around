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
    | density < air = "Wow! You're going for a ride in the sky!"  
    | density <= water = "Have fun swimming, but watch out for sharks!"  
    | otherwise   = "If it's sink or swim, you're going to sink."  
    where density = mass / volume  
          air = 1.2  
          water = 1000.0  

-- let bindings are like the where statements, but they are more local, so no going across guard clauses (or other langauge constructs that are not expressions).  This is because they are expressions NOT syntax elements


-- CASE EXPRESSION --
--head' :: [a] -> a  
--head' xs = case xs of [] -> error "No head for empty lists!"  
--                      (x:_) -> x  
-- the above is the same as the below
-- head' :: [a] -> a  
-- head' [] = error "No head for empty lists!"  
-- head' (x:_) = x  

-- case expression of pattern -> result  
--                    pattern -> result  
--                    pattern -> result  
--                    ...  
--

-- case of can be used anywhere!

describeList :: [a] -> String  
describeList xs = "The list is " ++ case xs of [] -> "empty."  
                                               [x] -> "a singleton list."  
                                               xs -> "a longer list."  
-- note that this is the same as 
describeList' :: [a] -> String  
describeList' xs = "The list is " ++ what xs  
    where what [] = "empty."  
          what [x] = "a singleton list."  
          what xs = "a longer list."  

-- implimation of maximum using 
maximum' :: (Ord a) => [a] -> a  -- elements must be able to be ordered
maximum' [] = error "maximum of empty list"  -- empty list has no max
maximum' [x] = x  -- a list with one element will have that one element be its max
maximum' (x:xs)   -- pattern match a list into first element 'x' and rest of the list 'xs'
    | x > maxTail = x  -- if the first element is greater then the max of the tail, then x is the max
    | otherwise = maxTail  -- always evals to TRUE
    where maxTail = maximum' xs  -- the max of the rest of the list

-- another way of writing the above function
maximumWithMax' :: (Ord a) => [a] -> a
maximumWithMax' [] = error "maximum of an empty list"
maximumWithMax' [x] = x
maximumWithMax' (x:xs) = max x (maximumWithMax' xs) -- find the max of the head of the list, and all subheads of the list 

-- a recreation of the replicate function
rep' :: (Num i, Ord i) => i -> a -> [a] -- rep has the following type: takes i as input which is an ord and a num, and some single value of type a, then returns a list full of that type of thing
rep' n x 
  | n <= 0 = []
  | otherwise = x:rep' (n-1) x


take' :: (Num i, Ord i) => i -> [a] -> [a] -- take has the following type: takes as input i, being of the typeclasses Num and Ord, and a list, and returns a list of type a 
take' n _                                  -- take input with a number less then 0 and with anything else, will be an empty list
  | n <= 0 = [] 
take' _ [] = []                            -- empty list with any number returns an empty list 
take' n (x:xs) = x:take' (n-1) xs          -- take some number, and the head of a list with the rest of the list.  return the head concat with takes head n times

-- reverse a list
rev' :: [a] -> [a] -- some list of things, then reverses that list of things:w
rev' [] = []       -- empty list case
rev' (x:xs) = rev' xs ++ [x] 

-- inf repete of a list, will never stop
repeat' :: a -> [a]
repeat' x = x:repeat' x

-- take' 3 (repeat' 4) is the same as replicate 3 4 (lazy eval!)

-- zip function implimented
zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):(zip' xs ys)

-- elem 
elem'::(Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs)
  | a == x = True
  | otherwise = a `elem'` xs 

-- quicksort! NOTE: How do we know that this is in place? -> it is not.
quicksort :: (Ord a) => [a] -> [a] -- takes some type that can be ordered in a list, and returns a list 
quicksort [] = []                  -- empty list is already sorted
quicksort (x:xs) = 
    let smallerSorted = quicksort [a | a <- xs, a <= x]  -- set from everything expect the head, only if <= then head 
        biggerSorted = quicksort [a | a <- xs, a > x]    -- set from everything expect the head, only if >
    in  smallerSorted ++ [x] ++ biggerSorted             -- smaller concat with head concat with the bigger elements

qs' :: (Ord a) => [a] -> [a]
qs' [] = []
qs' (x:xs) = qs' small ++ med ++ qs' large
  where small  = [y | y <- xs, y < x]
        med    = [y | y <- xs, y == x] ++ [x] -- if we don't concat x, then we will not include x in the found elements (as xs does not include x)
	large  = [y | y <- xs, y > x]

-- trying to understand currying 
add1 x y = x + y -- either this or add2 work
add2 x y = y + x -- either this or add1 work
addOne = add1 1  -- addOne 1, is really ((addOne) 1), which will be evaled to ((add1 1) 1)

-- note that all haskall functions only take one input.

divideByTen = (/10) -- infix function can be particallly applied with surrounding ()

isUpperAlpha :: Char -> Bool
isUpperAlpha = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a -- first parameter is a function that returns the same type of thing, and then
applyTwice f x = f (f x)
-- Try: applyTwice (++ "Hey") "Haha"
--      applyTwice ("Hey" ++) "Haha"
--applyTwice (3:) [1]  

-- zips with any function
zipwith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipwith' _ _ [] = []
zipwith' _ [] _ = []
zipwith' f (x:xs) (y:ys) = (f x y):(zipwith' f xs ys) -- make a list with  the elements from x list and y list, with function applied between them.

-- implimation of the flip function
flip' :: (a  -> b -> c) -> b -> a -> c -- NOTE: why do we not have the function notated with f
flip' f x y = f y x 

-- implimation of map
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

-- implimation of filter
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f xs = [x | x <- xs, f x == True]

-- could also be done as follows 
-- filter :: (a -> Bool) -> [a] -> [a]  
-- filter _ [] = []  
-- filter p (x:xs)  
--     | p x       = x : filter p xs  
--     | otherwise = filter p xs  

-- Lambdas --
-- : When to prefer over partical application via currying

--  
-- if pattern matching fails in lambda, error will occur

sumWithFoldLam :: (Num a) => [a] -> a
sumWithFoldLam xs = foldl (\acc x -> acc + x) 0 xs

-- again with foldl
-- foldl takes some function, some start value, and a list and then applies function to each element of the list, including the result of the first sum in other sums
-- (...((0 + 1) + 2) ... )
sumWithFoldl :: (Num a ) => [a] -> a
sumWithFoldl xs = foldl (+) 0 xs

-- elem with foldl
accWithFoldl :: (Eq a) => a -> [a] -> Bool
accWithFoldl y ys =  foldl (\acc x -> if x == y then True else acc) False ys

mapWithAcc :: (a -> b) -> [a] -> [b]
mapWithAcc f xs = foldr (\x acc -> f x : acc) [] xs  -- with left fold we would have had to use ++ instead of :, ++ is more expesnive then :

-- Folds are best when you traverse a list once, and return something based on that - very useful!

maximumWithFold' :: (Ord a) => [a] -> a  
maximumWithFold' = foldr1 (\x acc -> if x > acc then x else acc)
  
reverseWithFold' :: [a] -> [a]  
reverseWithFold' = foldl (\acc x -> x : acc) [] -- take starting value of empty list, prepend to the accumulator 
  
productWithFold' :: (Num a) => [a] -> a  
productWithFold' = foldr1 (*)  -- foldr1 is a right fold with starting val being the first value on the right
  
filterWithFold' :: (a -> Bool) -> [a] -> [a]  
filterWithFold' p = foldr (\x acc -> if p x then x : acc else acc) [] -- acc is a list containing the values if p x evals to true
  
headWithFold' :: [a] -> a  
headWithFold' = foldr1 (\x _ -> x)  
  
lastWithFold' :: [a] -> a  
lastWithFold' = foldl1 (\_ x -> x)  

-- scanl: like fold but shows the inbetween results
scanl (+) 0 [1,2..10] -- == [0,1,3,6,10,15,21,28,36,45,55]
foldl (+) 0 [1,2..10] -- == 55
