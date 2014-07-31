doubleMe :: Int->Int
doubleMe x=x+x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100 
						then x
						else x*2
						
triangels=[(a,b,c)|c <-[1..10],b <-[1..10],a <- [1..10]]
			
rightTriangels=[(a,b,c)|c <-[1..10],b <-[1..10],a <- [1..10],a^2+b^2==c^2,a+b+c==24]	--right triangels with sides 1..10, perimeter=24	

factorial :: Integer -> Integer
factorial n = product [1..n]
	
circumference :: Float -> Float
circumference r = 2*pi*r

addThree :: Int -> Int -> Int ->Int
addThree x y z = x + y + z  

lucky :: Int -> String  
lucky 7 = "LUCKY NUMBER SEVEN!"  
lucky x = "Sorry, you're out of luck, pal!"  
 
 
factorial' :: Integer -> Integer 
factorial' 0 = 1  
factorial' n = n * factorial (n - 1) --recurse 

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)  
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)  

head' :: [a] -> a  
head' [] = error "Empty list"  
head' (x:_) = x  

length' :: [a] -> Integer
length' [] = 0 
length' (_:xs) = 1 + length' xs --length is tail +1, recurse

sum' :: (Num a) => [a] -> a
sum' [] = 0 
sum' (x:xs) = x + sum' xs --sum is  head (first element) plus sum tail (remaining elements)

bmiTell :: (RealFloat a,Show a) => a -> a -> String
bmiTell weight height
    | bmi <= 18.5 = "You're underweight, you emo, you!" ++ show (bmi)
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!" ++ show (bmi)
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  ++show (bmi)
    | otherwise = "You're a whale, congratulations!" ++show (bmi)
    where bmi= weight / height ^ 2
	
maximum' :: (Ord a) => [a] -> a  
maximum' [] = error "maximum of empty list"  
maximum' [x] = x  
maximum' (x:xs)   
    | x > maxTail = x  
    | otherwise = maxTail  
    where maxTail = maximum' xs

maximum2' :: (Ord a) => [a] -> a  
maximum2' [] = error "maximum of empty list"  
maximum2' [x] = x  
maximum2' (x:xs) = max x (maximum' xs)


cylinderArea :: (RealFloat a ) => a -> a -> a
cylinderArea r h = 
    let sideArea = 2 * pi * r * h 
        topArea = pi * r^2
    in sideArea + 2 * topArea

describeList :: [a] -> String  
describeList xs = "The list is " ++ case xs of [] -> "empty."  
                                               [x] -> "a singleton list."   
                                               xs -> "a longer list." 


take' :: (Num i, Ord i) => i-> [a] -> [a]
take' n _
    | n <= 0 = [] 
take' _ [] = []
take' n (x:xs) = x:take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse (x:xs) = reverse' xs ++ [x]


repeat' x = repeat x:repeat' x

zip' :: [a] -> [b]  -> [(a,b)]
zip' _[] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool 
elem' a [] = False 
elem' a (x:xs)
    | a == x    = True
    | otherwise = a `elem'` xs

quicsort :: (Ord a) => [a] -> [a]
quicsort [] = []
quicsort (x:xs)=
    let smallerSorted = quicsort [a | a <- xs, a <= x]
        biggerSorted = quicsort [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted