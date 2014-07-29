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
 
 
factorial' :: Integer -> Integer  --recursively implemented
factorial' 0 = 1  
factorial' n = n * factorial (n - 1)  

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

bmiTell :: (RealFloat a) => a -> a -> String 
bmiTell weight height
    | bmi <= 18.5 = "You're underweight, you emo, you!"  ++ show bmi
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise                 = "You're a whale, congratulations!" 
    where bmi=weight / height ^ 2 